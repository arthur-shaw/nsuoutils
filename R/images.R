#' Créer un inventaire des produits-unités avec images
#' 
#' Puiser dans les bases NSU et identifier les produits-unités distincts et avec images.
#' 
#' Les produits-unités avec images sont définis comme ceux avec un texte non-vide au niveau des questions q109, q114_autre1, ou q119_autre2
#' 
#' @param dir Character vector. Chemin du répertoire où se trouvent les bases de données resorties de la fusion des bases.
#' @param unitesFixes Character vector. Nom du fichier des unités fixes.
#' @param unitesautre1 Character vector. Nom du fichier des unités connues mais associées avec d'autres produits.
#' @param unitesautre2 Character vector. Nom du fichier des unités inconnues d'avance.
#' 
#' @return Data frame des produits-unités distincts avec images
#' 
#' @importFrom haven read_dta
#' @importFrom dplyr `%>%` rename filter distinct mutate bind_rows
#' @importFrom labelled add_value_labels
#' @importFrom rlang .data
#' 
#' @export
inventory_product_units <- function(
    dir,
    unitesFixes = "unitesFixes.dta",
    unitesautre1 = "unitesautre1.dta",
    unitesautre2 = "unitesautre2.dta"
) {

    unites_fixes <- haven::read_dta(file = paste0(dir, unitesFixes)) %>%
        dplyr::rename(unite = .data$unitesFixes__id) %>%
        dplyr::filter(!.data$q109 %in% c("", "##N/A##")) %>%
        dplyr::distinct(.data$produit_nom, .data$unite)

    unites_autre_1 <- haven::read_dta(file = paste0(dir, unitesautre1)) %>%
        dplyr::rename(unite = .data$unitesautre1__id) %>%
        dplyr::filter(!.data$q114_autre1 %in% c("", "##N/A##")) %>%
        dplyr::distinct(.data$produit_nom, .data$unite)

    unites_autre_2 <- haven::read_dta(file = paste0(dir, unitesautre2)) %>%
        dplyr::rename(unite_txt = .data$q116) %>%
        dplyr::filter(!.data$q119_autre2 %in% c("", "##N/A##")) %>%
        dplyr::distinct(.data$produit_nom, .data$unite_txt) %>%
        dplyr::mutate(unite = 0) %>%
        labelled::add_value_labels(unite = c(Autre2 = 0))

    product_units <- dplyr::bind_rows(unites_fixes, unites_autre_1, unites_autre_2) %>%
        dplyr::distinct(.data$produit_nom, .data$unite)

    return(product_units)

}

#' Créer les répertoires pour les produits avec images
#' 
#' Créer un répertoire pour chaque produit avec image.
#' 
#' If the folder does not exist, create it. If the folder already exists, move to the next folder.
#' 
#' @param inventory_df Data frame. Contains an inventory of the unique product-units with images.
#' @param dir Character. Directory where product directories should be created.
#' 
#' @importFrom dplyr distinct `%>%` pull
#' @importFrom fs dir_create
#' @importFrom rlang .data
create_product_dirs <- function(
    inventory_df,
    dir
) {

    products <- dplyr::distinct(inventory_df, .data$produit_nom) %>%
        dplyr::pull(.data$produit_nom)

    product_dirs <- paste0(dir, products, "/")

    fs::dir_create(product_dirs)

}

#' Create unit directories
#' 
#' Create one directory for each product-unit with an image.
#' 
#' If the folder does not exist, create it. If the folder already exists, move to the next folder.
#' 
#' @param inventory_df Data frame. Contains an inventory of the unique product-units with images.
#' @param dir Character. Directory where unit directories should be created.
#' 
#' @importFrom dplyr `%>%` distinct mutate pull
#' @importFrom haven as_factor
#' @importFrom fs dir_create
#' @importFrom rlang .data
create_unit_dirs <- function(
    inventory_df,
    dir
) {

    unit_dirs <- inventory_df %>%
        dplyr::distinct(.data$produit_nom, .data$unite) %>%
        dplyr::mutate(unite_txt = haven::as_factor(.data$unite, levels = "labels")) %>%
        dplyr::mutate(dirs = paste0(dir, "/", .data$produit_nom, "/", .data$unite_txt, "/")) %>%
        dplyr::pull(.data$dirs)

    fs::dir_create(unit_dirs)

}

#' Create dirs for strata
#' 
#' Create directories for each strata within each product-unit directory
#' 
#' Strata are the user-defined set of variables specified in `strata_vars`.
#' The directory names will be hyphen-separated combinations of the variables that compose a strata (e.g., `"{region} - {urban_rural}"`)
#'  
#' @param inventory_df Data frame. Contains an inventory of the unique product-units with images.
#' @param market_path Character. File path to the combined market file.
#' @param strata_vars Character vector. Names of the variables that together form strata.
#' @param dir Character. Directory where unit directories should be created.
#' 
#' @importFrom rlang syms .data
#' @importFrom dplyr distinct `%>%` mutate across pull
#' @importFrom tidyr expand_grid unite
#' @importFrom haven as_factor
#' @importFrom labelled to_character
#' @importFrom fs dir_create
create_strata_dirs <- function(
    inventory_df,
    market_path,
    strata_vars,
    dir
) {

    strata_vars <- rlang::syms(strata_vars)

    # determine unique values of stratum variable(s)
    marches <- haven::read_dta(file = market_path)
    strata <- dplyr::distinct(marches, !!!strata_vars)

    # expand product-unit data set to have one obs per strata
    produits_unites_strata <- tidyr::expand_grid(inventory_df, strata)

    strata_dirs <- produits_unites_strata %>%
        dplyr::mutate(
            unite_txt = haven::as_factor(.data$unite, levels = "labels"),
            dplyr::across(
                .cols = -c(.data$produit_nom, .data$unite, .data$unite_txt),
                .fns = ~ labelled::to_character(.x)
            )
        ) %>%
        tidyr::unite(
            col = "strata_txt", 
            -c(.data$produit_nom, .data$unite, .data$unite_txt), 
            sep = " - ", 
            remove = FALSE
        ) %>%
        dplyr::mutate(dirs = paste0(dir, "/", .data$produit_nom, "/", .data$unite_txt, "/", .data$strata_txt)) %>%
        dplyr::pull(.data$dirs)

    fs::dir_create(strata_dirs)

}

#' Créer un répertoire pour chaque produit-unité
#' 
#' @param inventory_df Data frame. Base créée par `inventory_product_units()` qui fait l'inventaire complet des produits-unités des bases de l'enquête NSU.
#' @param market_path Character. File path to the combined market file.
#' @param strata_vars Character vector. Names of the variables that together form strata.
#' @param dir Character. Répertoire racine où les répertoires produit-unité doivent être créés.
#' 
#' @export 
create_image_dirs <- function(
    inventory_df,
    market_path,
    strata_vars,
    dir
) {

    # create folders for each product in the product-unit inventory
    create_product_dirs(
        inventory_df = inventory_df,
        dir = dir
    )

    # create folders for each product unit in the product-unit inventory
    create_unit_dirs(
        inventory_df = inventory_df,
        dir = dir
    )

    # create strata directories within each product-unit directory in the inventory
    create_strata_dirs(
        inventory_df = inventory_df,
        market_path = market_path,
        strata_vars = strata_vars,
        dir = dir
    )

}

#' Sort images for an interview
#' 
#' This workhorse function for `sort_images()` works within an interview folder of the exported images and copies images to their destination directory based on the product-unit found in the file name.
#' 
#' First, it determines where files should go by:
#' 
#' - Creating a list of files in the folder
#' - Parsing the file name into product name and unit code components
#' - Constructs a file name that includes the file's `interview__key`
#' - Constructing a destination file path, composed of product, unit, and strata
#' 
#' Then, it moves the file to its destination directory by:
#' 
#' - Checking whether the directory exists
#' - If not, issuing a warning
#' - If so, copying the file
#' 
#' @param dir_in Character. Folder for a single interview whose images to sort
#' @param interview_id_df Data frame. Created by `get_interview_ids()`. 
#' Consists of `interview__id` and variables that begin with `"s01q01"` and `numeroReleve`.
#' @param unit_labels Named numeric vector.
#' @param dir_out Character. Root folder of destination folder for sorted images.
#' @param strata_vars Character vector. Names of the variables that together form strata.
#' 
#' @importFrom rlang syms .data
#' @importFrom fs dir_ls path_ext_remove path_file path_dir file_copy
#' @importFrom dplyr `%>%` case_when rename filter pull
#' @importFrom stringr str_extract str_starts
#' @importFrom labelled set_value_labels to_character
#' @importFrom tidyr unite
#' @importFrom glue glue glue_collapse
sort_images_obs <- function(
    dir_in,
    interview_id_df,
    unit_labels,
    dir_out,
    strata_vars
) {

    # get list of all the files in the folder
    files <- fs::dir_ls(dir_in, type = "file")

    strata_vars_syms <- rlang::syms(strata_vars)

    # construct new file names and addresses
    # new names are needed so that same-named files are not overwritten (e.g., two q109_avocat___147.jpg from different observations)
    # new folders are needed to move the files to the right place based on their name
    file_map <- data.frame(files, stringsAsFactors = FALSE) %>%
        dplyr::mutate(
            image_name_trunc = fs::path_ext_remove(fs::path_file(.data$files)),
            #  stringr::str_replace_all(image_name, pattern = "^q109_|^q11[49]_|\\.jpg", replacement = "")
            question = stringr::str_extract(.data$image_name_trunc, pattern = "^q1[01][49]"),
            product_name = dplyr::case_when(
                stringr::str_starts(.data$image_name_trunc, "q109") ~ stringr::str_extract(.data$image_name_trunc, pattern = "(?<=q1[01][49]_).+(?=___)"),
                TRUE ~ stringr::str_extract(.data$image_name_trunc, pattern = "(?<=q11[49]_).+(?=_autre[12]__)")
            ),
            unit_code = dplyr::case_when(
                .data$question %in% c("q109", "q114") ~ stringr::str_extract(.data$image_name_trunc, pattern = "(?<=___).+"),
                .data$question == "q119" ~ "0"
            ),
            unit_code = as.numeric(.data$unit_code),
            interview__key = stringr::str_extract(.data$files, pattern = "(?<=)[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}"),
            image_name_new = glue::glue("{.data$question}_{.data$product_name}___{.data$unit_code}__{.data$interview__key}.jpg")
        ) %>%
        labelled::set_value_labels(unit_code = unit_labels) %>%
        dplyr::left_join(interview_id_df, by = c("interview__key", "question")) %>%   
        dplyr::mutate(
            unit_txt = haven::as_factor(.data$unit_code, levels = "labels"),
            across(
                c(!!!strata_vars_syms),
                .fns = ~ labelled::to_character(.x)
            )
        ) %>%
        tidyr::unite(
            col = "strata_txt", 
            !!!strata_vars_syms, 
            sep = " - ", 
            remove = FALSE
        ) %>%
        dplyr::mutate(
            path_new = dplyr::case_when(
                # TODO: replace `image_out_dir` below with param name
                .data$question %in% c("q109", "q114") ~ glue::glue("{dir_out}{.data$product_name}/{.data$unit_txt}/{.data$strata_txt}/{.data$image_name_new}"),
                .data$question == "q119" ~ glue::glue("{dir_out}{.data$product_name}/{.data$unit_txt}/{.data$strata_txt}/{.data$image_name_new}")
            ),
            dir_old = fs::path_dir(.data$files),
            file_old = fs::path_file(.data$files),
            dir_new = fs::path_dir(.data$path_new),
            file_new = fs::path_file(.data$path_new),
            dir_new_exists = dir.exists(.data$dir_new)
        ) %>%
        dplyr::rename(path_old = files)

    # split file map into cases whose destination directories do and do not exist--ok and bad, respectively
    file_map_ok <- dplyr::filter(file_map, .data$dir_new_exists == TRUE)
    file_map_bad <- dplyr::filter(file_map, .data$dir_new_exists != TRUE)

    # copy files from old path to the new path
    if (nrow(file_map_ok) >0) {
        fs::file_copy(path = file_map_ok$path_old, new_path = file_map_ok$path_new, overwrite = TRUE)
    }

    # issue warning if file does not have a destination folder
    if (nrow(file_map_bad) > 0) {
        source_dir <- dplyr::pull(file_map_bad, .data$dir_old) %>% unique()
        source_files <- dplyr::pull(file_map_bad, .data$file_old) %>% glue::glue_collapse(, sep = ", ")
        warning_msg <- glue::glue(
            "WARNING: Some files cannot be copied because their destination directories do not exist.",
            "Source directory: {source_dir}",
            "Files : {source_files}",
            .sep = "\n"
        )
        warning(warning_msg)
    } 

}

#' Sort images for a product group
#' 
#' Applies `sort_images_obs()` over a list of product group folders
#' 
#' @param dir_in Character. Folder for a single product group whose images to sort
#' @param interview_id_df Data frame. Created by `get_interview_ids()`. 
#' @param unit_labels Named numeric vector.
#' @param strata_vars Character vector. Names of the variables that together form strata.
#' @param dir_out Character. Root folder of destination folder for sorted images.
#' 
#' @importFrom fs dir_ls
#' @importFrom purrr walk
sort_images_product <- function(
    dir_in,
    interview_id_df,
    unit_labels,    
    dir_out,
    strata_vars
) {

    # get list of folders whose files to move
    folders <- fs::dir_ls(dir_in, type = "directory")

    # apply image sorting function to each folder
    purrr::walk(
        .x = folders,
        .f = ~ sort_images_obs(
            dir_in = .x,
            interview_id_df,
            unit_labels = unit_labels,
            dir_out = dir_out,
            strata_vars = strata_vars
        )
    )

}

#' Classer les images selon le produit-unité
#' 
#' Met une copie de chaque image dans le bon répertoire selon le produit-unité décelé dans le nom de l'image.
#' 
#' La fonction marche comme suit:
#' 
#' - Parcourir chaque répertoire d'image
#' - Traiter chaque image rencontrée
#' - Analyser le nom de l'image afin de comprendre le produit, l'unité, et la taille concernés
#' - Construire un de l'image pour intégrer la valeur de `interview__key`
#' - Copier chaque image depuis son répertoire source vers son répertoire destinataire (selon son produit-unité-taille)
#' 
#' @param inventory_df Data frame. Base créée par `inventory_product_units()` qui fait l'inventaire complet des produits-unités des bases de l'enquête NSU.
#' @param data_dir Character. Répertoire où trouver les bases fusionnées par `combine_nsu_data()`.
#' @param data_files Character vector. Nom des bases d'où obtenir les identfiants des observations liées aux images.
#' @param dir_in Character. Répertoire racine dans lequel se trouve tous les répertoire d'images exportées de tous les produits.
#' @param image_dir_pattern Character. Expression régulière qui identifie les répertoires d'images exportés dans le répertoire racine `dir_in`.
#' @param dir_out Character. Répertoire racine dans lequel se trouve les répertoires destinataires créés par `create_image_dirs()`.
#' @param strata_vars Character vector. Nom des variables qui, ensemblent, identifient les strates.
#' 
#' @importFrom fs dir_ls
#' @importFrom labelled val_labels
#' @importFrom purrr map walk
#' @importFrom dplyr `%>%` bind_rows distinct
#' 
#' @export
sort_images <- function(
    inventory_df,
    data_dir,
    data_files = c("unitesFixes.dta", "unitesautre1.dta", "unitesautre1.dta"),
    dir_in,
    image_dir_pattern = "_Binary_",
    dir_out,
    strata_vars    
) {

    # get list of product folders whose files to move
    folders <- fs::dir_ls(dir_in, type = "directory", regexp = image_dir_pattern)

    # extract value labels for units in order to contruct a path that uses the label
    unit_labels <- labelled::val_labels(inventory_df$unite)

    # get interview IDs for constructing paths to strata folders
    interview_id_df <- data_files %>%
        purrr::map(.f = ~ get_interview_ids(path = paste0(data_dir, .x))) %>%
        dplyr::bind_rows() %>%
        dplyr::distinct(.data$interview__key, .data$question, .keep_all = TRUE)

    # apply image sorting function to each product folder
    purrr::walk(
        .x = folders,
        .f = ~ sort_images_product(
            dir_in = .x,
            interview_id_df = interview_id_df,
            unit_labels = unit_labels,
            dir_out = dir_out,
            strata_vars = strata_vars
        )
    )

}

#' Get interview IDs
#' 
#' Get interview ID variables for all files with images.
#' This data will the image sorting functions to construct a path to the strata folders.
#' 
#' @param path Character. Full path to combined data files of interest 
#' (i.e., unitesFixes.dta, unitesautre1.dta, and unitesautre2.dta)
#' 
#' @importFrom haven read_dta
#' @importFrom dplyr `%>%` mutate case_when select matches
get_interview_ids <- function(path) {

    df <- haven::read_dta(file = path)

    col_names <- names(df)

    df_keys <- df %>%
        dplyr::mutate(
            question = dplyr::case_when(
                "q109" %in% col_names ~ "q109",
                "q114_autre1" %in% col_names ~ "q114_autre1",
                "q119_autre2" %in% col_names ~ "q119_autre2"
            )
        ) %>% 
        dplyr::select(
            .data$interview__key, .data$question,
            dplyr::matches("^s00q0[0-8]")    
        )

    return(df_keys)

}
