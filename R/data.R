#' Get vector of file names matching a pattern
#' 
#' Returns a character vector of file names that match a regex pattern in the target directory
#' 
#' @param dir Character vector. File path to directory
#' @param pattern Character vector. Regular expression describing files to return
#' 
#' @return Character vector. File names with `.dta` extension
#' 
#' @importFrom fs dir_ls path_file
#' @importFrom stringr str_subset
#' 
#' @export
get_matching_files <- function(
    dir, 
    pattern
) {

    pattern_mod <- dplyr::if_else(
        pattern == "releve",
        "^releve_",
        pattern
    )

    file_names <- fs::dir_ls(
            path = dir,
            recurse = FALSE,
            regexp = "\\.dta"
        ) %>%
        fs::path_file() %>%
        stringr::str_subset(pattern = pattern_mod)
  
    return(file_names)

}

#' Read Stata file into memory
#' 
#' First, ingest data file. 
#' Then, write it to an object in the global environment, where the object name is the file name minus the `.dta` extension.
#' These names follow the same pattern as the input files in order to facilitate merging.
#' 
#' @param dir Character vector. File path to directory
#' @param file_name Character vector. File name with `.dta` extension
#' 
#' @return Data frame
#' 
#' @importFrom stringr str_replace
#' @importFrom haven read_dta
#' 
#' @export 
ingest_dta_file <- function(
    dir,
    file_name
) {

    df_name <- stringr::str_replace(
            string = file_name,
            pattern = "\\.dta",
            replacement = ""
        )

    haven::read_dta(file = paste0(dir, file_name))

}

#' Check whether expected columns are present
#' 
#' @param df Data frame. Data frame to check.
#' @param df_name Character. Name of data frame as a character.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' 
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom assertthat assert_that
#' @importFrom glue glue glue_collapse
#' 
#' @export 
check_cols <- function(
    df,
    df_name,
    df_type
) {

# df_name <- deparse(substitute(df))
cols_found <- names(df)

    # unitesFixes
    if (df_type == "unitesFixes") {

        cols_expected <- c(
            "interview__id", "interview__key", "unitesFixes__id", "produit_nom",
            "q104", "q105", "q105autre", "q108", "q109a", "q109"
        )

        cols_missing <- cols_expected[!cols_expected %in% cols_found]
        cols_missing_ms <- dplyr::case_when(
            !any(stringr::str_detect(cols_found, "q106__[0-9]+")) ~ "q106__*",
            !any(stringr::str_detect(cols_found, "q107__[0-9]+")) ~ "q107__*",
            !any(stringr::str_detect(cols_found, "q106__[0-9]+")) & !any(stringr::str_detect(cols_found, "q107__[0-9]+")) ~ c("q106__*", "q107__*"),
            TRUE ~ NA_character_
        )
        cols_missing <- c(cols_missing, cols_missing_ms)
        cols_missing <- cols_missing[!is.na(cols_missing)]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

    # releve
    if (df_type == "releve") {

        cols_expected <- c(
            "interview__id", "interview__key", "unitesFixes__id", "releve__id", "produit_nom", 
            "q107autre", "q110b", "q110a"
        )

        cols_missing <- cols_expected[!cols_expected %in% cols_found]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

    # unitesautre1
    if (df_type == "unitesautre1") {

        cols_expected <- c(
            "interview__id", "interview__key", "unitesautre1__id", "produit_nom",
            "q113_autre1", "q114a_autre1", "q114_autre1"
            # sometimes q114a_autre1
        )

        cols_missing <- cols_expected[!cols_expected %in% cols_found]
        cols_missing_ms <- dplyr::case_when(
            !any(stringr::str_detect(cols_found, "q112_autre1__[0-9]+")) ~ "q112_autre1__*",
            TRUE ~ NA_character_
        )
        cols_missing <- c(cols_missing, cols_missing_ms)
        cols_missing <- cols_missing[!is.na(cols_missing)]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

    # autre1releve_
    if (df_type == "autre1releve") {

        cols_expected <- c(
            "interview__id", "interview__key", "unitesautre1__id", "autre1releve__id", "produit_nom",
            "q112autre_autre1", "q115b_autre1", "q115a_autre1"
        )

        cols_missing <- cols_expected[!cols_expected %in% cols_found]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

    # unitesautre2
    if (df_type == "unitesautre2") {

        cols_expected <- c(
            "interview__id", "interview__key", "unitesautre2__id", "produit_nom",
            "q116", "q118_autre2", "q119_autre2"
        )

        cols_missing <- cols_expected[!cols_expected %in% cols_found]
        cols_missing_ms <- dplyr::case_when(
            !any(stringr::str_detect(cols_found, "q117_autre2__[0-9]+")) ~ "q117_autre2__*",
            TRUE ~ NA_character_
        )
        cols_missing <- c(cols_missing, cols_missing_ms)
        cols_missing <- cols_missing[!is.na(cols_missing)]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

    # autre2releve_
    if (df_type == "autre2releve") {

        cols_expected <- c(
            "interview__id", "interview__key", 
            "autre2releve__id", "unitesautre2__id", "produit_nom",
            "q117autre_autre2", "q120b_autre2", "q120a_autre2"
        )        

        cols_missing <- cols_expected[!cols_expected %in% cols_found]

        assertthat::assert_that(
            length(cols_missing) == 0,
            msg = glue::glue(
                "Expected column(s) missing in {df_name}",
                "Columns missing: {glue::glue_collapse(cols_missing, sep = ', ')}",
                .sep = "\n"
            )
        )

    }

}

#' Change column to expected type
#' 
#' First, check whether the column is of the expected type
#' Then, if not, change the column's type
#' 
#' @param df Data frame whose columns types to check and correct.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' 
#' @return Data frame with column types corrected where applicable.
#' 
#' @importFrom dplyr `%>%` mutate across starts_with
#' @importFrom rlang .data
#' 
#' @export 
correct_col_type <- function(
    df, 
    df_type
) {

    df_fixed <- df %>%
        {
            # unitesFixes
            if (df_type == "unitesFixes") {

                dplyr::mutate(.,
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom, .data$q105autre, .data$q109
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$unitesFixes__id, .data$q104, .data$q105, 
                            .data$q108, .data$q109a, 
                            starts_with("q106__"), starts_with("q107__")
                        ),
                        .fns = as.double
                    )
                )

            # releve
            } else if (df_type == "releve") {

                dplyr::mutate(.,
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom, .data$q107autre
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$unitesFixes__id, .data$releve__id, 
                            .data$q110b, .data$q110a
                        ),
                        .fns = as.double
                    )
                )

            # unitesautre1
            } else if (df_type == "unitesautre1") {

                dplyr::mutate(., 
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom, .data$q114_autre1
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$unitesautre1__id, .data$q113_autre1, 
                            starts_with("q112_autre1__")
                        ),
                        .fns = as.double
                    )
                )

            # autre1releve
            } else if (df_type == "autre1releve") {

                dplyr::mutate(., 
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$unitesautre1__id, .data$autre1releve__id, 
                            .data$q112autre_autre1, .data$q115b_autre1, 
                            .data$q115a_autre1
                        ),
                        .fns = as.double
                    )
                )

            # unitesautre2
            } else if (df_type == "unitesautre2") {

                dplyr::mutate(., 
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom, .data$q116, .data$q119_autre2
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$unitesautre2__id, 
                            starts_with("q117_autre2_"), 
                            .data$q118_autre2
                        ),
                        .fns = as.double
                    )
                )

            # autre2releve
            } else if (df_type == "autre2releve") {

                dplyr::mutate(., 
                    # character cols
                    dplyr::across(
                        .cols = c(
                            .data$interview__id, .data$interview__key, 
                            .data$produit_nom, .data$q117autre_autre2
                        ),
                        .fns = as.character
                    ),
                    # double cols
                    dplyr::across(
                        .cols = c(
                            .data$autre2releve__id, .data$unitesautre2__id, 
                            .data$q120b_autre2, .data$q120a_autre2
                        ), 
                        .fns = as.double
                    )

                )

            }

        }

}

#' Rename variables to harmonized names
#' 
#' First, changes variable names by removing the product-specific component, thereby leaving harmonized names.
#' Then, overwrite the input df in the global environment with an updated df.
#' 
#' @param df Data frame whose columns to rename.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' 
#' @return Data frame with whose columns have harmonized names.
#' 
#' @importFrom stringr str_subset str_extract str_replace
#' @importFrom dplyr `%>%` case_when mutate rename_with everything starts_with
#' 
#' @export 
rename_variables <- function(
    df,
    df_type
) {

    get_product_name <- function(
        df,
        pattern
    ) {

        column_name <- stringr::str_subset(names(df), pattern = pattern)

        product_name <- stringr::str_extract(column_name, pattern = pattern)

        return(product_name)

    }

    # convert `df_type` input into the type of regex needed for tasks below
    pattern <- dplyr::case_when(
        df_type == "unitesFixes" ~ "(?<=unitesFixes_)[A-Za-z0-9]+(?=__id)",
        df_type == "releve" ~ "(?<=releve_)[A-Za-z0-9]+(?=__id)",
        df_type == "unitesautre1" ~ "(?<=unitesautre1)[A-Za-z0-9]+(?=__id)",
        df_type == "autre1releve" ~ "(?<=autre1releve_)[A-Za-z0-9]+(?=__id)",
        df_type == "unitesautre2" ~ "(?<=unitesautre2)[A-Za-z0-9]+(?=__id)",
        df_type == "autre2releve" ~ "(?<=autre2releve_)[A-Za-z0-9]+(?=__id)"
    )

    produit_nom <- get_product_name(df = df, pattern = pattern)

    df_renamed <- df %>%
        dplyr::mutate(produit_nom = produit_nom) %>%
        dplyr::rename_with(
            .cols = dplyr::everything(),
            .fn = ~ stringr::str_replace(
                string = .x,
                pattern = paste0("_", produit_nom),
                replacement = ""
            )
        ) %>%          
        { 
            if (df_type == "unitesautre1") {
                # roster ID
                rename_with(
                    .data = .,
                    .cols = starts_with("unitesautre1"),
                    .fn = ~ stringr::str_replace(
                        string = .x,
                        pattern = "(?<=unitesautre1)[a-z0-9]+(?=__id)",
                        replacement = ""
                    )
                )             
            } else if (df_type == "autre1releve") {
                # ID from parent roster
                rename_with(
                    .data = .,
                    cols = starts_with("unitesautre1"),
                    .fn = ~ stringr::str_replace(
                        string = .x,
                        pattern = "(?<=unitesautre1)[a-z0-9]+(?=__id)",
                        replacement = ""
                    )
                )
            } else if (df_type == "unitesautre2") {
                # roster ID
                rename_with(
                    .data = .,
                    .cols = starts_with("unitesautre2"),
                    .fn = ~ stringr::str_replace(
                        string = .x,
                        pattern = "(?<=unitesautre2)[a-z0-9]+(?=__id)",
                        replacement = ""
                    )
                )
            } else if (df_type == "autre2releve") {
                # ID from parent roster
                rename_with(
                    .data = .,
                    cols = starts_with("unitesautre2"),
                    .fn = ~ stringr::str_replace(
                        string = .x,
                        pattern = "(?<=unitesautre2)[a-z0-9]+(?=__id)",
                        replacement = ""
                    )
                )
            } else {
                .
            }


        }

    return(df_renamed)

}

#' Fix miscellaneous issues with input data
#' 
#' Master fix-it function that applies solutions to known data issues
#' 
#' Can easily be extended to new issues. Fixes to issues are organized as follows:
#' 
#' - At the top level, group by file type
#' - Within file type, group by particular files by checking for file-specific column names
#' 
#' @param df Data frame whose disparate issues to fix.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' 
#' @return Data frame with corrections, where applicable.
#' 
#' @importFrom dplyr `%>%` mutate rename rename_with matches
#' @importFrom labelled add_value_labels
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' 
#' @export 
fix_misc_issues <- function(
    df,
    df_type
) {

    if (df_type == "unitesautre1") {

        df_fixed <- df %>%
            # add q114a_autre column if missing
            {
                if (!"q114a_autre1" %in% names(df)) {
                    dplyr::mutate(
                        .data = .,
                        q114a_autre1 = NA_real_
                    ) %>%
                    labelled::add_value_labels(
                        .data = ., 
                        q114a_autre1 = c(Oui = 1, Non = 2)
                    )
                }
                else {
                    .
                }

            } %>%
            # misnamed variable in tomato concentrate df
            {
                if ("unitesautre1tmteCctr__id" %in% names(df)) {
                    dplyr::rename(
                        .data = .,
                        unitesautre1__id = .data$unitesautre1tmteCctr__id
                    )
                } else {
                    .
                }
            }

    } else if (df_type == "autre1releve") {

        df_fixed <- df %>%
            # misnamed variable in tomato concentrate df
            {
                if ("unitesautre1tmteCctr__id" %in% names(df)) {
                    dplyr::rename(
                        .data = .,
                        unitesautre1__id = .data$unitesautre1tmteCctr__id
                    )
                } else {
                    .
                }
            }

    } else if (df_type == "unitesautre2") {

        df_fixed <- df %>%
            # misnamed variable in tomato concentrate df
            {
                if ("unitesautre2tmteCctr__id" %in% names(df)) {
                    dplyr::rename(
                        .data = .,
                        unitesautre2__id = .data$unitesautre2tmteCctr__id
                    )
                } else {
                    .
                }
            } %>%
            # misnamed variables in sucre en morceaux df
            {
                if ("q116_sucrem" %in% names(df)) {
                    dplyr::rename_with(
                        .data = .,
                        .cols = matches("_sucrem"),
                        .fn = ~ stringr::str_replace(
                            string = .x, 
                            pattern = "_sucrem",
                            replacement = ""
                        )
                    )                    
                } else {
                    .
                }
            }

    } else if (df_type == "autre2releve") {

        df_fixed <- df %>%
            # misnamed variable in tomato concentrate df
            {
                if ("unitesautre2tmteCctr__id" %in% names(df)) {
                    dplyr::rename(
                        .data = .,
                        unitesautre2__id = .data$unitesautre2tmteCctr__id
                    )
                } else {
                    .
                }
            }

    } else {

        df_fixed <- df

    }

    return(df_fixed)

}


#' Extract labels from all labelled variables
#' 
#' First, determines which variables are labelled. 
#' Then, extracts the labels
#' Next, creates a named list: names are variable names; values are associated labels. 
#' Then, creates a label object in the global environment whose name is the `{df}_lbls`.
#' 
#' @param df Data frame whose value labels to extract
#' 
#' @return List of value labels. Each element is a named numeric vector. Each vector consists of a named values. The names correspond to character labels and the values to the numerical values.
#' 
#' @importFrom purrr map_lgl map
#' @importFrom haven is.labelled
#' 
#' @return Side-effect: create list object in global environment containing labels for all variables
extract_labels <- function(df) {

    # get list of all variable names
    vars_in_df <- names(df)

    # determine--TRUE/FALSE--which have labels
    which_have_labels_lgl <- purrr::map_lgl(
        .x = vars_in_df, 
        .f = ~ haven::is.labelled(df[[.x]]))

    # return the names of those variables with labels
    which_have_labels_names <- names(df)[which_have_labels_lgl]

    # extract labels for each variable into a list
    all_labels <- purrr::map(
        .x = which_have_labels_names,
        .f = ~ attr(x = df[[.x]], which = "labels")
    )

    # create a named list: name is variable name; value is corresponding labels
    labels_named <- stats::setNames(
        nm = which_have_labels_names,
        object = all_labels
    )

}

#' Label variables
#' 
#' Apply labels to all variables in the data frame that need them. To do this:
#' 
#' First, determine the variables that have labels for them in the list of labels.
#' Then, apply those labels to each column
#' 
#' @param df Data frame
#' @param labels_list List that contains the consolidated labels
#' 
#' @return Data frame with labelled columns
#' 
#' @importFrom dplyr `%>%` mutate across all_of cur_column
#' @importFrom haven labelled
label_variables <- function(
    df,
    labels_list
) {

    # extract variable names from label list
    vars_to_label <- names(labels_list)

    # apply labels to columns 
    df <- df %>%
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::all_of(vars_to_label),
                .fns = ~ haven::labelled(
                    .x, 
                    labels = labels_list[[dplyr::cur_column()]]
                )
            )
        )

    return(df)

}

#' Save data to disk
#' 
#' Save data to disk in Stata format
#' 
#' @param df Data frame to save to disk.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' @param dir Character. Directory where to save data.
#' 
#' @importFrom assertthat assert_that
#' @importFrom haven write_dta
save_nsu_data <- function(
    df,
    df_type,
    dir
) {

    expected_types <- c(
        # unites fixes
        "unitesFixes",
        "releve",

        # unites prévues ailleurs
        "unitesautre1",
        "autre1releve",

        # unites imprévues
        "unitesautre2",
        "autre2releve"
    )

    assertthat::assert_that(
        df_type %in% expected_types,
        msg = "Unexpected `type` provided."
    )

    assertthat::assert_that(
        dir.exists(dir),
        msg = "Directory provided in `dir` does not exist."
    )

    # construct files name
    file_name <- paste0(df_type, ".dta")

    # save file
    haven::write_dta(
        data = df,
        path = paste0(dir, file_name)
    )

}

#' Find the market file for the food group
#' 
#' First, filter files: 
#' - those with a .dta extension
#' - those that do not start with prefixes associated with other files (e.g., "interview__", "assignment__", "unitesFixes", etc.)
#' 
#' Then, retain as the market file the only file remaining.
#' 
#' @param dir Character. Directory whose files to search for the market file.
#' @param pattern_to_exclude Character. Regular expression of files that are not the market file.
#' 
#' @return Character. Name of the market file, with extension
#' 
#' @importFrom fs dir_ls path_file
#' @importFrom stringr str_subset
#' @importFrom glue glue
find_market_file <- function(
    dir,
    pattern_to_exclude = "^interview__|^assignment__|^autre2releve_|^unitesautre2|^autre1releve_|^unitesautre1|^releve_|^unitesFixes_"
) {

    all_files_paths <- fs::dir_ls(dir, type = "file")
    all_files <- fs::path_file(all_files_paths)
    dta_files <- stringr::str_subset(string = all_files, pattern = "\\.dta$")
    market_file <- stringr::str_subset(
        string = dta_files,
        pattern = pattern_to_exclude,
        negate = TRUE
    )

    n_market_files <- length(market_file)

    if (n_market_files == 1) {
        
        return(market_file)

    } else if (n_market_files == 0) {

        stop(glue::glue(
            "No market file found for the current food group",
            "Directory: {dir}",
            "Please check the name of the market file and change `pattern_to_exclude` so that this file is not excluded.",
            .sep = "\n"
        ))

    } else if (n_market_files > 1) {

        stop(glue::glue(
            "More than one potential market file found.",
            "These files are {market_file}",
            .sep = "\n"
        ))

    }

}

#' Combine NSU all data of same type in the same directory
#'  
#' @param dir_in Character. Directory path whose files should be combined.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' 
#' @importFrom dplyr if_else bind_rows `%>%` slice starts_with everything left_join mutate
#' @importFrom stringr str_ends
#' @importFrom purrr map walk2 pmap modify
#' @importFrom fs path_ext_remove path_file
#' @importFrom haven zap_labels read_dta
#' @importFrom tidyr pivot_longer
combine_nsu_in_dir <- function(
    dir_in,
    df_type
) {

    # add "/" to folder so that file paths later are correctly constructed
    dir_in <- dplyr::if_else(
        stringr::str_ends(dir_in, "/"),
        dir_in,
        paste0(dir_in, "/")
    )

    # get list of matching files
    files <- get_matching_files(
        dir = dir_in, 
        pattern = df_type
    )

    # ingest files
    data_list <- purrr::map(
        .x = files,
        .f = ingest_dta_file,
        dir = dir_in
    )

    # rename variables to harmonized names
    renamed_data_list <- purrr::map(
        .x = data_list,
        .f = ~ rename_variables(
            df = .x,
            df_type = df_type
        )
    )

    # grab names of files
    file_names <- purrr::map(
        .x = files,
        .f = ~ fs::path_ext_remove(fs::path_file(.x)) 
    )

    # apply those names to data frames in list
    named_data <- stats::setNames(
        object = renamed_data_list,
        nm = file_names
    )

    # fix errors in files
    fixed_data <- purrr::map(
        .x = named_data,
        .f = ~ fix_misc_issues(
            df = .x,
            df_type = df_type
        )
    )

    # check for required columns
    purrr::walk2(
        .x = fixed_data,
        .y = file_names,
        .f = ~ check_cols(
            df = .x,
            df_name = .y,
            df_type = df_type
        )
    )

    # remove labels from source data frames
    data_wo_labels <- purrr::map(
        .x = fixed_data,
        .f = ~ haven::zap_labels(.x)
    )

    # correct column types
    corrected_data <- purrr::map(
        .x = data_wo_labels,
        .f = ~ correct_col_type(
            df = .x,
            df_type = df_type
        )
    )

    # extract labels
    labels <- purrr::map(
        .x = fixed_data,
        .f = ~ extract_labels(df = .x)
    )    

    # combine labels into a single list of named labels
    combined_labels <- purrr::pmap(
        .l = unname(labels), 
        .f = c
    )

    # add an empty entry for q114 for fruits
    # this ensures that the loop is the same length for each named label--that is, that q114 is not missing for fruits
    # if (df_type == "unitesautre1" & (!"q114a_autre1" %in% names(labels))) {
    #     combined_labels = purrr::list_merge(
    #         .x = combined_labels, 
    #         q114a_autre1 = double()
    #     )
    # }

    # remove duplicate labels by modify each vector by removing duplicates
    consolidated_labels <- purrr::modify(
        .x = combined_labels, 
        .f = ~ .x[!duplicated(.x)]
    )

    # combine files
    nsu_combined <- dplyr::bind_rows(corrected_data)

    # apply value labels
    # nsu_combined <- label_variables(
    #     df = nsu_combined,
    #     labels_list = consolidated_labels
    # )
    
    # add product ID code and labels
    # drawing from market file where that information exists
    # first, identify the market file
    market_file <- find_market_file(dir = dir_in)
    # then, determine if the market file has any observations
    market_df <- haven::read_dta(file = paste0(dir_in, market_file))
    # if so, construct a product name-ID mapping
    if (nrow(market_df) > 0) {

        product_name_id_map <- market_df %>%
            dplyr::slice(1) %>% # take first obs, since all observations contain the same information
            select(starts_with("codeProduit_")) %>% # keep variables whose names contain the product name and whose values contains the code
            tidyr::pivot_longer(
                cols = everything(), 
                names_to = "produit_nom", 
                names_prefix = "codeProduit_",
                values_to = "produit_id"
            )
        # next, construct values labels for product IDs 
        product_id_labels <- stats::setNames(
            nm = product_name_id_map$produit_nom, 
            object = product_name_id_map$produit_id
        )
        product_id_labels <- list(product_id_labels)
        product_id_labels <- stats::setNames(
            nm = "produit_id",
            object = product_id_labels
        )

        labels <- c(consolidated_labels, product_id_labels)

        # then, add product IDs to data set and label codes
        nsu_combined <- nsu_combined %>%
            dplyr::left_join(product_name_id_map, by = "produit_nom")
    
    # if not, create empty entries:
    # - produit_id in the df
    # - produit_id entry in the labels
    # in this way, both data and labels have same length for cases where there are some observations
    } else {
        nsu_combined <- nsu_combined %>%
            dplyr::mutate(produit_id = NA_integer_)
        product_id_labels <- list(NA)
        product_id_labels <- stats::setNames(
            nm = "produit_id",
            object = product_id_labels
        )        
        labels <- c(consolidated_labels, product_id_labels)
    }
    
    data_and_labels <- list(
        data = nsu_combined,
        labels = labels
    )

}

#' Combine all NSU data of the same type across food group directories
#' 
#' Apply `combine_nsu_in_dir()` iteratively to each food group folders. Combine the data. Combine value labels. Save the output to combined data to disk in Stata format.
#' 
#' @param dir_in Character. Root directory where food group sub-directories are located.
#' @param dir_regexp Character. Regular expression to identify folders over which to iterate.
#' @param df_type Character. One of: "unitesFixes", "releve", "unitesautre1", "autre1releve", "unitesautre2", "autre2releve"
#' @param dir_out Character. Directory where combined files should be saved.
#' 
#' @importFrom fs dir_ls
#' @importFrom purrr map pluck pmap modify
#' @importFrom dplyr `%>%` bind_rows
combine_nsu_across_dirs <- function(
    dir_in,
    dir_regexp = "_STATA_",
    df_type,
    dir_out
) {

    # identify data folders to iterate over
    folders <- fs::dir_ls(
        dir_in, 
        type = "directory",
        regexp = dir_regexp,
        recurse = FALSE
    )

    # combine files in each folder, yielding
    # data and labels for each folder
    outputs <- purrr::map(
        .x = folders,
        .f = ~ combine_nsu_in_dir(
            dir_in = .x,
            df_type = df_type
        )
    )

    # bind together df from the "data" elements of the output
    combined_nsu_df <- seq_along(1:length(outputs)) %>%
        purrr::map(
            .f = ~ purrr::pluck(
                outputs, 
                .x, 
                "data"
            )
        ) %>% 
        dplyr::bind_rows()

    # combine value labels across all data so that all values are labelled
    combined_nsu_labels <- seq_along(1:length(outputs)) %>%
        # extract the "labels" element from each element of the output
        purrr::map(
            .f = ~ purrr::pluck(
                outputs, 
                .x, 
                "labels"
            )
        ) %>% 
        # combine labels into a single list of named labels
        purrr::pmap(.f = c) %>%
        # remove duplicate labels by modify each vector by removing duplicates
        purrr::modify(.f = ~ .x[!duplicated(.x)]) %>%
        # remove NA entries created if no entries in the market file
        purrr::modify(.f = ~ .x[!is.na(.x)])

    # apply combined labels to combined data frame
    nsu_combined <- label_variables(
        df = combined_nsu_df,
        labels_list = combined_nsu_labels
    )

    return(nsu_combined)

}

#' Fusionner l'ensemble des bases NSU
#' 
#' Créer les bases suivantes sous format Stata:
#' 
#' - unitesFixes
#' - releve
#' - unitesautre1
#' - autre1releve
#' - unitesautre2
#' - autre2releve
#' 
#' Ces bases seront créées en:
#' 
#' - Parcourant les répertoires qui correspondent à la chaîne regex `dir_regexp`.
#' - Fusionnant les bases cibles
#' - "Fusionnant" les étiquettes de valeurs pour les variables étiquettées
#' - Sauvegardant les bases résultantes dans le répertoire `dir_out`
#' 
#' @param dir_in Character. Root directory where food group sub-directories are located.
#' @param dir_regexp Character. Regular expression to identify folders over which to iterate.
#' @param dir_out Character. Directory where combined files should be saved.
#' 
#' @importFrom purrr map walk2
#' @importFrom haven write_dta
#' 
#' @export 
combine_nsu_data <- function(
    dir_in,
    dir_regexp = "_STATA_",
    dir_out
) {

    file_types <- c(
        "unitesFixes", "releve", 
        "unitesautre1", "autre1releve", 
        "unitesautre2", "autre2releve"
    )

    # create combined data files
    nsu_combined <- purrr::map(
        .x = file_types,
        .f = ~ combine_nsu_across_dirs(
            dir_in = dir_in,
            dir_regexp = dir_regexp,
            df_type = .x,
            dir_out = dir_out
        )

    )

    # write data to disk
    purrr::walk2(
        .x = nsu_combined,
        .y = file_types,
        .f = ~ haven::write_dta(
            data = .x,
            path = paste0(dir_out, .y, ".dta")
        )
    )

}

#' Fusionner les données relatives aux marchés
#' 
#' Créer un fichier `marches.dta` dans le répertoire désigné dans `dir_out`.
#' 
#' @param dir_in Character. Root directory where food group sub-directories are located.
#' @param dir_regexp Character. Regular expression to identify folders over which to iterate.
#' @param dir_out Character. Directory where combined files should be saved.
#' 
#' @importFrom fs dir_ls
#' @importFrom purrr map_chr map map2
#' @importFrom haven read_dta write_dta
#' @importFrom dplyr mutate select bind_rows
#' 
#' @export 
combine_market_data <- function(
    dir_in,
    dir_regexp = "_STATA_",
    dir_out
) {

    # compile list folders with data inside
    dirs <- fs::dir_ls(
        path = dir_in, 
        type = "directory", 
        regexp = dir_regexp, 
        recurse = FALSE
    )

    # find market file in folder
    market_files <- purrr::map_chr(
        .x = dirs,
        .f = ~ find_market_file(dir = .x)
    )

    file_paths <- paste0(names(market_files), "/", market_files)

    # ingest market files
    market_dfs <- purrr::map(
        .x = file_paths,
        .f = ~ haven::read_dta(file = .x)
    ) 

    # add column to indicate source file name
    # this will allow counting obs per food group
    market_dfs_w_source <- purrr::map2(
        .x = market_dfs,
        .y = market_files,
        .f = ~ mutate(
            .data = .x,
            fichier_source = .y
        )
    )

    # retain relevant columns only
    market_dfs_edited <- purrr::map(
        .x = market_dfs_w_source,
        .f = ~ dplyr::select(.data = .x,
            # SuSo identifiers
            interview__id, interview__key, assignment__id, fichier_source,
            # geographical IDs and other process data
            starts_with("s00q"), observation,
            # information on status
            has__errors, interview__status
        )
    )

    # combine dfs into one df
    market_df <- dplyr::bind_rows(market_dfs_edited)

    # save df to disk as Stata file
    haven::write_dta(data = market_df, path = paste0(dir_out, "/", "marches.dta"))

}

