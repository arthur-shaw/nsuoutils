% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/images.R
\name{sort_images}
\alias{sort_images}
\title{Classer les images selon le produit-unité}
\usage{
sort_images(
  inventory_df,
  data_dir,
  data_files = c("unitesFixes.dta", "unitesautre1.dta", "unitesautre1.dta"),
  dir_in,
  image_dir_pattern = "_Binary_",
  dir_out,
  strata_vars
)
}
\arguments{
\item{inventory_df}{Data frame. Base créée par \code{inventory_product_units()} qui fait l'inventaire complet des produits-unités des bases de l'enquête NSU.}

\item{data_dir}{Character. Répertoire où trouver les bases fusionnées par \code{combine_nsu_data()}.}

\item{data_files}{Character vector. Nom des bases d'où obtenir les identfiants des observations liées aux images.}

\item{dir_in}{Character. Répertoire racine dans lequel se trouve tous les répertoire d'images exportées de tous les produits.}

\item{image_dir_pattern}{Character. Expression régulière qui identifie les répertoires d'images exportés dans le répertoire racine \code{dir_in}.}

\item{dir_out}{Character. Répertoire racine dans lequel se trouve les répertoires destinataires créés par \code{create_image_dirs()}.}

\item{strata_vars}{Character vector. Nom des variables qui, ensemblent, identifient les strates.}
}
\description{
Met une copie de chaque image dans le bon répertoire selon le produit-unité décelé dans le nom de l'image.
}
\details{
La fonction marche comme suit:
\itemize{
\item Parcourir chaque répertoire d'image
\item Traiter chaque image rencontrée
\item Analyser le nom de l'image afin de comprendre le produit, l'unité, et la taille concernés
\item Construire un de l'image pour intégrer la valeur de \code{interview__key}
\item Copier chaque image depuis son répertoire source vers son répertoire destinataire (selon son produit-unité-taille)
}
}
