
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nsuoutils

<!-- badges: start -->
<!-- badges: end -->

L’objectif du package est de fournir des outils pour deux tâches
récurrantes de l’enquête sur les unités non-standard (NSU):

1.  Fusionner les bases
2.  Classer les images

L’enquête NSU produit un grand nombre de bases. Pour les NSU de
consommation, les données sont captées dans 11 instruments: un par
groupe de produits (e.g., céréales, viandes, etc.). Pour chaque
instrument, des informations sont collectées pour plusiers produits.
Pour chaque produit, des informations sont receuillies pour les unités
connues (`unitesFixes`), les unités connues pour d’autres produits
(`unitesautre1`), et les unités inconnues avant l’enquête
(`unitesautre2`) ainsi que les relevés de taille et de prix pour chacun
de ces types d’unités (`releve`, `autre1releve`, `autre2releve`). Autant
dire qu’il y a des dizaines de bases par produit, et peut-être des
centaines pour l’ensemble de l’enquête. Tout ceci doit être fusionné.

L’enquête NSU capte des images pour chaque produit-unité observé dans
les marchés. Dans les fichiers exportés, les images classées par
entretien. Cette stratégie d’organisation rend facile retrouver une
image pour un entretien donné. Mais ce classement n’est pas commode pour
d’autres tâches comme comparer la qualité des images au sein d’un groupe
d’un produit-image donné et choisir la meilleure image pour la taille
moyenne du produit-image. Tout ceci doit

Ce package fournit les outils pour accomplir ces deux tâches–la fusion
des bases et le classement des images–de manière aussi simple pour
l’utilisateur final que possible.

## Installation

Ce package n’est pas encore disponible sur CRAN, mais peut être installé
par la commande suivante:

``` r
# si {devtools} n'est pas installé, activer la commande ci-dessous afin de l'installer
# install.packages("devtools")
devtools::install_github("arthur-shaw/nsuoutils")
```

## Emploi

``` r
# enregistrer les répertoires clé
# ces répertoires seront utilisés par les exemples qui suivent
projet_dir <- "C:/EHCVM/NSU/"
donnees_entree_dir <- paste0(projet_dir, "entree/")
donnees_sortie_dir <- paste0(projet_dir, "sortie/")
images_entree_dir <- donnees_entree_dir
images_sortie_dir <- paste0(projet_dir, "images/")
```

### Fusionner les bases

``` r
# Fusionner en spécifiant:
# - le répertoire parent où retrouver les sous-répertoires avec données
# - le texte--une expression régulière--qui identifie les sous-répertoires avec données
# - le répertoire où sauvegarder les bases fusionnées en format Stata
combine_nsu_data(
    dir_in = donnees_entree_dir,
    dir_regexp = "_STATA_",
    dir_out = donnees_sortie_dir
)
```

### Classer les images

Le classement des images suit le flux de travail suivant:

``` r
# D'abord, faire l'inventaire des produits-unités
produits_unites <- inventory_product_units(dir = donnees_sortie_dir)

# Ensuite, créer des répertoires pour chaque produit et produit-unité retrouvé

create_image_dirs(
    inventory_df = produits_unites, 
    dir = donnees_sortie_dir
)

# Puis, copier les images vers les répertoires produit-unité
sort_images(
    inventory_df = produits_unites,
    dir_in = images_entree_dir,
    image_dir_pattern = "_Binary_",
    dir_out = images_sortie_dir    
)
```
