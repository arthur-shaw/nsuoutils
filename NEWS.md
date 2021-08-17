# nsuoutils 0.2.0

- Permet à `create_image_dirs()` de créer des répertoires produit-unité-strate où les strates sont définies par la combinaison de variables indiquée dans le paramètre `strata_vars`<
- Dote `sort_images()` de la capacité à classer les images dans le système de répertoires produit-unité-strate

# nsuoutils 0.1.5

- Appliquer des corrections identifées dans les bases du Niger: des noms de variables qui ne suivent pas les consignes.

# nsuoutils 0.1.4

- Corrections diverses dans les fonctions de classement d'images
- Ajout de documentation pour le paramètre `data_type` de la fonction `combine_market_data`

# nsuoutils 0.1.3

- Ajouter un paramètre `data_type` à la fonction `combine_market_data()` pour que les fichiers de sortie aient un nom approprié selon qu'il s'agit de données de consommation ou de production (i.e., "marches" ou "fermes", respectivement).

# nsuoutils 0.1.2

- Exporter `combine_market_data()`

# nsuoutils 0.1.1

- Exporter les fonctions `create_image_dirs()` et `sort_images()`.

# nsuoutils 0.1.0

- Gérer les données de production.
- Elargir les fonctions de fusion suivantes : 
  - `check_cols()`, `correct_col_type()`, `rename_variables()` s'attendent à voir et à traiter les colonnes spécifiques aux fichiers de production "unitesFixes", "unitesAutre1", "unitesAutre2"
  - `fix_misc_issues()` corrige de nouvelles erreurs spécifiques aux fichiers "unitesAutre1" et "unitesAutre2"
- `combine_nsu_in_dir`, `combine_nsu_across_dirs`, et `combine_nsu_data` ont acquis un paramètre `data_type` pour indiquer si ces fonctions auront à faire avec des données de consommation, relevé dans les marchés, ou des données de production, obtenu auprès des agriculteurs.

# nsuoutils 0.0.0.9000

* Première version. Fonctionnalité restreinte aux données de consommation.
