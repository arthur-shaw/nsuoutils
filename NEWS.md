# nsuoutils 0.1.1

- Exporter les fonctions `sort_images_product()` et `sort_images()`

# nsuoutils 0.1.0

- Gérer les données de production.
- Elargir les fonctions de fusion suivantes : 
  - `check_cols()`, `correct_col_type()`, `rename_variables()` s'attendent à voir et à traiter les colonnes spécifiques aux fichiers de production "unitesFixes", "unitesAutre1", "unitesAutre2"
  - `fix_misc_issues()` corrige de nouvelles erreurs spécifiques aux fichiers "unitesAutre1" et "unitesAutre2"
- `combine_nsu_in_dir`, `combine_nsu_across_dirs`, et `combine_nsu_data` ont acquis un paramètre `data_type` pour indiquer si ces fonctions auront à faire avec des données de consommation, relevé dans les marchés, ou des données de production, obtenu auprès des agriculteurs.

# nsuoutils 0.0.0.9000

* Première version. Fonctionnalité restreinte aux données de consommation.
