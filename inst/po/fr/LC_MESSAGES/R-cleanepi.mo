��    L      |  e   �      p  ;   q  d   �  B     >   U  �   �  U     �   k  O   �  6   >	  l   u	  '   �	  (   

  N   3
  �   �
  %   *  T   P  X   �  Y   �  :   X  >   �  c   �  -   6  d   d  G   �       1   ,  *   ^     �  D   �  .   �  ?        T  )   n  �   �  /   %  S   U  \   �  ,     Q   3  O   �  D   �  �     L   �  �   �     k  V   �  W   B  N   �  *   �  9     9   N  ,   �  0   �  �   �  U   m  D   �       1     I   J  *   �  >   �  X   �  �   W  ~   �  D   ]     �  -   �     �  :     6   =  $   t     �  5   �      �     	    $  D   7  �   |  H     B   J  �   �  |   =  �   �  d   ^  I   �  �      -   �   -   �   c   �   �   O!  ,   "  W   ?"  R   �"  U   �"  A   @#  g   �#  ~   �#  ;   i$  S   �$  e   �$  #   _%  8   �%  1   �%     �%  V   �%  8   V&  b   �&  /   �&  3   "'  �   V'  7   �'  >   (  ^   X(  D   �(  \   �(  M   Y)  E   �)  �   �)  V   �*  �   �*  �   �+  `   ,  �   },  b   �,  K   b-  D   �-  n   �-  J   b.  E   �.  �   �.  x   �/  M   0     Z0  4   l0  P   �0  -   �0  L    1  _   m1  �   �1  ~   s2  W   �2  K   J3  '   �3     �3  >   �3  %   4  5   B4  "   x4  B   �4  +   �4  )   
5     !   .      H   J   :      8   >   -   7   $                      C         A              (       	   "           1          #           ,   E   I      B   *   5          3   2                    4   &              L   K       D   6       
   F                        G   =           ?   )                              %      '   +      ;       9   @   <          0   /    All new names must be different from existing column names. At least one data cleaning operation must be applied to this data before calling {.fn print_report}. At least two columns of type Date are required for this operation. Automatic detection of columns to convert into numeric failed. Can not replace the following values found in column {.code {opts}} but not defined in the dictionary: {.code {undefined_opts}}. Columns in `grp` column of the data dictionary must be found in the input data frame. Consider converting characters into numeric or replacing the numeric values by `NA` using the `replace_missing_values()` function. Constant data was removed after {.val {nrow(constant_data_report)}} iterations. Could not detect the provided missing value character. Detected {.code {length(bad_order)}} incorrect date sequence{?s} at line{?s}: {.code {toString(bad_order)}}. Did you enter an incorrect column name? Did you enter an incorrect correct name? Does your data contain missing value characters other than the specified ones? Enter {.code attr(dat, "report")[["constant_data"]]} for more information, where {.val dat} represents the object used to store the output from {.fn remove_constants}. Expected values with the same format. Found one or more columns with insuffisient numeric values for automatic conversion. Found the following unrecognised arguments to `clean_data()`: {.code {toString(extra)}}. Found the following unrecognised column name{?s}: {.code {target_columns[missing_cols]}}. Found {.code {nrow(dups)}} duplicated rows in the dataset. Found {.code {num_values}} numeric value{?s} in {.code {col}}. Found {ambiguous_count} numeric value{?s} that can also be of type Date in column {.code {x_name}}. Generating html report in {.file {temp_dir}}. Here `target_columns` is the name of the column that unique identifies the individuals in your data. Incorrect column names provided in column `grp` of the data dictionary. Incorrect data dictionary. Incorrect value provided to the `order` argument. Insufficient number of columns to compare. Invalid output format! Make sure that the columns to be renamed are part of the input data. No character column found from the input data. No character column with numeric values found by `scan_data()`. No duplicates were found. No report associated with the input data. Only one format is needed if all target columns contain values of the same format. Otherwise, one format per target column must be provided. Only {.val html} format is currently supported. Please specify names of the columns to convert into numeric using `target_columns`. Please specify the formats encountered in your column of interest via the `format` argument. Provided replace column names already exist. Target columns will be standardized using the following format: {.code {format}}. The following colonne{?s} will be converted into numeric: {.code {to_numeric}}. The following columns are mandatory: `options`, `values`, and `grp`. The list of functions in {.pkg cleanepi} can be found at: {.url https://epiverse-trace.github.io/cleanepi/reference/index.html}. The number of target columns does not match the number of specified formats. The percent of character values must be less than twice the numeric values for a column to be considered for automatic conversion. The value for the `dictionary` argument must a data frame with the following columns: `options`, `values`, `grp`, and `orders`. The value for {.emph end_date} argument must be of type {.cls Date} in ISO8601 format. To rename columns, use: `rename = c(new_name1 = 'old_name1', new_name2 = 'old_name2')`. Type {.code ?check_subject_ids} to see the help on the corresponding function. Unable to match formats to target columns. Unexpect type in the value for argument {.emph end_date}. Unexpected data type provided to `date_guess()` function. Unexpected format in the function arguments. Unrecognised column names specified in `rename`. Use `attr(dat, "report")[["duplicated_rows"]]` to access them, where `dat` is the object used to store the output from this operation. Value for `order` argument must be either a character or a list of character vectors. You can convert the values into character to enable format guessing. You can either: You did not specify a value for `target_columns`. You provided a name of a column of type {.cls {class(data[[end_date]])}}. You provided more arguments than expected. You've tried to convert values in different formats into Date. You've tried to guess the date format from values of type other than Date and character. `clean_data()` does not support arguments other than the defaults. Run get_default_params() to display the list of default parameters. `first_date` and `last_date` must be of type Date or character written in ISO8601 format ('2024-12-31' for December 31, 2024). add them to the dictionary using the `add_to_dictionary()` function. checking subject IDs format checking whether date sequences are respected cleaning column names converting {.code {toString(target_columns)}} into numeric correct the misspelled options from the input data, or performing dictionary-based cleaning removing duplicated rows removing the constant columns, empty rows and columns replacing missing values with NA standardizing Date columns Project-Id-Version: cleanepi 1.0.2.9000
PO-Revision-Date: 2024-11-07 12:38+0000
Last-Translator: Karim Mané
Language-Team: none
Language: fr
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=2; plural=(n > 1);
 Tout nouveau nom doit être différent des noms qui existent déjà. Au moins une opération de nettoyage de données doit être appliquée aux données avant d'évoquer la fonction {.fn print_report}. Cette opération nécessite au moins deux noms de colonnes de type Date. Impossible de détecter automatiquement les colonnes à convertir. Les valeures suivantes retrouvées dans la colonne {.code {opts}} et non définies dans le dictionnaire de données ne peuvent pas être remplacées: {.code {undefined_opts}}. Les noms de colonnes fournies dans la colonne `grp` du dictionnaire de données doivent figurer dans les données d'entrée. Vous pouvez soit convertir les valeures de type charactères en chiffre ou remplacer les valeures numériques avec `NA` via la fonction `replace_missing_values()`. Les données constantes ont été supprimées après {.val {nrow(constant_data_report)}} iterations. Impossible de détecter la valeure représentant les données manquantes. La séquence des dates est incorrecte à travers les {.code {length(bad_order)}} lignes suivantes: {.code {toString(bad_order)}}. Avez-vous fourni un nom de colonne incorrect? Avez-vous fourni un nom de colonne incorrect? Vos données contiennent-elles d'autres valeures utilisées pour désigner les données manquantes? Veuillez entrer {.code attr(dat, "report")[["constant_data"]]} pour plus d'information. {.val dat} represente ici l'object créé pour stocker le résultat de la fonction {.fn remove_constants}. Les valeures devaient avoir le même format. Des colonnes avec un nombre insuffisant de valeures numériques ont été identifiées. `clean_data()` ne reconnait pas les arguments suivants: {.code {toString(extra)}}. Les colonnes suivantes ne sont pas reconnues: {.code {target_columns[missing_cols]}}. Ces données contiennent {.code {nrow(dups)}} lignes redondantes. {.code {num_values}} valeures de type numérique ont été identifiées dans la colonne: {.code {col}}. {ambiguous_count} valeures numériques pouvant aussi être de type Date ont été identifiées à la colonne {.code {x_name}}. Creation du rapport en format html dans {.file {temp_dir}}. Ici, `target_columns` est le nom de la colonne avec les identifiants des individus. Des noms de colonnes incorrects ont été trouvés dans la colonne `grp` du dictionnaire de données. Dictionnaire de données incorrect. La valeure fournie à l'argument `order` est incorrecte. Le nombre de colonne à comparer est insuffisant. Format invalide! Assurez-vous que toutes les colonnes à renommer fassent parti des données d'entrée. Aucune colonne de type charactère n'a été identifiée Aucune colonne de type charactère contenant des chiffres n'a été identifiée par `scan_data()`. Aucune lignes redondantes n'a été détectée. Aucun rapport n'est associé à ce jeu de données. Seul un format est requis si les colonnes ciblées ont des valeures du même format. Sinon, vous devez fornir un format par colonne cible. Seul le format {.val html} est accepté pour le moment. Veuillez entrer les noms des colonnes à convertir en chiffre. Veuillez fournir les formats retrouvés dans la colonne d'intéret via le paramètre `format`. Les noms utilisés pour renommer certaines colonnes existent déjà. Les colonnes ciblée seront standardisées en utilisant le format suivant: {.code {format}}. Les colonnes suivantes seront converties en numérique: {.code {to_numeric}}. Les colonnes suivantes sont obligatoires: `options`, `values`, `grp`. Veuillez consulter la liste des fonction disponibles dans {.pkg cleanepi} à travers le lien suivant: {.url https://epiverse-trace.github.io/cleanepi/reference/index.html}. Le nombre de colonnes ciblées ne correspond pas au nombre de formats qui sont forunis Le pourcentage de valeures de type charactères doit faire moin du double de celui des valeures numériques pour qu'une colonne soit automatiquement convertie. La valeur fournie a l'argument `dictionary` doit être un data frame avec les colonnes suivantes: `options`, `values`, `grp`, et `orders`. La valeure de l'argument {.emph end_date} doit être de type {.cls Date} dans un format ISO8601. Les noms de colonnes peuvent être renommer comme suit: `rename = c(nouveau_nom1 = 'ancien_nom1', nouveau_nom2 = 'ancien_nom2')`. Veuillez entrer {.code ?check_subject_ids} pour plus d'information sur la function correspondante. Impossible d'établir la correspondence entre formats et colonnes ciblées. Le type de la valeure de l'argument {.emph end_date} est incorrecte. Le type de donnée fourni à la fonction `date_guess()` est différent du type que la fonction devait prendre. Les valeures fournies aux arguments de fonction ont un format non reconnu. Certains noms de colonnes fournis dans `rename` ne sont pas reconnus. Veuillez entrer `attr(dat, "report")[["duplicated_rows"]]` pour y accéder. `dat` represente ici l'object créé pour stocker le résultat de cette opération. L'argument `order` prend comme valeure un vecteur de charactère ou une liste d'un ou plusieurs vecteurs de charactère. Vous pouvez convertir les valeures en charactère pour détecter leur format. Vous pouvez soit: Vous n'avez pas donné la valeur de `target_columns`  Vous avez fourni le nom d'une colonne de type {.cls {class(data[[end_date]])}}. Vous avez fourni plus d'arguments que prévu. You avez essayé de convertir en Date des valeures ayant differents formats. Vous avez tenté de détecter le format de valeures qui ne sont ni de type Date ou charactère. `clean_data()` ne prend pas d'arguments autre que ceux qui sont définis par défault. Entrez get_default_params() pour consulter la list des arguments prédéfinis. `first_date` et `last_date` doivent être de type Date ou charactère au format ISO8601 ('2024-12-31' pour 31 Décembre 2024.) les ajouter au dictionnaire de données en utilisant la fonction `add_to_dictionary()`. vérification du format de la colonne contenant l'identifiant des individus vérification de la séquence des dates nettoyage des noms de colonnes conversion de {.code {toString(target_columns)}} en numérique corriger les options mal écrites, ou substitution de valeurs via dictionnaire des données suppression des lignes redondantes suppression des colonnes invariables, des lignes et colonnes vides remplacement des données manquantes par NA standardisation des colonnes de type Date 