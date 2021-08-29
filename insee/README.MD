## Moulinette pour exploiter le fichier des personnes décédées de l’INSEE

* [Source primaire INSEE « fichier des personnes décédées »](https://www.data.gouv.fr/fr/datasets/fichier-des-personnes-decedees/)
* [Article « fichier des personnes décédées » de la Wikipédia française](https://fr.wikipedia.org/wiki/Fichier_des_personnes_décédées)
* [Code officiel géographique](https://www.insee.fr/fr/information/2560452) ([cog_ensemble_2019_csv.zip](https://www.insee.fr/fr/statistiques/fichier/3720946/cog_ensemble_2019_csv.zip))
* Pré-requis : `wget` et `jq` (en option GeneWeb avec son environnement de [dev Ocaml et dépendances Opam](https://geneweb.tuxfamily.org/wiki/OCaml))

La moulinette calcule un score de cohérence entre les données d’une liste personnelle d’individus et le fichier INSEE.

Un score de 6 correspond à des données identiques.

Seules les comparaisons ayant un score supérieur ou égal à 2 sont jugées exploitables.

### Création de la base MariaDB
```
sudo mysql -u root -p
```
Renseigner le mot de passe puis valider avec `Entrée`
Puis, dans la console `mysql>` :
```
create database insee character set UTF8;
create user 'insee'@'localhost' identified by '<mot_de_passe>';
grant all privileges on insee.* to 'insee'@'localhost';
grant file on *.* to 'insee'@'localhost';
exit
```

Si besoin, pour mémoriser le mot de passe de la connexion de manière un peu plus sécurisée (nécessaire uniquement pour MySQL):
```
mysql_config_editor set --login-path=client --user=insee --password
```

### Explications sur les divers scripts et outils SQL

- `dune build insee.exe` (nécessite Ocaml v4.07.0 maximum en l’état) : outil à compiler pour extraire les données d’une base GeneWeb à confronter au fichier INSEE ;
- `./mysql.sh` : permet de se connecter à la base de données MySQL (identifiants à ajuster) ;
- `./get-files.sh` : permet de récupérer et mettre à jour le fichier INSEE ;
- `./load-files.sh` : permet de charger tous les fichiers INSEE en base ;
- `./reload-file.sh` : permet de recharger une année du fichier INSEE en base ; 
- `./get-COG.sh` : permet de récupérer le référentiel des communes de l’INSEE (COG) ;
- `./mysql.sh < load-SQL.sql` : permet de chargrer le fichier des pays ;
- `./mysql.sh < load-COG.sql` : permet de charger le référentiel COG en base ;
- `./mysql.sh < PlaceNorme.sql` : permet de synthétiser le référentiel COG en une seule table (trois alternatives de nommenclatures sont disponibles) ;
- `./mysql.sh < getPlaceLib.sql` : création d’une fonction pour convertir un code INSEE en libellé commune pour une date donnée ;
- `./mysql.sh < processTodo.sql` : création de la procédure qui va confronter les données GeneWeb avec le fichier INSEE ;
- `./run-TODO.sh` : (à ajuster si besoin pour utilisation avec GeneWeb) si le binaire `insee.exe` est trouvé, lance l’extraction GeneWeb et génère un fichier `TODO.lst` qui est chargé ensuite dans la base MySQL et dont chaque ligne sera confrontée aux données de l’INSEE ; enfin le script utilise `rapport.sh` et `rapportFormate.sh` pour créer un rapport lisible pour simplifier les mises à jour dans GeneWeb.

Si vous n’avez pas GeneWeb, vous pouvez extraire les données de votre base dans un fichier `TODO.lst` avec comme séparateur | et colonnes :

- Nom (en majuscules sans accentuation) ;
- Prenom (en majuscules sans accentuation) ;
- Sexe (1 ou 2 comme dans le fichier INSEE) ;
- Année de naissance YYYY (0000 si inconnu) ;
- Mois de naissance MM (00 si inconnu) ;
- Jour de naissance DD (00 si inconnu) ;
- Lieu de naissance (ex. : Arras, Pas-de-Calais, France) ;
- Année de décès YYYY (0000 si inconnu) ;
- Mois de décès MM (00 si inconnu) ;
- Jour de décès (00 si inconnu) ;
- Clé (optionnelle, identifiant permettant de retrouver facilement la personne dans la base GeneWeb en cas d’homonymie, sous la « forme Prénom.Occurence Nom »).

Exemple fictif d’entrée pour le fichier `TODO.lst`, pour un Jean-François Dupont dont on sait qu'il est né en mars 1931 à Arras, la ligne sera :
`DUPONT|JEAN FRANCOIS|1|1931|03|00|Arras, Pas-de-Calais, France|0000|00|00||`
