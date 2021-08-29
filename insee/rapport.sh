#!/bin/bash

MYSQL=./mysql.sh

$MYSQL -N << EOF
# Entête Prénom Nom
#   select concat( Prenom, Nom, '\n', concat_ws( '|',
# Entête Prénom Nom YYYY-YYYY
#   select concat( Prenom, Nom, NaissanceY-DecesY, '\n', concat_ws( '|',
# Entête par défaut Prénom.Occurence Nom (clé GeneWeb)
select concat( Cle, '\n', concat_ws( '|',
	Nom, Prenom, Sexe,
	concat( '°', NaissanceD, '/', NaissanceM, '/', NaissanceY),
	NaissancePlace,
	concat( '+', DecesD, '/', DecesM, '/', DecesY),
	DecesPlace ), '\n', Msg, '\nIdInsee(', IdInsee, ') Score ', Score, '\n')
from TODO
where (Etat = 2 and (NaissanceY <> '0000' or DecesY <> '0000'))
   or (Etat = -2 and score > 2)
order by score desc
;
EOF
