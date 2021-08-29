#!/bin/bash

MYSQL="./mysql.sh"

echo "(Re)create table..."
$MYSQL << EOF
drop table if exists INSEE;

create table INSEE (
	Id INTEGER UNSIGNED auto_increment primary key,
	Nom VARCHAR(80) not null,
	Prenom VARCHAR(80) not null,
	Sexe CHAR(1) not null,
	NaissanceY CHAR(4) not null,
	NaissanceM CHAR(2) not null,
	NaissanceD CHAR(2) not null,
	NaissanceCode CHAR(5) not null,
	NaissanceLocalite VARCHAR(30) not null,
	NaissancePays VARCHAR(30) not null,
	DecesY CHAR(4) not null,
	DecesM CHAR(2) not null,
	DecesD CHAR(2) not null,
	DecesCode CHAR(5) not null,
	NumeroActe CHAR(9) not null,
	Fichier VARCHAR(50) not null,
        index I_INSEE_N (Nom),
        index I_INSEE_BY (NaissanceY),
        index I_INSEE_DY (DecesY),
        index I_INSEE_F (Fichier)
);
EOF

for f in $(ls INSEE/deces*txt)
do

	annee=$(echo $f | sed -e 's#^INSEE/deces-##' -e 's/\.txt$//')
	if [[ "$annee" > "2016" ]]
	then
		charset=utf8
	else
		charset=ascii
	fi

	./reload-file.sh $f $charset

done
