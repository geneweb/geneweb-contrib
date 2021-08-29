#!/bin/bash

MYSQL=./mysql.sh

echo
echo "Comparing ONE -> INSEE..."
echo BEGIN $(date '+%FT%T')
$MYSQL -N << EOF | sed 's/\\n/\n/g'
call processOne(
	"LEBORGNE", "JEANNE MARIE ANGELE", "2",
	"1913", "05", "24", "Beaurainville (Pas-de-Calais)",
	"1992", "03", "06", "Campagne-lès-Hesdin (Pas-de-Calais)",
	"myKey", @Etat, @nbr, @score, @id, @record, @msg);

select concat( case @Etat
	when -5 then 'Score faible D'
	when -4 then 'Score faible NP'
	when -3 then 'Non trouvé'
	when -2 then 'Indécis'
	when -1 then 'Vivant ?'
	when 1 then 'Identique'
	when 2 then 'Différences'
	when 3 then 'Écarté'
 end,
 "\nNbr : ", @nbr, "\nScore : ", @score, "\n", @record, @msg)
EOF
echo END $(date '+%FT%T')
