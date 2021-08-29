#!/bin/bash

echo "Removing 2020 files in INSEE directory..."
rm INSEE/deces-2020*.txt

echo "Removing 2020 files in database..."
./mysql.sh << EOF
delete from INSEE where Fichier = 'INSEE/deces-2020.txt';
EOF

echo "Change last synchronisation date..."
echo "2020-02-01T00:00:00" > INSEE/last_sync.txt
