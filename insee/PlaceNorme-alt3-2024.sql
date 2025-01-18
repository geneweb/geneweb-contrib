-- Version : "Commune (Département)" si homonyme, "Commune" sinon

DROP TEMPORARY TABLE IF EXISTS commune_homonymes;

DROP TABLE IF EXISTS PlaceNorme;

CREATE TABLE PlaceNorme (
    Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    Code CHAR(5) NOT NULL,
    DateFin CHAR(10) NOT NULL,
    Libelle VARCHAR(500),
    INDEX I_Place_Code (Code)
);

-- Création table temporaire ici, juste avant son utilisation
CREATE TEMPORARY TABLE commune_homonymes AS
SELECT Libelle 
FROM COG_commune 
WHERE TypeCom = 'COM'
GROUP BY Libelle 
HAVING COUNT(*) > 1;

CREATE INDEX idx_libelle ON commune_homonymes(Libelle);

insert into PlaceNorme
select null, c.Code, '2050-01-01', 
       case 
         when h.Libelle is not null 
         then concat(c.Libelle, ' (', d.Libelle, ')')
         else c.Libelle
       end
from COG_commune c
inner join COG_departement d on c.Departement = d.Code
left join commune_homonymes h on c.Libelle = h.Libelle
where c.TypeCom = "COM";

insert into PlaceNorme
select null, c.Code, '2050-01-01', concat_ws(', ', c.Libelle, n.Libelle)
from COG_commune c
inner join PlaceNorme n on c.Parent = n.Code
where c.TypeCom in ("COMA","COMD");

insert into PlaceNorme
select null, c.Code, '2050-01-01',
 concat(
 substr(c.Libelle, 1, locate(' ', c.Libelle)),
 lpad(
  substr(c.Libelle, locate(' ', c.Libelle)+1, locate('e', substr(c.Libelle, locate(' ', c.Libelle)+1))-1),
  2, '0'), ', ', n.Libelle )
from COG_commune c
inner join PlaceNorme n on c.Parent = n.Code
where TypeCom = "ARM";

insert into PlaceNorme
select null, m.AvCode, m.DateMvt, n.Libelle
from COG_mvt m
inner join PlaceNorme n on m.ApCode = n.Code
where TypeMvt = "41";

insert into PlaceNorme
select null, m.AvCode, m.DateMvt, concat(m.AvLibelle, ', ', n.Libelle)
from COG_mvt m
inner join PlaceNorme n on m.APCode = n.Code
where TypeMvt in ("31", "32", "33") and AvLibelle <> ApLibelle;