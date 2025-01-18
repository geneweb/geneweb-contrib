-- Version : "Commune (D?partement)" si homonyme, "Commune" sinon
-- avec prise en compte des mouvements des communes depuis 1943

-- On crée une table normale au lieu d'une table temporaire pour les homonymes
DROP TABLE IF EXISTS commune_homonymes;
DROP TABLE IF EXISTS PlaceNorme;

-- Table des homonymes (non temporaire)
CREATE TABLE commune_homonymes (
    Libelle VARCHAR(500) PRIMARY KEY
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

-- Remplissage de la table des homonymes
INSERT INTO commune_homonymes
SELECT Libelle 
FROM cog_mvt1943 
GROUP BY Libelle 
HAVING COUNT(DISTINCT Code) > 1;

-- Affichage des statistiques sur les homonymes
SELECT 'Nombre de libellés homonymes' as Description, COUNT(*) as Nombre 
FROM commune_homonymes
UNION ALL
SELECT 'Nombre d''entrées concernées par les homonymes',
    COUNT(*) 
FROM cog_mvt1943 m
INNER JOIN commune_homonymes h ON m.Libelle = h.Libelle;

-- Création de la table finale
CREATE TABLE PlaceNorme (
    Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    Code CHAR(5) NOT NULL,
    DateDebut DATE NOT NULL,
    DateFin DATE DEFAULT NULL,
    Libelle VARCHAR(500),
    INDEX I_Place_Code (Code),
    INDEX I_Place_Dates (DateDebut, DateFin)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

-- Insertion avec gestion des homonymes
INSERT INTO PlaceNorme (Code, DateDebut, DateFin, Libelle)
SELECT 
    m.Code,
    m.DateDebut,
    m.DateFin,
    CASE 
        WHEN h.Libelle IS NOT NULL AND c.Departement IS NOT NULL THEN
            CONCAT(m.Libelle, ' (', d.Libelle, ')')
        ELSE 
            m.Libelle
    END as Libelle
FROM cog_mvt1943 m
LEFT JOIN commune_homonymes h ON m.Libelle = h.Libelle
LEFT JOIN COG_commune c ON m.Code = c.Code
LEFT JOIN COG_departement d ON c.Departement = d.Code;

-- Statistiques finales
SELECT 'Nombre total de lignes après jointures' as Description, COUNT(*) as Nombre 
FROM PlaceNorme
UNION ALL
SELECT 'Nombre de codes uniques après jointures', COUNT(DISTINCT Code) 
FROM PlaceNorme
UNION ALL
SELECT 'Nombre de libellés uniques après jointures', COUNT(DISTINCT Libelle) 
FROM PlaceNorme;

-- Nettoyage
DROP TABLE commune_homonymes;