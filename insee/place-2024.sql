-- Table place_france : norme "Commune (Département)" si homonyme, "Commune"sinon
-- prise en compte des mouvements des communes, des communes d’outre-mer
-- et support brut des trois communes avec arrondissements.

-- Création de la table place_france
DROP TABLE IF EXISTS place_france;

CREATE TABLE place_france (
    Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    Code CHAR(5) NOT NULL,
    DateDebut CHAR(10) NOT NULL,
    DateFin CHAR(10) DEFAULT NULL,
    Libelle VARCHAR(500),
    INDEX I_Place_Code (Code),
    INDEX I_Place_Dates (DateDebut, DateFin)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

-- Support des communes avec des arrondissements
DROP PROCEDURE IF EXISTS InsertCommArrond;
DELIMITER //
CREATE PROCEDURE InsertCommArrond(
    prefix CHAR(4),     -- Préfixe du code INSEE (751+752, 132, 6938)
    ville VARCHAR(20),  -- Nom de la ville
    debut INT,          -- Premier arrondissement à insérer
    fin INT,            -- Dernier arrondissement à insérer
    date_debut CHAR(10) -- Date de début de validité
)
BEGIN
    DECLARE i INT;
    SET i = debut;
    WHILE i <= fin DO
        INSERT INTO place_france (Code, DateDebut, DateFin, Libelle)
        VALUES (
            CONCAT(prefix, RIGHT(CONCAT('0',i), 2)),
            date_debut,
            NULL,
            CONCAT(ville, ' ', CASE WHEN i=1 THEN '1er' ELSE CONCAT(i,'e') END)
        );
        SET i = i + 1;
    END WHILE;
END //
DELIMITER ;

-- Paris (20 arrondissements 751xx stables depuis 1859, avec extension 752xx)
CALL InsertCommArrond('751', 'Paris', 1, 20, '1943-01-01');
CALL InsertCommArrond('752', 'Paris', 1, 20, '1943-01-01');

-- Lyon (évolution des arrondissements)
-- 7 premiers arrondissements existaient avant 1943
CALL InsertCommArrond('6938', 'Lyon', 1, 7, '1943-01-01');
-- 8e arrondissement créé en 1959
CALL InsertCommArrond('6938', 'Lyon', 8, 8, '1959-01-01');
-- 9e arrondissement créé en 1964
CALL InsertCommArrond('6938', 'Lyon', 9, 9, '1964-01-01');

-- Lyon : code principal historique (69123)
INSERT INTO place_france (Code, DateDebut, DateFin, Libelle) VALUES
('69123', '1943-01-01', NULL, 'Lyon'),
-- Marseille : code principal (13055) et code supplémentaire (13155)
('13055', '1943-01-01', NULL, 'Marseille');

-- Marseille (création des arrondissements en 1946)
CALL InsertCommArrond('132', 'Marseille', 1, 16, '1946-01-01');

DROP PROCEDURE IF EXISTS InsertCommArrond;

-- Création d’une table temporaire pour les homonymies de communes
CREATE TEMPORARY TABLE homonymes (
    Libelle VARCHAR(500) PRIMARY KEY
) ENGINE=MEMORY;

INSERT INTO homonymes
SELECT Libelle
FROM cog_comm_hist
GROUP BY Libelle
HAVING COUNT(DISTINCT Code) > 1;

-- Stockage des statistiques d'homonymie dans des variables
SET @nb_homonymes = (SELECT COUNT(*) FROM homonymes);

SET @nb_entrees_homonymes = (
    SELECT COUNT(*)
    FROM cog_comm_hist m
    INNER JOIN homonymes h ON m.Libelle = h.Libelle
);

-- Remplissage de la table avec gestion des homonymes
INSERT INTO place_france (Code, DateDebut, DateFin, Libelle)
SELECT
    m.Code,
    m.DateDebut,
    m.DateFin,
    CASE
        WHEN h.Libelle IS NOT NULL THEN
            -- For homonyms, add department name in parentheses
            CONCAT(
                m.Libelle,          -- The commune name
                ' (',
                d.Libelle,          -- The department name from joined table
                ')'
            )
        ELSE
            m.Libelle
    END as Libelle
FROM cog_comm_hist m
LEFT JOIN homonymes h ON m.Libelle = h.Libelle
-- Join with department table using first 2 characters of commune code
LEFT JOIN cog_dept d ON LEFT(m.Code, 2) = d.DEP;

-- Insert COM territories
INSERT INTO place_france (Code, DateDebut, DateFin, Libelle)
SELECT DISTINCT
    c.Code,
    '1943-01-01' as DateDebut,
    NULL as DateFin,
    CASE 
        WHEN c.LibelleComer = c.NCC THEN c.NCC
        ELSE CONCAT(c.NCC, ' (', c.LibelleComer, ')')
    END as Libelle
FROM cog_comm_comer c
WHERE NOT EXISTS (
    SELECT 1 FROM place_france p
    WHERE p.Code = c.Code
);

-- Insert RNIPP extension codes
INSERT INTO place_france (Code, DateDebut, DateFin, Libelle)
SELECT DISTINCT
    ce.CodeGeo,
    '1943-01-01' as DateDebut,
    NULL as DateFin,
    ce.LibGeo as Libelle  -- Simply use the provided name
FROM cog_codes_ext ce
WHERE NOT EXISTS (
    SELECT 1 FROM place_france p
    WHERE p.Code = ce.CodeGeo
)
AND ce.CodeGeo < '99000'  -- This already includes DOM-TOM codes (97xxx)
AND ce.CodeGeo NOT LIKE '752%';  -- Paris handled separately

-- then insert all French territory extension codes
INSERT INTO place_france (Code, DateDebut, DateFin, Libelle)
SELECT
    ce.CodeExt,
    '1943-01-01' as DateDebut,
    NULL as DateFin,
    COALESCE(
        pn.Libelle,  -- First try: use existing place name
        ce.LibGeo    -- Fallback: use provided name
    ) as Libelle
FROM cog_codes_ext ce
LEFT JOIN place_france pn ON ce.CodeGeo = pn.Code
    AND pn.DateFin IS NULL
WHERE ce.CodeExt NOT IN (
    SELECT Code FROM place_france
)
AND ce.CodeExt < '99000'  -- Only French territories
AND ce.CodeExt NOT LIKE '752%';  -- Paris handled separately

-- Debug information to verify the import
SELECT
    CASE
        WHEN Code LIKE '976%' THEN 'Martinique Extensions'
        WHEN Code BETWEEN '90000' AND '98999' THEN 'Other Overseas Extensions'
        ELSE 'Metropolitan Extensions'
    END as Category,
    COUNT(*) as Count
FROM place_france
WHERE Code IN (
    SELECT CodeExt FROM cog_codes_ext
)
GROUP BY
    CASE
        WHEN Code LIKE '976%' THEN 'Martinique Extensions'
        WHEN Code BETWEEN '90000' AND '98999' THEN 'Other Overseas Extensions'
        ELSE 'Metropolitan Extensions'
    END
ORDER BY Count DESC;

-- Statistiques finales
SELECT RPAD(Description, 22, ' ') as '', LPAD(FORMAT(Nombre, 0), 6, ' ') as ''
FROM (
    SELECT 'Libellés homonymes' as Description, @nb_homonymes as Nombre
    UNION ALL
    SELECT 'Entrées avec homonymes', @nb_entrees_homonymes
    UNION ALL
    SELECT 'Total de lignes', COUNT(*) FROM place_france
    UNION ALL
    SELECT 'Code Insee uniques', COUNT(DISTINCT Code) FROM place_france
    UNION ALL
    SELECT 'Libellés distincts', COUNT(DISTINCT Libelle) FROM place_france
) stats;

-- Create table for foreign places with historical support
-- Table for country places with historical support
DROP TABLE IF EXISTS place_country;
CREATE TABLE place_country (
    Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    Code CHAR(5) NOT NULL,
    DateDebut DATE NOT NULL,
    DateFin DATE,
    Libelle VARCHAR(500),
    LibelleDetail VARCHAR(500), -- For additional historical context
    CodeISO2 CHAR(2),           -- When available, helps validate current countries
    CodeISO3 CHAR(3),           -- When available, helps validate current countries
    Source ENUM('COG', 'HIST', 'MANUAL') NOT NULL,
    INDEX idx_code_dates (Code, DateDebut, DateFin),
    INDEX idx_dates (DateDebut, DateFin)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

-- Insert current country codes from COG
INSERT INTO place_country (
    Code, DateDebut, DateFin, Libelle, LibelleDetail,
    CodeISO2, CodeISO3, Source
)
SELECT
    COG,
    COALESCE(
        STR_TO_DATE(CAST(ANI AS CHAR), '%Y'),
        '1943-01-01'
    ) as DateDebut,
    NULL as DateFin,
    LIBCOG,
    LIBENR,
    CODEISO2,
    CODEISO3,
    'COG'
FROM cog_pays
ORDER BY COG;

-- Add historical entries with date ranges
INSERT INTO place_country (
    Code, DateDebut, DateFin, Libelle, LibelleDetail, Source
)
SELECT
    Code,
    DateDebut,
    DateFin,
    LibelleCOG,
    LibelleDetailled,
    'HIST'
FROM cog_pays_hist h
WHERE Code >= '99000'  -- Only foreign countries
    AND NOT EXISTS (
        SELECT 1 FROM place_country p
        WHERE p.Code = h.Code
        AND p.Source = 'COG'
        AND p.DateFin IS NULL
    )
ORDER BY Code, DateDebut;

-- Add essential manual entries found in Insee death files data
INSERT INTO place_country (Code, DateDebut, DateFin, Libelle, Source) VALUES
('99000', '1943-01-01', NULL, 'Pays étranger', 'MANUAL'),
('99999', '1943-01-01', NULL, 'Pays étranger', 'MANUAL'),
('99016', '1943-01-01', NULL, 'Albanie*', 'MANUAL'),
('99052', '1943-01-01', NULL, 'Bulgarie*', 'MANUAL'),
('99062', '1943-01-01', NULL, 'Chypre*', 'MANUAL'),
('99066', '1943-01-01', '1992-12-31', 'Tchécoslovaquie', 'MANUAL'),
('99069', '1943-01-01', NULL, 'Danemark*', 'MANUAL'),
('99115', '1943-01-01', NULL, 'Grèce*', 'MANUAL'),
('99124', '1943-01-01', NULL, 'Hongrie*', 'MANUAL'),
('99141', '1943-01-01', NULL, 'Irlande*', 'MANUAL'),
('99142', '1943-01-01', NULL, 'Islande*', 'MANUAL'),
('99143', '1943-01-01', NULL, 'Israël*', 'MANUAL'),
('99146', '1943-01-01', '1991-12-31', 'Arménie*', 'MANUAL'),
('99149', '1943-01-01', NULL, 'Norvège*', 'MANUAL'),
('99150', '1943-01-01', NULL, 'Pologne*', 'MANUAL'),
('99152', '1943-01-01', NULL, 'Pays-Bas*', 'MANUAL'),
('99154', '1943-01-01', NULL, 'Roumanie*', 'MANUAL'),
('99183', '1943-01-01', NULL, 'Turquie*', 'MANUAL'),
('99202', '1943-01-01', NULL, 'Royaume-Uni*', 'MANUAL'),
('99233', '1943-01-01', NULL, 'Suède*', 'MANUAL'),
('99237', '1943-01-01', NULL, 'Suisse*', 'MANUAL'),
('99245', '1943-01-01', NULL, 'Ukraine*', 'MANUAL'),
('99307', '1943-01-01', NULL, 'Cameroun*', 'MANUAL'),
('99320', '1943-01-01', NULL, 'Congo*', 'MANUAL'),
('99325', '1943-01-01', NULL, 'Côte d''Ivoire*', 'MANUAL'),
('99386', '1943-01-01', '1976-12-31', 'Vietnam*', 'MANUAL'),
('99402', '1943-01-01', NULL, 'Madagascar*', 'MANUAL'),
('99504', '1943-01-01', NULL, 'Canada*', 'MANUAL'),
('99601', '1943-01-01', NULL, 'Australie*', 'MANUAL'),
('99606', '1943-01-01', NULL, 'Brésil*', 'MANUAL'),
('99607', '1943-01-01', NULL, 'Chili*', 'MANUAL'),
('99645', '1943-01-01', NULL, 'Uruguay*', 'MANUAL'),
('99735', '1943-01-01', NULL, 'Japon*', 'MANUAL');

CREATE OR REPLACE VIEW v_place_country AS
SELECT 
    Code,
    Libelle,
    COALESCE(LibelleDetail, Libelle) as LibelleComplet,
    DateDebut,
    DateFin,
    CodeISO2,
    CodeISO3,
    Source,
    ROW_NUMBER() OVER (
        PARTITION BY Code, 
        CASE WHEN DateFin IS NULL THEN 1 ELSE 0 END
        ORDER BY DateDebut DESC
    ) as rn
FROM place_country;

-- Standardize straight apostrophes (U+0027) to curved apostrophes (U+2019)
UPDATE place_france
SET Libelle = REPLACE(Libelle, "'", '’')
WHERE Libelle LIKE '%''%';

UPDATE place_country
SET Libelle = REPLACE(Libelle, "'", '’'),
    LibelleDetail = REPLACE(LibelleDetail, "'", '’')
WHERE Libelle LIKE '%''%' 
   OR LibelleDetail LIKE '%''%';

-- Statistics for place_country table
SELECT RPAD(Description, 30, ' ') as '', LPAD(FORMAT(Nombre, 0), 8, ' ') as ''
FROM (
    SELECT 'Total des entrées' as Description, 
           COUNT(*) as Nombre 
    FROM place_country
    
    UNION ALL
    SELECT 'Pays actuels (COG)', 
           SUM(CASE WHEN Source = 'COG' THEN 1 ELSE 0 END)
    FROM place_country
    
    UNION ALL
    SELECT 'Entrées historiques', 
           SUM(CASE WHEN Source = 'HIST' THEN 1 ELSE 0 END)
    FROM place_country
    
    UNION ALL
    SELECT 'Entrées manuelles', 
           SUM(CASE WHEN Source = 'MANUAL' THEN 1 ELSE 0 END)
    FROM place_country
    
    UNION ALL
    SELECT 'Codes pays uniques', 
           COUNT(DISTINCT Code)
    FROM place_country
    
    UNION ALL
    SELECT 'Entrées avec date de fin', 
           SUM(CASE WHEN DateFin IS NOT NULL THEN 1 ELSE 0 END)
    FROM place_country
) stats;

-- Quality checks with same formatting
SELECT RPAD(Description, 30, ' ') as '', LPAD(FORMAT(Nombre, 0), 8, ' ') as ''
FROM (
    SELECT 'Dates qui se chevauchent' as Description,
           COUNT(*) as Nombre
    FROM place_country p1
    JOIN place_country p2 ON p1.Code = p2.Code
        AND p1.Id < p2.Id
        AND p1.DateDebut < p2.DateFin
        AND (p2.DateDebut < p1.DateFin OR p1.DateFin IS NULL)
    
    UNION ALL
    SELECT 'Champs obligatoires manquants',
           COUNT(*)
    FROM place_country
    WHERE Code IS NULL 
       OR DateDebut IS NULL 
       OR Libelle IS NULL
) checks;

-- Function to get place name for a given date
-- Add known invalid codes constant to improve performance
DROP FUNCTION IF EXISTS insee.getPlaceName;

DELIMITER //

CREATE FUNCTION insee.getPlaceName(
    inCode CHAR(5),
    inYear CHAR(4),
    inMonth CHAR(2),
    inDay CHAR(2)
) RETURNS VARCHAR(500)
DETERMINISTIC
READS SQL DATA
BEGIN
    DECLARE res VARCHAR(500);
    DECLARE searchDate CHAR(10);
    
    -- Early validation including known invalid codes
    IF inCode = '99' THEN
      RETURN 'Pays étranger';
    ELSEIF inCode IS NULL OR LENGTH(TRIM(inCode)) != 5 THEN
      RETURN 'Code Insee invalide';
    END IF;
    
    SET searchDate = CONCAT(inYear, '-', inMonth, '-', inDay);
    
    -- Foreign country lookup
    IF inCode >= '99000' THEN
        -- just match the code and get the name regardless of dates
        -- administration often used modern codes retroactively
        SELECT Libelle INTO res
        FROM place_country
        WHERE Code = inCode
        LIMIT 1;
    ELSE
        -- Single query for French place with date handling
        IF searchDate < '1943-01-01' THEN
            -- For dates before 1943, take the earliest available name
            SELECT Libelle INTO res
            FROM place_france
            WHERE Code = inCode
            ORDER BY DateDebut ASC
            LIMIT 1;
        ELSE
            -- For dates after 1943, use the name valid at that time
            SELECT Libelle INTO res
            FROM place_france
            WHERE Code = inCode
                AND DateDebut <= searchDate 
                AND (DateFin > searchDate OR DateFin IS NULL)
            ORDER BY DateDebut DESC
            LIMIT 1;
        END IF;
    END IF;
    
    -- Message informatif selon le type de code manquant
    RETURN IFNULL(res,
        CASE 
            WHEN inCode >= '99000' THEN 'Pays inconnu'
            WHEN inCode < '99000' THEN 'Lieu inconnu'
            ELSE 'Code invalide'
        END
    );
END//

DELIMITER ;