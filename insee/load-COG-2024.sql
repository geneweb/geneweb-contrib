DROP TABLE IF EXISTS
    cog_pays, cog_pays_hist, cog_dept,
    cog_comm, cog_comm_hist, cog_mvt_comm,
    cog_comer, cog_comm_comer, cog_codes_ext;

CREATE TABLE COG_pays (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   COG CHAR(5) NOT NULL COMMENT 'Code du pays ou territoire',
   ACTUAL ENUM('1','2','3','4') NOT NULL COMMENT 'Code actualité',  
   CRPAY CHAR(5) COMMENT 'Code pays de rattachement',
   ANI CHAR(4) COMMENT 'Année d''apparition du code',
   LIBCOG VARCHAR(70) NOT NULL COMMENT 'Libellé utilisé dans le COG',
   LIBENR VARCHAR(200) NOT NULL COMMENT 'Nom officiel détaillé',
   CODEISO2 CHAR(2) NOT NULL COMMENT 'Code ISO 3166-1 alpha-2',
   CODEISO3 CHAR(3) NOT NULL COMMENT 'Code ISO 3166-1 alpha-3',
   CODENUM3 CHAR(3) COMMENT 'Code ISO 3166-1 numérique',
   UNIQUE KEY uk_code (COG),
   INDEX idx_iso (CODEISO2, CODEISO3),
   INDEX idx_libelle (LIBCOG)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table de référence des pays et territoires';

CREATE TABLE cog_pays_hist (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   Code CHAR(5) NOT NULL COMMENT 'Code pays (5 caractères)',
   CodeRattachement CHAR(5) COMMENT 'Code du pays actuel de rattachement',
   LibelleCOG VARCHAR(70) NOT NULL COMMENT 'Libellé utilisé dans le COG',
   LibelleDetailled VARCHAR(200) NOT NULL COMMENT 'Nom officiel ou composition détaillée',
   DateDebut DATE NOT NULL COMMENT 'Date de début du couple code*libellé',
   DateFin DATE COMMENT 'Date de fin (NULL = pays toujours actif)',
   INDEX idx_code_dates (Code, DateDebut, DateFin),
   INDEX idx_dates (DateDebut, DateFin),
   INDEX idx_libelle (LibelleCOG)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table des mouvements des pays depuis 1943';

CREATE TABLE COG_dept (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   DEP CHAR(3) NOT NULL COMMENT 'Code département',
   REG CHAR(2) NOT NULL COMMENT 'Code région',
   CHEFLIEU CHAR(5) NOT NULL COMMENT 'Code commune chef-lieu',
   TNCC CHAR(1) NOT NULL COMMENT 'Type nom en clair',
   NCC VARCHAR(200) NOT NULL COMMENT 'Nom en clair (majuscules)',
   NCCENR VARCHAR(200) NOT NULL COMMENT 'Nom en clair (typographie riche)',
   LIBELLE VARCHAR(200) NOT NULL COMMENT 'Nom avec article',
   UNIQUE KEY uk_code (DEP),
   INDEX idx_region (REG)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table de référence des départements';

CREATE TABLE COG_comm (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   TYPECOM ENUM('COM','COMA','COMD','ARM') NOT NULL COMMENT 'Type commune',
   COM CHAR(5) NOT NULL COMMENT 'Code commune',
   REG CHAR(2) NOT NULL COMMENT 'Code région',
   DEP CHAR(3) COMMENT 'Code département',
   CTCD CHAR(4) COMMENT 'Code collectivité territoriale',
   ARR CHAR(4) COMMENT 'Code arrondissement',
   TNCC CHAR(1) NOT NULL COMMENT 'Type nom en clair',
   NCC VARCHAR(200) NOT NULL COMMENT 'Nom en clair (majuscules)',
   NCCENR VARCHAR(200) NOT NULL COMMENT 'Nom en clair (typographie riche)',
   LIBELLE VARCHAR(200) NOT NULL COMMENT 'Nom avec article',
   CAN CHAR(5) COMMENT 'Code canton',
   COMPARENT CHAR(5) COMMENT 'Code commune parente',
   UNIQUE KEY uk_code (COM),
   INDEX idx_dept (DEP),
   INDEX idx_parent (COMPARENT),
   INDEX idx_type_dept (TYPECOM, DEP)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table de référence des communes';

CREATE TABLE cog_comm_hist (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   Code CHAR(5) NOT NULL COMMENT 'Code commune (5 caractères)',
   TNCC CHAR(1) NOT NULL COMMENT 'Type de nom en clair',
   NCC VARCHAR(200) NOT NULL COMMENT 'Nom en clair (majuscules)',
   NCCENR VARCHAR(200) NOT NULL COMMENT 'Nom en clair (typographie riche)',
   Libelle VARCHAR(200) NOT NULL COMMENT 'Nom avec article',
   DateDebut DATE NOT NULL COMMENT 'Date de début du couple code*libellé',
   DateFin DATE COMMENT 'Date de fin (NULL = commune toujours active)',
   INDEX idx_code_dates (Code, DateDebut, DateFin),
   INDEX idx_dates (DateDebut, DateFin),
   INDEX idx_libelle (Libelle),
   INDEX idx_ncc (NCC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table des mouvements de communes depuis 1943';

CREATE TABLE cog_mvt_comm (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   TypeMvt CHAR(2) NOT NULL COMMENT 'Type événement',
   DateMvt DATE NOT NULL COMMENT 'Date effet',
   AvTypeCom ENUM('COM','COMA','COMD','ARM') NOT NULL COMMENT 'Type commune avant',
   AvCode CHAR(5) NOT NULL COMMENT 'Code commune avant',
   TNCC_AV CHAR(1) NOT NULL COMMENT 'Type nom en clair avant',
   NCC_AV VARCHAR(200) NOT NULL COMMENT 'Nom en majuscules avant',
   NCCENR_AV VARCHAR(200) NOT NULL COMMENT 'Nom typographie riche avant',
   AvLibelle VARCHAR(200) NOT NULL COMMENT 'Nom avant avec article',
   ApTypeCom ENUM('COM','COMA','COMD','ARM') NOT NULL COMMENT 'Type commune après',
   ApCode CHAR(5) NOT NULL COMMENT 'Code commune après',
   TNCC_AP CHAR(1) NOT NULL COMMENT 'Type nom en clair après',
   NCC_AP VARCHAR(200) NOT NULL COMMENT 'Nom en majuscules après',
   NCCENR_AP VARCHAR(200) NOT NULL COMMENT 'Nom typographie riche après',
   ApLibelle VARCHAR(200) NOT NULL COMMENT 'Nom après avec article',
   INDEX idx_dates (DateMvt),
   INDEX idx_codes (AvCode, ApCode),
   INDEX idx_type_date (TypeMvt, DateMvt)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table des mouvements de communes depuis 1943';

CREATE TABLE cog_codes_ext (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   CodeExt CHAR(5) NOT NULL COMMENT 'Code extension RNIPP',
   CodeGeo CHAR(5) NOT NULL COMMENT 'Code territoire associé',
   LibGeo VARCHAR(70) NOT NULL COMMENT 'Libellé du territoire',
   INDEX idx_codes (CodeExt, CodeGeo)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Codes extension RNIPP pour territoires à forte natalité';

CREATE TABLE cog_comer (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   Code CHAR(3) NOT NULL COMMENT 'Code collectivité',
   TNCC CHAR(1) NOT NULL COMMENT 'Type nom en clair',
   NCC VARCHAR(200) NOT NULL COMMENT 'Nom en majuscules',
   NCCENR VARCHAR(200) NOT NULL COMMENT 'Nom typographie riche',
   Libelle VARCHAR(200) NOT NULL COMMENT 'Nom avec article',
   UNIQUE KEY uk_code (Code)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table des collectivités d’outre-mer';

CREATE TABLE cog_comm_comer (
   Id INTEGER UNSIGNED AUTO_INCREMENT PRIMARY KEY,
   Code CHAR(5) NOT NULL COMMENT 'Code du zonage',
   TNCC CHAR(1) NOT NULL COMMENT 'Type nom en clair',
   NCC VARCHAR(200) NOT NULL COMMENT 'Nom en majuscules',
   NCCENR VARCHAR(200) NOT NULL COMMENT 'Nom typographie riche',
   Libelle VARCHAR(200) NOT NULL COMMENT 'Nom avec article',
   NatureZonage CHAR(3) NOT NULL COMMENT 'Type de zonage',
   CodeComer CHAR(3) NOT NULL COMMENT 'Code COM de rattachement',
   LibelleComer VARCHAR(200) NOT NULL COMMENT 'Nom de la COM',
   INDEX idx_codes (Code, CodeComer)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci
COMMENT='Table des communes des collectivités d’outre-mer';

LOAD DATA 
   LOCAL INFILE 'COG/v_pays_territoire_2024.csv'
   INTO TABLE COG_pays
   CHARACTER SET utf8mb4 
   FIELDS TERMINATED BY ',' 
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (COG, ACTUAL, @crpay, ANI, LIBCOG, LIBENR, CODEISO2, CODEISO3, CODENUM3)
   SET 
       Id = NULL,
       CRPAY = NULLIF(@crpay, '');

LOAD DATA
   LOCAL INFILE 'COG/v_pays_et_territoire_depuis_1943.csv'
   IGNORE
   INTO TABLE cog_pays_hist
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (Code, @crpay, LibelleCOG, LibelleDetailled, DateDebut, @datefin)
   SET
       Id = NULL,
       CodeRattachement = NULLIF(@crpay, ''),
       DateFin = NULLIF(@datefin, '');

LOAD DATA 
   LOCAL INFILE 'COG/v_departement_2024.csv'
   INTO TABLE COG_dept
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ',' 
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (DEP, REG, CHEFLIEU, TNCC, NCC, NCCENR, LIBELLE);

LOAD DATA 
   LOCAL INFILE 'COG/v_commune_2024.csv'
   INTO TABLE COG_comm
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ',' 
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (TYPECOM, COM, REG, @dep, CTCD, ARR, TNCC, NCC, NCCENR, LIBELLE, @can, @comparent)
   SET
       DEP = NULLIF(@dep, ''),
       CAN = NULLIF(@can, ''),
       COMPARENT = NULLIF(@comparent, '');

LOAD DATA
   LOCAL INFILE 'COG/v_commune_depuis_1943.csv'
   IGNORE
   INTO TABLE cog_comm_hist
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (Code, TNCC, NCC, NCCENR, Libelle, DateDebut, @DateFin)
   SET
       Id = NULL,
       DateFin = NULLIF(@DateFin, '');

LOAD DATA
   LOCAL INFILE 'COG/v_mvt_commune_2024.csv'
   IGNORE
   INTO TABLE cog_mvt_comm
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (TypeMvt, DateMvt, AvTypeCom, @cod1, TNCC_AV, NCC_AV, NCCENR_AV, AvLibelle,
    ApTypeCom, @cod2, TNCC_AP, NCC_AP, NCCENR_AP, ApLibelle)
   SET
       Id = NULL,
       AvCode = LPAD(@cod1, 5, '0'),
       ApCode = LPAD(@cod2, 5, '0');

LOAD DATA
   LOCAL INFILE 'COG/v_codes_extension_2024.csv'
   INTO TABLE cog_codes_ext
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (CodeExt, CodeGeo, LibGeo);

LOAD DATA
   LOCAL INFILE 'COG/v_comer_2024.csv'
   INTO TABLE cog_comer
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
   LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (Code, TNCC, NCC, NCCENR, Libelle);

LOAD DATA
   LOCAL INFILE 'COG/v_commune_comer_2024.csv'
   INTO TABLE cog_comm_comer
   CHARACTER SET utf8mb4
   FIELDS TERMINATED BY ','
   OPTIONALLY ENCLOSED BY '"'
      LINES TERMINATED BY '\r\n'
   IGNORE 1 ROWS
   (Code, TNCC, NCC, NCCENR, Libelle, NatureZonage, CodeComer, LibelleComer);

SELECT
    RPAD(Description, 30, ' ') as '',
    LPAD(FORMAT(Nombre, 0), 8, ' ') as ''
FROM (
    SELECT 'Pays et territoires' as Description, COUNT(*) as Nombre FROM cog_pays
    UNION ALL SELECT 'Historique pays', COUNT(*) FROM cog_pays_hist
    UNION ALL SELECT 'Départements', COUNT(*) FROM cog_dept
    UNION ALL SELECT 'Communes', COUNT(*) FROM cog_comm
    UNION ALL SELECT 'Historique communes', COUNT(*) FROM cog_comm_hist
    UNION ALL SELECT 'Mouvements communes', COUNT(*) FROM cog_mvt_comm
    UNION ALL SELECT 'Collectivités outre-mer', COUNT(*) FROM cog_comer
    UNION ALL SELECT 'Communes outre-mer', COUNT(*) FROM cog_comm_comer
    UNION ALL SELECT 'Codes extension', COUNT(*) FROM cog_codes_ext
) stats
ORDER BY Description;