DROP FUNCTION  IF EXISTS insee.formatDebugMessage;
DROP FUNCTION  IF EXISTS insee.compareNames;
DROP FUNCTION  IF EXISTS insee.formatDateString;
DROP PROCEDURE IF EXISTS insee.compare;
DROP PROCEDURE IF EXISTS insee.blacklist;
DROP PROCEDURE IF EXISTS insee.processOne;
DROP PROCEDURE IF EXISTS insee.processTodo;

DELIMITER //

CREATE FUNCTION insee.compareNames(
    name1 VARCHAR(80),
    name2 VARCHAR(80),
    isFirstName BOOLEAN) RETURNS INTEGER
DETERMINISTIC
BEGIN
    DECLARE n1, n2 VARCHAR(80);
    DECLARE word1, word2, curr_word VARCHAR(80);
    DECLARE word_count1, word_count2 INTEGER DEFAULT 0;
    DECLARE matching_words INTEGER DEFAULT 0;
    DECLARE found_match BOOLEAN;
    DECLARE words_in_order BOOLEAN DEFAULT TRUE;
    DECLARE has_only_soundex_matches BOOLEAN DEFAULT TRUE;  -- Pour tracker si on n'a que des variations orthographiques

    -- Validation initiale
    IF name1 IS NULL OR name2 IS NULL THEN
        RETURN -1;
    END IF;

    -- Normalisation complète et sophistiquée
    -- Étape 1 : Majuscules
    SET n1 = UPPER(name1);
    SET n2 = UPPER(name2);

    -- Étape 2 : Normalisation des apostrophes (courbes vers droites)
    SET n1 = REPLACE(n1, '’', "CHAR(39)");
    SET n2 = REPLACE(n2, '’', "CHAR(39)");

    -- Étape 3 : Normalisation des tirets (EN DASH et EM DASH vers simple)
    SET n1 = REPLACE(REPLACE(n1, '–', '-'), '—', '-');
    SET n2 = REPLACE(REPLACE(n2, '–', '-'), '—', '-');

    -- Étape 4 : Conversion des séparateurs en espaces
    SET n1 = REPLACE(REPLACE(n1, '-', ' '), "CHAR(39)", ' ');
    SET n2 = REPLACE(REPLACE(n2, '-', ' '), "CHAR(39)", ' ');

    -- Cinquième étape : nettoyage des espaces multiples dont nbsp/nnbsp
    SET n1 = TRIM(REGEXP_REPLACE(n1, '[   ]+', ' '));
    SET n2 = TRIM(REGEXP_REPLACE(n2, '[   ]+', ' '));

    -- Comptage des mots dans chaque chaîne
    SET word_count1 = (LENGTH(n1) - LENGTH(REPLACE(n1, ' ', '')) + 1);
    SET word_count2 = (LENGTH(n2) - LENGTH(REPLACE(n2, ' ', '')) + 1);

    -- Cas d'identité parfaite après normalisation
    IF n1 = n2 THEN
        -- Pour les prénoms, score basé sur le nombre de mots
        IF isFirstName THEN
            RETURN CASE
                WHEN word_count1 >= 3 THEN 4.0  -- Ex: "MARIE LOUISE HENRIETTE"
                WHEN word_count1 = 2 THEN 3.0   -- Ex: "MARIE LOUISE"
                ELSE 2.0                        -- Ex: "MARIE"
            END;
        -- Pour les noms, score plus simple
        ELSE
            RETURN CASE
                WHEN word_count1 >= 2 THEN 3  -- Ex: "DUPONT DURAND"
                ELSE 2                        -- Ex: "DUPONT"
            END;
        END IF;
    END IF;

    -- Pour les noms de famille (non prénoms), traitement simplifié
    IF NOT isFirstName THEN
        -- Comparaison mot à mot basique
        SET matching_words = 0;
        SET word1 = n1;
        WHILE LENGTH(word1) > 0 DO
            IF LOCATE(' ', word1) > 0 THEN
                SET curr_word = SUBSTRING(word1, 1, LOCATE(' ', word1) - 1);
                SET word1 = SUBSTRING(word1, LOCATE(' ', word1) + 1);
            ELSE
                SET curr_word = word1;
                SET word1 = '';
            END IF;

            IF LOCATE(curr_word, n2) > 0 THEN
                SET matching_words = matching_words + 1;
            END IF;
        END WHILE;

        RETURN CASE
            WHEN matching_words = 0 THEN -1
            ELSE FLOOR(matching_words)
        END;
    END IF;

    -- Analyse détaillée pour les prénoms
    SET matching_words = 0;
    SET word1 = n1;
    SET @position = 0;
    SET @prev_match_pos = 0;

    main_loop: WHILE LENGTH(word1) > 0 DO
        -- Extraction du mot courant de n1
        IF LOCATE(' ', word1) > 0 THEN
            SET curr_word = SUBSTRING(word1, 1, LOCATE(' ', word1) - 1);
            SET word1 = SUBSTRING(word1, LOCATE(' ', word1) + 1);
        ELSE
            SET curr_word = word1;
            SET word1 = '';
        END IF;
        SET @position = @position + 1;

        -- Recherche dans n2
        SET word2 = n2;
        SET found_match = FALSE;
        SET @match_pos = 0;

        inner_loop: WHILE LENGTH(word2) > 0 AND NOT found_match DO
            SET @match_pos = @match_pos + 1;

            IF LOCATE(' ', word2) > 0 THEN
                SET @compare_word = SUBSTRING(word2, 1, LOCATE(' ', word2) - 1);
                SET word2 = SUBSTRING(word2, LOCATE(' ', word2) + 1);
            ELSE
                SET @compare_word = word2;
                SET word2 = '';
            END IF;

            -- Test de correspondance exacte d'abord
            IF curr_word = @compare_word THEN
                SET matching_words = matching_words + 1;
                SET found_match = TRUE;
                SET has_only_soundex_matches = FALSE;

                -- Vérification de l'ordre
                IF @match_pos < @prev_match_pos THEN
                    SET words_in_order = FALSE;
                END IF;
                SET @prev_match_pos = @match_pos;

            -- Puis test de correspondance phonétique
            ELSEIF soundex(curr_word) = soundex(@compare_word) AND
                   LENGTH(curr_word) > 2 AND LENGTH(@compare_word) > 2 THEN
                SET matching_words = matching_words + 1;
                SET found_match = TRUE;
            END IF;
        END WHILE;
    END WHILE;

    -- Détermination du score final pour les prénoms
    -- Tous les mots correspondent (exactement ou avec variations orthographiques)
    IF matching_words = word_count1 AND word_count1 = word_count2 THEN
        IF words_in_order THEN
            RETURN CASE
                WHEN matching_words >= 3 THEN 4.0  -- Trois prénoms ou plus identiques
                WHEN matching_words = 2 THEN 3.0   -- Deux prénoms identiques
                ELSE 2.0                          -- Un prénom identique
            END;
        ELSE
            RETURN CASE
                WHEN matching_words >= 3 THEN 3.5  -- Trois prénoms ou plus permutés
                WHEN matching_words = 2 THEN 2.5   -- Deux prénoms permutés
                ELSE 2.0                          -- Cas rare mais géré
            END;
        END IF;
    -- Correspondance partielle
    ELSEIF matching_words > 0 THEN
        RETURN CASE
            WHEN has_only_soundex_matches THEN 1.0  -- Uniquement des variations orthographiques
            ELSE matching_words                     -- Nombre de prénoms qui correspondent
        END;
    END IF;

    -- Aucune correspondance
    RETURN -1;
END//

-- Fonction utilitaire pour formater une date en supprimant les parties vides
CREATE FUNCTION insee.formatDateString(
    p_day CHAR(2),
    p_month CHAR(2),
    p_year CHAR(4)
) RETURNS VARCHAR(10)
DETERMINISTIC
BEGIN
    RETURN CASE
        WHEN p_year = '0000' THEN ''
        WHEN p_month = '00' THEN p_year
        WHEN p_day = '00' THEN CONCAT(p_month, '/', p_year)
        ELSE CONCAT(p_day, '/', p_month, '/', p_year)
    END;
END//

create procedure insee.compare(
    IN tNom VARCHAR(80),
    IN tPrenom VARCHAR(80),
    IN tSexe CHAR(1),
    IN tNaissanceY CHAR(4),
    IN tNaissanceM CHAR(2),
    IN tNaissanceD CHAR(2),
    IN tNaissancePlace VARCHAR(500),
    IN tDecesY CHAR(4),
    IN tDecesM CHAR(2),
    IN tDecesD CHAR(2),
    IN tDecesPlace VARCHAR(500),
    IN iId INTEGER UNSIGNED,
    IN iNom VARCHAR(80),
    IN iPrenom VARCHAR(80),
    IN iSexe CHAR(1),
    IN iNaissanceY CHAR(4),
    IN iNaissanceM CHAR(2),
    IN iNaissanceD CHAR(2),
    IN iNaissancePlace VARCHAR(500),
    IN iNaissanceCode CHAR(5),
    IN iNaissanceLocalite VARCHAR(30),
    IN iNaissancePays VARCHAR(30),
    IN iDecesY CHAR(4),
    IN iDecesM CHAR(2),
    IN iDecesD CHAR(2),
    IN iDecesPlace VARCHAR(500),
    IN iDecesCode CHAR(5),
    IN iNumActe CHAR(9),
    OUT score INTEGER,
    OUT record VARCHAR(1000),
    OUT msg VARCHAR(1000)
)
BEGIN
    DECLARE scoreTmp, wc INTEGER;
    DECLARE iNom2, iPrenom2, tNom2, tPrenom2 VARCHAR(80);
    DECLARE placeNaissance VARCHAR(500);
    DECLARE placeDeces VARCHAR(500);
    DECLARE prenom_score INTEGER;
    DECLARE nom_score INTEGER;
    DECLARE v_location VARCHAR(500);
    DECLARE v_country VARCHAR(500);
    DECLARE fullPlaceName VARCHAR(500);
    DECLARE exact_matches INTEGER DEFAULT 0;
    DECLARE raw_prenom1, raw_prenom2 VARCHAR(80);
    DECLARE normalized_prenom1, normalized_prenom2 VARCHAR(80);
    DECLARE local_score INTEGER;

    -- Score initial -2 reste inchangé pour exiger plusieurs correspondances
    SET score = -2;
    SET local_score = 0;
    SET msg = '';

    -- 1. Comparaison des noms de famille
    SET nom_score = insee.compareNames(tNom, iNom, FALSE);
    CASE
        WHEN nom_score = 3 THEN
            -- Noms composés identiques - confiance maximale
            SET local_score = local_score + 3;
            -- Pas de message nécessaire
        WHEN nom_score = 2 THEN
            -- Noms simples identiques - bonne confiance
            SET local_score = local_score + 2;
            -- Pas de message nécessaire
        WHEN nom_score = 1 THEN
            -- Correspondance partielle (partie d'un nom composé)
            SET local_score = local_score + 1;
            SET msg = concat(msg, '\n Nom, partie commune : ', tNom, ' -> ', iNom);
        ELSE
            -- Noms différents
            SET local_score = local_score - 1;
            SET msg = concat(msg, '\n Nom : ', tNom, ' != ', iNom);
    END CASE;

    -- 2. Analyse détaillée des prénoms
    SET prenom_score = insee.compareNames(tPrenom, iPrenom, TRUE);

    -- Structure de décision basée sur les scores précis
    CASE
        WHEN prenom_score = 4.0 THEN
            -- Trois prénoms ou plus, parfaitement identiques
            SET local_score = local_score + 4;
            -- Pas de message nécessaire - correspondance parfaite

        WHEN prenom_score = 3.5 THEN
            -- Trois prénoms ou plus, tous présents mais dans un ordre différent
            SET local_score = local_score + 3;  -- Léger malus pour le désordre
            SET msg = concat(msg, '\n Prénoms, ordre différent: ',
                                 tPrenom, ' -> ', iPrenom);

        WHEN prenom_score = 3.0 THEN
            -- Deux prénoms identiques dans l'ordre
            SET local_score = local_score + 3;
            -- Pas de message nécessaire

        WHEN prenom_score = 2.5 THEN
            -- Deux prénoms présents mais dans un ordre différent
            SET local_score = local_score + 2;  -- Léger malus pour le désordre
            SET msg = concat(msg, '\n Prénoms, ordre différent : ',
                                 tPrenom, ' -> ', iPrenom);

        WHEN prenom_score = 2.0 THEN
            -- Un prénom identique ou prénoms inclus dans une chaîne plus longue
            SET local_score = local_score + 2;
            -- Pas de message car variation naturelle dans les actes

        WHEN prenom_score > 0 THEN
            -- Correspondance partielle avec variations possibles
            SET local_score = local_score + 1;
            -- Si on a des variations orthographiques
            IF (LOCATE('-', tPrenom) > 0 AND LOCATE('-', iPrenom) = 0) OR
               (LOCATE('-', tPrenom) = 0 AND LOCATE('-', iPrenom) > 0) THEN
                SET msg = concat(msg, '\n Prénoms, tiret vs espace : ',
                                     tPrenom, ' -> ', iPrenom);
            ELSE
                SET msg = concat(msg, '\n Prénoms, variantes : ',
                                     tPrenom, ' -> ', iPrenom);
            END IF;

        ELSE
            -- Prénoms complètement différents
            SET local_score = local_score - 1;
            SET msg = concat(msg, '\n Prénoms différents : ',
                                 tPrenom, ' -> ', iPrenom);
    END CASE;

    /* Date de naissance */
    IF tNaissanceD = iNaissanceD &&
       tNaissanceM = iNaissanceM &&
       tNaissanceY = iNaissanceY THEN
        set score = score + 1;
        set exact_matches = exact_matches + 1;
    ELSEIF tNaissanceY <> "0000" && iNaissanceY <> "0000" && abs(tNaissanceY-iNaissanceY) > 5 THEN
        set score = score - 2;
        set msg = concat(msg, '\n Date naissance : ',
            formatDateString(tNaissanceD, tNaissanceM, tNaissanceY), ' !=2 ',
            iNaissanceD, '/', iNaissanceM, '/', iNaissanceY);
    ELSE
        set scoreTmp = 0;
        IF tNaissanceD = "00" ||
           iNaissanceD = "00" ||
           tNaissanceD = iNaissanceD THEN
            set scoreTmp = scoreTmp + 1;
        END IF;
        IF tNaissanceM = "00" ||
           iNaissanceM = "00" ||
           tNaissanceM = iNaissanceM THEN
            set scoreTmp = scoreTmp + 1;
        END IF;
        IF tNaissanceY = iNaissanceY THEN
            set scoreTmp = scoreTmp + 1;
            -- Marquage des cas indécis avec année seule :
            -- uniquement si aucune autre donnée temporelle ne correspond
            IF tNaissanceD = "00" && tNaissanceM = "00" &&
               tDecesD = "00" && tDecesM = "00" && tDecesY = "0000" THEN
                set msg = concat(msg, '\n Indécis : correspondance uniquement sur l’année de naissance');
                set score = score + 0.5;
            END IF;
        END IF;
        IF scoreTmp > 1 THEN
            set msg = concat(msg, '\n Date naissance : ',
                formatDateString(tNaissanceD, tNaissanceM, tNaissanceY), ' =~ ',
                iNaissanceD, '/', iNaissanceM, '/', iNaissanceY);
        ELSE
            set score = score - 1;
            set msg = concat(msg, '\n Date naissance : ',
                formatDateString(tNaissanceD, tNaissanceM, tNaissanceY), ' != ',
                iNaissanceD, '/', iNaissanceM, '/', iNaissanceY);
        END IF;
    END IF;

    /* Lieu de naissance */
    SET placeNaissance = getPlaceName(
      iNaissanceCode,
      iNaissanceY,
      iNaissanceM,
      iNaissanceD
    );

    SET v_location = CASE WHEN iNaissanceLocalite <> ''
                          THEN InitCap(LOWER(iNaissanceLocalite))
                          ELSE '' END;
    SET v_country = CASE WHEN iNaissancePays <> ''
                         THEN InitCap(LOWER(iNaissancePays))
                         ELSE '' END;

    IF tNaissancePlace = placeNaissance AND tNaissancePlace != "" THEN
        SET score = score + 1;
        set exact_matches = exact_matches + 1;
    ELSEIF LOCATE(' (', placeNaissance) > 0 AND  -- department specification
         LOCATE(tNaissancePlace, SUBSTRING(placeNaissance, 1, LOCATE(' (', placeNaissance) - 1)) = 1 AND
         LENGTH(tNaissancePlace) = LOCATE(' (', placeNaissance) - 1 THEN
      SET score = score + 1;
      SET msg = CONCAT(msg, '\n Lieu naissance : ', tNaissancePlace, ' -> ', placeNaissance);
    ELSEIF iNaissanceCode >= '99000' THEN
      -- For foreign places, first format INSEE place with both locality and country
      SET fullPlaceName = CASE
          WHEN iNaissanceLocalite <> '' AND iNaissancePays <> ''
          THEN CONCAT(InitCap(LOWER(iNaissanceLocalite)), ', ', InitCap(LOWER(iNaissancePays)))
          WHEN iNaissanceLocalite <> ''
          THEN InitCap(LOWER(iNaissanceLocalite))
          ELSE InitCap(LOWER(iNaissancePays))
      END;

      -- Compare the full place name
      IF LOCATE(tNaissancePlace, fullPlaceName) != 0 OR
         LOCATE(fullPlaceName, tNaissancePlace) != 0 THEN
          set msg = CONCAT(msg, '\n Lieu naissance : ',
                          tNaissancePlace, ' =~ ', fullPlaceName);
      ELSE
          set score = score - 1;
          set msg = CONCAT(msg, '\n Lieu naissance : ',
                          tNaissancePlace, ' != ', fullPlaceName);
      END IF;
    ELSEIF LOCATE(placeNaissance, tNaissancePlace) != 0 THEN
        IF SUBSTRING(iNaissanceCode, 1, 2) != '99' THEN
            SET msg = CONCAT(msg, '\n Lieu naissance : ',
                            tNaissancePlace, ' =~ ', placeNaissance);
        END IF;
    ELSE
        SET score = score - 1;
        SET msg = CONCAT(msg, '\n Lieu naissance : ',
                        tNaissancePlace, ' != ', placeNaissance);
    END IF;

    /* Date de décès - Structure similaire */
    IF tDecesD = iDecesD &&
       tDecesM = iDecesM &&
       tDecesY = iDecesY THEN
        set score = score + 1;
        set exact_matches = exact_matches + 1;
    ELSEIF tDecesY <> "0000" && iDecesY <> "0000" && abs(tDecesY-iDecesY) > 5 THEN
        set score = score - 2;
        set msg = concat(msg, '\n Date décès : ',
            tDecesD, '/', tDecesM, '/', tDecesY, ' !=2 ',
            iDecesD, '/', iDecesM, '/', iDecesY);
    ELSE
        set scoreTmp = 0;
        IF tDecesD = "00" ||
           iDecesD = "00" ||
           tDecesD = iDecesD THEN
            set scoreTmp = scoreTmp + 1;
        END IF;
        IF tDecesM = "00" ||
           iDecesM = "00" ||
           tDecesM = iDecesM THEN
            set scoreTmp = scoreTmp + 1;
        END IF;
        IF tDecesY = iDecesY THEN
            set scoreTmp = scoreTmp + 1;
            -- Cas indécis uniquement si nous n'avons RIEN d'autre qui correspond
            IF tDecesD = "00" && tDecesM = "00" &&
               tNaissanceD = "00" && tNaissanceM = "00" && tNaissanceY = "0000" THEN
                set msg = concat(msg, '\n Indécis : correspondance uniquement sur l’année de décès');
                set score = score + 0.5;
            END IF;
        END IF;
        IF scoreTmp > 1 THEN
            set msg = concat(msg, '\n Date décès : ',
                tDecesD, '/', tDecesM, '/', tDecesY, ' =~ ',
                iDecesD, '/', iDecesM, '/', iDecesY);
        ELSE
            set score = score - 1;
            set msg = concat(msg, '\n Date décès : ',
                tDecesD, '/', tDecesM, '/', tDecesY, ' != ',
                iDecesD, '/', iDecesM, '/', iDecesY);
        END IF;
    END IF;

    /* Bonus pour correspondance des deux années */
    IF tNaissanceY = iNaissanceY && tDecesY = iDecesY &&
       tNaissanceY <> "0000" && tDecesY <> "0000" THEN
        set score = score + 1;
    END IF;

    /* Lieu de décès */
    SET placeDeces = getPlaceName(
      iDecesCode,
      iDecesY,
      iDecesM,
      iDecesD
    );

    IF tDecesPlace = placeDeces AND tDecesPlace != "" THEN
        SET score = score + 1;
        SET exact_matches = exact_matches + 1;
    ELSEIF LOCATE(' (', placeDeces) > 0 AND  -- Has department specification
         LOCATE(tDecesPlace, SUBSTRING(placeDeces, 1, LOCATE(' (', placeDeces) - 1)) = 1 AND
         LENGTH(tDecesPlace) = LOCATE(' (', placeDeces) - 1 THEN
      SET score = score + 1;
      SET msg = CONCAT(msg, '\n Lieu décès : ', tDecesPlace, ' -> ', placeDeces);
    ELSEIF LOCATE(tDecesPlace, placeDeces) != 0 THEN
        SET msg = CONCAT(msg, '\n Lieu décès : ',
                        tDecesPlace, ' -> ', placeDeces);
    ELSEIF LOCATE(placeDeces, tDecesPlace) != 0 THEN
        IF SUBSTRING(iDecesCode, 1, 2) != '99' THEN
            SET msg = CONCAT(msg, '\n Lieu décès : ',
                            tDecesPlace, ' =~ ', placeDeces);
        END IF;
    ELSE
        SET score = score - 1;
        SET msg = CONCAT(msg, '\n Lieu décès : ',
                        tDecesPlace, ' != ', placeDeces);
    END IF;

   -- Bonus pour correspondances multiples
    IF exact_matches >= 3 THEN
        SET score = score + 3;  -- Bonus important pour 3+ correspondances exactes
    END IF;

    /* Record */
    SET msg = concat( msg, '\nInsee (', InitCap(iPrenom), ', acte n<sup>o</sup> ', iNumActe, ')' );

    SET record = CONCAT_WS('|',
      iNom, iPrenom, iSexe,
      CONCAT('°', iNaissanceD, '/', iNaissanceM, '/', iNaissanceY),
      CASE
          WHEN iNaissanceCode >= '99000' AND v_location <> ''
          THEN CONCAT(v_location,
                     CASE WHEN v_country <> ''
                          THEN CONCAT(', ', v_country)
                          ELSE '' END)
          ELSE placeNaissance
      END,
      CONCAT('+', iDecesD, '/', iDecesM, '/', iDecesY),
      placeDeces,
      iNaissanceCode, iNaissanceLocalite, iNaissancePays, iDecesCode,
      CONCAT('acte n° ', iNumActe)
    );
    SELECT CONCAT('DEBUG: Nom', tNom,
             ' Prénoms ', tPrenom,
             ' Score ', score,
             ' Matches exacts', exact_matches,
             ' États prénoms ', prenom_score) as DebugInfo;
END//

CREATE TABLE IF NOT EXISTS debug_log (
    id INT AUTO_INCREMENT PRIMARY KEY,
    step VARCHAR(100),
    details TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_step (step),
    INDEX idx_created (created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE FUNCTION insee.formatDebugMessage(
    p_cursor_name VARCHAR(20),
    p_key VARCHAR(100),
    p_id INTEGER,
    p_score INTEGER,
    p_total_matches INTEGER
) RETURNS VARCHAR(1000)
DETERMINISTIC
BEGIN
    RETURN CONCAT(
        RPAD(p_cursor_name, 8, ' '), ' | ',
        RPAD(IFNULL(p_key, ''), 30, ' '),
        'id', LPAD(p_id, 8, '0'),
        ', score ', LPAD(CAST(p_score AS CHAR), 3, ' '),
        ', correspondances ', p_total_matches
    );
END//

CREATE PROCEDURE insee.blacklist(
    IN p_table_name VARCHAR(64),
    IN p_nom VARCHAR(80),
    IN p_prenom VARCHAR(80),
    IN p_sexe CHAR(1),
    IN p_naissance_y CHAR(4),
    IN p_naissance_m CHAR(2),
    IN p_naissance_d CHAR(2),
    IN p_naissance_place VARCHAR(500),
    IN p_deces_y CHAR(4),
    IN p_deces_m CHAR(2),
    IN p_deces_d CHAR(2),
    IN p_deces_place VARCHAR(500)
)
BEGIN
    DECLARE v_blacklist_table VARCHAR(64);

    -- Only proceed if we have a database name
    IF @database_name IS NOT NULL THEN
        SET v_blacklist_table = CONCAT('blacklist_', @database_name);

        -- Check if blacklist table exists
        IF EXISTS (
            SELECT 1 FROM information_schema.tables
            WHERE table_name = v_blacklist_table
        ) THEN
            -- Construct the TodoKey and store it in a user variable
            SET @todo_key = CONCAT_WS('|',
                p_nom,
                p_prenom,
                p_sexe,
                CONCAT('°', p_naissance_d, '/', p_naissance_m, '/', p_naissance_y),
                p_naissance_place,
                CONCAT('+', p_deces_d, '/', p_deces_m, '/', p_deces_y),
                p_deces_place
            );

            -- First handle complete blacklist entries (null TodoKey and GwKey)
            SET @sql = CONCAT('
                UPDATE ', p_table_name, ' m
                SET m.IsBlacklisted = TRUE
                WHERE EXISTS (
                    SELECT 1
                    FROM `', v_blacklist_table, '` b
                    WHERE b.IdInsee = m.Id
                    AND b.TodoKey IS NULL
                    AND b.GwKey IS NULL
                )');
            PREPARE stmt1 FROM @sql;
            EXECUTE stmt1;
            DEALLOCATE PREPARE stmt1;

            -- Then handle specific TodoKey matches
            SET @sql = CONCAT('
                UPDATE ', p_table_name, ' m
                SET m.IsBlacklisted = TRUE
                WHERE EXISTS (
                    SELECT 1
                    FROM `', v_blacklist_table, '` b
                    WHERE b.IdInsee = m.Id
                    AND b.TodoKey = @todo_key
                )');
            PREPARE stmt2 FROM @sql;
            EXECUTE stmt2;
            DEALLOCATE PREPARE stmt2;

            -- Optional debug logging
            IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'debug_log') THEN
                SET @sql = CONCAT('
                    SELECT COUNT(*) INTO @total_blacklisted
                    FROM ', p_table_name, '
                    WHERE IsBlacklisted = TRUE');
                PREPARE stmt3 FROM @sql;
                EXECUTE stmt3;
                DEALLOCATE PREPARE stmt3;

                INSERT INTO debug_log (step, details)
                VALUES (
                    'BLACKLIST_APPLIED',
                    CONCAT('todoKey ', @todo_key, ', blacklisted ', IFNULL(@total_blacklisted, 0))
                );
            END IF;
        END IF;
    END IF;
END//

/*
=========================================================================
PROCEDURE: insee.processOne
=========================================================================
Description:
Cherche des correspondances possibles pour une personne entre une base
généalogique (TODO.lst d’insee.ml) et la base INSEE des décès.

Variables d'entrée (IN):
-----------------------
tNom VARCHAR(80)        : Nom de famille à rechercher
tPrenom VARCHAR(80)     : Prénom(s) à rechercher
tSexe CHAR(1)           : Sexe (1=M, 2=F)
tNaissanceY CHAR(4)     : Année de naissance (format YYYY)
tNaissanceM CHAR(2)     : Mois de naissance (format MM)
tNaissanceD CHAR(2)     : Jour de naissance (format DD)
tNaissancePlace VARCHAR(500) : Lieu de naissance
tDecesY CHAR(4)         : Année de décès (format YYYY)
tDecesM CHAR(2)         : Mois de décès (format MM)
tDecesD CHAR(2)         : Jour de décès (format DD)
tDecesPlace VARCHAR(500): Lieu de décès
tCle VARCHAR(100)       : Clé unique de référence

Variables de sortie (OUT):
-------------------------
etat INTEGER            : État du traitement
                           1=Match exact, 2=Différences acceptables, 3=Écarté
                          -1=Vivant?, -2=Indécis, -3=Non trouvé
                          -4=Score faible NP, -5=Score faible D
nbMatch INTEGER         : Nombre de correspondances trouvées
bestScore INTEGER       : Meilleur score obtenu
bestId INTEGER UNSIGNED : ID INSEE de la meilleure correspondance
bestRecord VARCHAR(1000): Enregistrement complet de la meilleure correspondance
bestMsg VARCHAR(1000)   : Message détaillé sur la correspondance

Variables internes:
------------------
iId INTEGER UNSIGNED: ID temporaire pour parcours des résultats INSEE
score INTEGER       : Score de correspondance calculé
record VARCHAR(1000): Enregistrement en cours d'analyse
msg VARCHAR(1000)   : Message pour l'enregistrement en cours
nbRows INTEGER      : Compteur de lignes traitées

Variables de session (@):
------------------------
@max_score INTEGER     : Score maximum trouvé dans la session
@best_id INTEGER       : ID de la meilleure correspondance dans la session

Variables temporaires de comparaison:
----------------------------------
cNaisD, cNaisM, cNaisY    : Comparaison date naissance
cDesD, cDesM              : Comparaison date décès
iNom, iPrenom, iSexe      : Données INSEE en cours
iNaissanceCode, iDecesCode: Codes géographiques INSEE
iNaissanceLocalite        : Localité de naissance INSEE
iNaissancePays            : Pays de naissance INSEE
iNumActe                  : Numéro d'acte INSEE

Variables de curseur:
------------------
theEnd INT: Contrôle de fin de curseur
cursorNP  : Curseur recherche par Nom/Prénom
cursorD   : Curseur recherche par Date

Tables temporaires:
------------------
temp_matches:
  - Id INTEGER UNSIGNED    : ID INSEE
  - Score INTEGER          : Score de correspondance
  - Record VARCHAR(1000)   : Enregistrement complet
  - Msg VARCHAR(1000)      : Messages de diagnostic
  - IsBlacklisted BOOLEAN  : Indicateur d'exclusion

Notes sur les scores:
--------------------
- Score initial : -2 (exige plusieurs correspondances)
- Bonus nom : +2
- Bonus prénom : +1 à +3 selon le nombre de prénoms
- Bonus dates exactes : +1 par date
- Bonus lieux : +1 par lieu
- Bonus correspondances multiples : +3 pour 3+ correspondances exactes
- Malus différences : -1 à -2 selon l'importance

=========================================================================
*/

CREATE PROCEDURE insee.processOne(
    IN tNom VARCHAR(80),
    IN tPrenom VARCHAR(80),
    IN tSexe CHAR(1),
    IN tNaissanceY CHAR(4),
    IN tNaissanceM CHAR(2),
    IN tNaissanceD CHAR(2),
    IN tNaissancePlace VARCHAR(500),
    IN tDecesY CHAR(4),
    IN tDecesM CHAR(2),
    IN tDecesD CHAR(2),
    IN tDecesPlace VARCHAR(500),
    IN tCle VARCHAR(100),
    OUT etat INTEGER,
    OUT nbMatch INTEGER,
    OUT bestScore INTEGER,
    OUT bestId INTEGER UNSIGNED,
    OUT bestRecord VARCHAR(1000),
    OUT bestMsg VARCHAR(1000)
)
BEGIN
    -- Déclarations des variables
    DECLARE iId INTEGER UNSIGNED;
    DECLARE iNom, iPrenom VARCHAR(80);
    DECLARE iSexe CHAR(1);
    DECLARE iNaissanceY, iDecesY CHAR(4);
    DECLARE iNaissanceM, iNaissanceD, iDecesM, iDecesD CHAR(2);
    DECLARE iNaissanceCode, iDecesCode CHAR(5);
    DECLARE iNaissanceLocalite, iNaissancePays VARCHAR(30);
    DECLARE iNaissancePlace, iDecesPlace VARCHAR(500);
    DECLARE iNumActe CHAR(9);
    DECLARE tId INTEGER UNSIGNED;
    DECLARE score INTEGER DEFAULT 0;
    DECLARE record, msg VARCHAR(1000);
    DECLARE nbRows INTEGER DEFAULT 0;
    DECLARE theEnd INT DEFAULT FALSE;
    DECLARE blacklist_prepared BOOLEAN DEFAULT FALSE;
    DECLARE v_blacklist_table VARCHAR(64);
    DECLARE is_blacklisted BOOLEAN DEFAULT FALSE;
    DECLARE current_cursor VARCHAR(20);
    DECLARE max_matches INTEGER DEFAULT 20;

    -- Curseurs optimisés
   -- Premier curseur : recherche la plus fiable par événement complet (date + lieu)
    DECLARE cursorEvent CURSOR FOR
        SELECT STRAIGHT_JOIN
            i.Id, i.Nom, i.Prenom, i.Sexe,
            i.NaissanceD, i.NaissanceM, i.NaissanceY,
            getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) as NaissancePlace,
            i.NaissanceCode, i.NaissanceLocalite, i.NaissancePays,
            i.DecesD, i.DecesM, i.DecesY,
            getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) as DecesPlace,
            i.DecesCode, i.NumeroActe
        FROM INSEE i USE INDEX (idx_nom_prenom)
        WHERE i.Sexe = tSexe
        AND i.Nom = tNom
        AND compareNames(i.Prenom, tPrenom, TRUE) >= 0
        AND (
            -- Événement de naissance complet
            (tNaissanceY != '0000'
             AND tNaissancePlace != ''
             AND i.NaissanceY = tNaissanceY
             AND i.NaissanceM = tNaissanceM
             AND i.NaissanceD = tNaissanceD
             AND getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD)
                 LIKE CONCAT('%', tNaissancePlace, '%'))
            OR
            -- Événement de décès complet
            (tDecesY != '0000'
             AND tDecesPlace != ''
             AND i.DecesY = tDecesY
             AND i.DecesM = tDecesM
             AND i.DecesD = tDecesD
             AND getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD)
                 LIKE CONCAT('%', tDecesPlace, '%'))
        )
        AND NOT EXISTS (
            SELECT 1
            FROM temp_matches t
            WHERE t.Id = i.Id
        )
        LIMIT 2;

    -- Deuxième curseur : recherche par données partielles (année ou lieu séparément)
    DECLARE cursorPartial CURSOR FOR
        SELECT STRAIGHT_JOIN
            i.Id,
            i.Nom, i.Prenom, i.Sexe,
            i.NaissanceD, i.NaissanceM, i.NaissanceY,
            getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) as NaissancePlace,
            i.NaissanceCode, i.NaissanceLocalite, i.NaissancePays,
            i.DecesD, i.DecesM, i.DecesY,
            getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) as DecesPlace,
            i.DecesCode, i.NumeroActe
        FROM INSEE i USE INDEX (idx_nom_prenom)
        WHERE i.Sexe = tSexe
        AND i.Nom = tNom
        AND compareNames(i.Prenom, tPrenom, TRUE) >= 0
        /*AND (i.Prenom LIKE CONCAT('%', tPrenom, '%')
             OR REPLACE(REPLACE(i.Prenom, '-', ' '), "'", ' ')
                LIKE CONCAT('%', REPLACE(REPLACE(tPrenom, '-', ' '), "'", ' '), '%')) */
        AND (
            -- Recherche par année de naissance
            (tNaissanceY != '0000' AND (
                i.NaissanceY = tNaissanceY  -- Année exacte
                OR i.NaissanceY BETWEEN tNaissanceY - 2 AND tNaissanceY + 2  -- ±2 ans pour approximations
                OR LEFT(i.NaissanceY, 3) = LEFT(tNaissanceY, 3)  -- Même décennie
            ))
            OR
            -- Recherche par année de décès
            (tDecesY != '0000' AND (
                i.DecesY = tDecesY
                OR LEFT(i.DecesY, 3) = LEFT(tDecesY, 3)  -- Même décennie
            ))
            OR
            -- Recherche par lieu de naissance seul
            (tNaissancePlace != ''
             AND getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD)
                 LIKE CONCAT('%', tNaissancePlace, '%'))
            OR
            -- Recherche par lieu de décès seul
            (tDecesPlace != ''
             AND getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD)
                 LIKE CONCAT('%', tDecesPlace, '%'))
        )
        AND NOT EXISTS (
            SELECT 1
            FROM temp_matches t
            WHERE t.Id = i.Id
        )
        LIMIT 12;

    -- Troisième curseur : recherche par nom/prénom uniquement
    DECLARE cursorName CURSOR FOR
        SELECT STRAIGHT_JOIN
            i.Id,
            i.Nom, i.Prenom, i.Sexe,
            i.NaissanceD, i.NaissanceM, i.NaissanceY,
            getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) as NaissancePlace,
            i.NaissanceCode, i.NaissanceLocalite, i.NaissancePays,
            i.DecesD, i.DecesM, i.DecesY,
            getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) as DecesPlace,
            i.DecesCode, i.NumeroActe
        FROM INSEE i USE INDEX (idx_nom_prenom)
        WHERE i.Sexe = tSexe
        AND i.Nom = tNom
        AND compareNames(i.Prenom, tPrenom, TRUE) >= 0
        AND NOT EXISTS (
            SELECT 1
            FROM temp_matches t
            WHERE t.Id = i.Id
        )
        LIMIT 4;

    -- Quatrième curseur : recherche par années de naissance ou décès
    DECLARE cursorDate CURSOR FOR
        SELECT STRAIGHT_JOIN
            i.Id,
            i.Nom, i.Prenom, i.Sexe,
            i.NaissanceD, i.NaissanceM, i.NaissanceY,
            getPlaceName(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) as NaissancePlace,
            i.NaissanceCode, i.NaissanceLocalite, i.NaissancePays,
            i.DecesD, i.DecesM, i.DecesY,
            getPlaceName(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) as DecesPlace,
            i.DecesCode, i.NumeroActe
        FROM INSEE i USE INDEX (idx_naissance, idx_deces)
        WHERE i.Sexe = tSexe
        AND ( -- Premier filtre sur les dates
            (tNaissanceY != '0000' AND
             i.NaissanceY = tNaissanceY AND
             i.NaissanceM = tNaissanceM)
            OR
            (tDecesY != '0000' AND
             i.DecesY = tDecesY AND
             i.DecesM = tDecesM)
        )
        AND ( -- Ensuite les comparaisons de noms
            (compareNames(i.Nom, tNom, FALSE) >= 0 AND compareNames(i.Prenom, tPrenom, TRUE) >= 0)
            OR
            (compareNames(i.Nom, tNom, FALSE) >= 0 AND SUBSTRING_INDEX(i.Prenom, ' ', 1) = SUBSTRING_INDEX(tPrenom, ' ', 1))
            OR
            (SUBSTRING_INDEX(i.Nom, ' ', 1) = SUBSTRING_INDEX(tNom, ' ', 1) AND compareNames(i.Prenom, tPrenom, TRUE) >= 0)
        )
        AND NOT EXISTS (
            SELECT 1
            FROM temp_matches t
            WHERE t.Id = i.Id
        )
        LIMIT 4;

    DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;

    -- Table temporaire pour stocker uniquement les correspondances valides
    -- (non blacklistées et avec un score suffisant)
    DROP TEMPORARY TABLE IF EXISTS temp_matches;
    CREATE TEMPORARY TABLE temp_matches (
        Id INTEGER UNSIGNED,            -- ID INSEE
        Score INTEGER,                  -- Score calculé
        OriginalScore INTEGER,          -- On garde OriginalScore qui marche bien
        SourceCursor CHAR(20),          -- Identifie quel curseur a trouvé la correspondance
        Record VARCHAR(1000),           -- Enregistrement complet
        OriginalRecord VARCHAR(1000),
        Msg VARCHAR(1000),              -- Messages détaillés du scoring
        OriginalMsg VARCHAR(1000),      -- Nouveau
        ProcessedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        PRIMARY KEY (Id, ProcessedAt),
        INDEX idx_score (Score, OriginalScore)
    ) ENGINE=MEMORY;

    -- Préparation de l'instruction de vérification blacklist
    IF @database_name IS NOT NULL THEN
        SET v_blacklist_table = CONCAT('blacklist_', @database_name);

        -- Verify table exists first
        SET @check_table = CONCAT('
            SELECT COUNT(*) INTO @table_exists
            FROM information_schema.tables
            WHERE table_name = ''', v_blacklist_table, '''');
        PREPARE check_stmt FROM @check_table;
        EXECUTE check_stmt;
        DEALLOCATE PREPARE check_stmt;

        IF @table_exists > 0 THEN
            -- Prepare a more precise blacklist check query
            SET @blacklist_query = CONCAT(
                'SELECT COUNT(*) INTO @is_blacklisted FROM `', v_blacklist_table, '` b ',
                'WHERE b.IdInsee = ? AND (',
                '    (b.TodoKey IS NULL AND b.GwKey IS NULL) OR ',  -- Complete blacklist
                '    (b.TodoKey = ? AND b.GwKey = ?))'  -- Specific non-match
            );
            PREPARE blacklist_stmt FROM @blacklist_query;
            SET blacklist_prepared = TRUE;
        END IF;
    END IF;

    -- Initialisation des autres variables de suivi
    SET bestScore = NULL;
    SET nbMatch = 0;
    SET bestMsg = '';
    SET @max_cursors = IFNULL(@max_cursors, 2);
    SET @max_total_matches = IFNULL(@max_total_matches, 20);
    SET @continue_cursors = TRUE;

    -- 1. Recherche de correspondance exacte
    /* SELECT CONCAT('Recherche ', tNom, ' ', tPrenom) as Debug; */

    SELECT MIN(Id) INTO iId
    FROM INSEE USE INDEX (idx_nom_prenom)
    WHERE Nom = tNom
    AND Prenom = tPrenom
    AND Sexe = tSexe
    AND NaissanceY = tNaissanceY
    AND NaissanceM = tNaissanceM
    AND NaissanceD = tNaissanceD
    AND getPlaceName(NaissanceCode, NaissanceY, NaissanceM, NaissanceD) = tNaissancePlace
    AND DecesY = tDecesY
    AND DecesM = tDecesM
    AND DecesD = tDecesD
    AND getPlaceName(DecesCode, DecesY, DecesM, DecesD) = tDecesPlace;

    IF iId IS NOT NULL THEN
        -- Correspondance complète mise en liste noire
        IF @database_name IS NOT NULL THEN
            SET v_blacklist_table = CONCAT('blacklist_', @database_name);

            -- Supprimer toutes les entrées existantes pour cet IdInsee
            SET @delete_sql = CONCAT(
                'DELETE FROM `', v_blacklist_table, '` ',
                'WHERE IdInsee = ', iId
            );
            PREPARE delete_stmt FROM @delete_sql;
            EXECUTE delete_stmt;
            DEALLOCATE PREPARE delete_stmt;

            -- (Ré)Insérer la nouvelle entrée
            SET @insert_sql = CONCAT(
                'INSERT INTO `', v_blacklist_table, '` ',
                '(IdInsee, TodoKey, GwKey) VALUES (',
                iId, ', NULL, NULL)'
            );
            PREPARE stmt FROM @insert_sql;
            EXECUTE stmt;
            DEALLOCATE PREPARE stmt;
        END IF;

       SELECT CONCAT('Correspondance complète ', tPrenom, ' ',
           tNom, ' en liste noire') as Debug;

       SET etat = 1;
       SET nbMatch = 1;
       SET bestScore = NULL;
       SET bestId = iId;
       SET bestRecord = "";
       SET bestMsg = "";
    ELSE  -- Boucles des curseurs, par ordre de fiabilité
        SET @total_matches = 1;

        -- 2. Curseur EVENT : recherche par événement complet
        SET @current_cursor = 'EVENT';
        SET theEnd = FALSE;
        OPEN cursorEvent;
        event_loop: LOOP

            FETCH cursorEvent INTO
                iId, iNom, iPrenom, iSexe,
                iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;
            IF theEnd OR @total_matches >= max_matches THEN
                CLOSE cursorEvent;
                LEAVE event_loop;
            END IF;

            -- Vérification préalable de la liste noire avant les calculs coûteux
            IF @database_name IS NOT NULL THEN
                -- Construction des variables utilisateur
                SET @id = iId;
                SET @todo_key = CONCAT_WS('|',
                    tNom, tPrenom, tSexe,
                    CONCAT('°', tNaissanceD, '/', tNaissanceM, '/', tNaissanceY),
                    tNaissancePlace,
                    CONCAT('+', tDecesD, '/', tDecesM, '/', tDecesY),
                    tDecesPlace
                );
                SET @gw_key = tCle;
                EXECUTE blacklist_stmt USING @id, @todo_key, @gw_key;
                IF @is_blacklisted > 0 THEN
                   -- SELECT CONCAT('EVENT | id', iId, ' BLACKLISTED') as Debug;
                   ITERATE event_loop;
                END IF;
            END IF;

            CALL insee.compare( -- Calcul du score via la procédure compare
                tNom, tPrenom, tSexe,
                tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
                tDecesY, tDecesM, tDecesD, tDecesPlace,
                iId, iNom, iPrenom, iSexe,
                iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
                score, record, msg
            );
            SELECT insee.formatDebugMessage(
                @current_cursor, tCle, iId, score, @total_matches
            ) as Debug;
            IF NOT EXISTS (SELECT 1 FROM temp_matches WHERE Id = iId) THEN
                INSERT INTO temp_matches (
                    Id, Score, OriginalScore,
                    SourceCursor, Record, OriginalRecord,
                    Msg, OriginalMsg
                )
                VALUES (
                    iId, score, score,
                    @current_cursor, record, record,
                    msg, msg
                );
            END IF;
            SET @total_matches = @total_matches + 1;
        END LOOP;

    IF @max_cursors >= 2 AND @continue_cursors THEN

        -- 3. Curseur PARTIAL : recherche par données partielles
        SET @current_cursor = 'PARTIAL';
        SET theEnd = FALSE;
        OPEN cursorPartial;
        partial_loop: LOOP

            FETCH cursorPartial INTO
                iId, iNom, iPrenom, iSexe,
                iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;
            IF theEnd OR @total_matches >= max_matches THEN
                CLOSE cursorPartial;
                LEAVE partial_loop;
            END IF;

            IF @database_name IS NOT NULL THEN
                SET @id = iId;
                SET @todo_key = CONCAT_WS('|',
                    tNom, tPrenom, tSexe,
                    CONCAT('°', tNaissanceD, '/', tNaissanceM, '/', tNaissanceY),
                    tNaissancePlace,
                    CONCAT('+', tDecesD, '/', tDecesM, '/', tDecesY),
                    tDecesPlace
                );
                SET @gw_key = tCle;
                EXECUTE blacklist_stmt USING @id, @todo_key, @gw_key;
                IF @is_blacklisted > 0 THEN
                   /* SELECT CONCAT('PARTIAL | id', iId, ' BLACKLISTED') as Debug; */
                   ITERATE partial_loop;
                END IF;
            END IF;

            CALL insee.compare(
                tNom, tPrenom, tSexe,
                tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
                tDecesY, tDecesM, tDecesD, tDecesPlace,
                iId, iNom, iPrenom, iSexe,
                iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
                score, record, msg
            );
            SELECT insee.formatDebugMessage(
                @current_cursor, tCle, iId, score, @total_matches
            ) as Debug;
            IF NOT EXISTS (SELECT 1 FROM temp_matches WHERE Id = iId) THEN
                INSERT INTO temp_matches (
                    Id, Score, OriginalScore,
                    SourceCursor, Record, OriginalRecord,
                    Msg, OriginalMsg
                )
                VALUES (
                    iId, score, score,
                    @current_cursor, record, record,
                    msg, msg
                );
            END IF;
            SET @total_matches = @total_matches + 1;
        END LOOP;

END IF;
IF @max_cursors >= 3 AND @continue_cursors THEN

        -- 4. Curseur NAME : recherche par nom/prénom seulement
        SET @current_cursor = 'NAME';
        SET theEnd = FALSE;
        OPEN cursorName;
        name_loop: LOOP

            FETCH cursorName INTO
                iId, iNom, iPrenom, iSexe,
                iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;
            IF theEnd OR @total_matches >= max_matches THEN
                CLOSE cursorName;
                LEAVE name_loop;
            END IF;

            IF @database_name IS NOT NULL THEN
                SET @id = iId;
                SET @todo_key = CONCAT_WS('|',
                    tNom, tPrenom, tSexe,
                    CONCAT('°', tNaissanceD, '/', tNaissanceM, '/', tNaissanceY),
                    tNaissancePlace,
                    CONCAT('+', tDecesD, '/', tDecesM, '/', tDecesY),
                    tDecesPlace
                );
                SET @gw_key = tCle;
                EXECUTE blacklist_stmt USING @id, @todo_key, @gw_key;
                IF @is_blacklisted > 0 THEN
                    ITERATE name_loop;
                    /* SELECT CONCAT('NAME | id', iId, , ' BLACKLISTED') as Debug; */
                END IF;
            END IF;

            CALL insee.compare(
                tNom, tPrenom, tSexe,
                tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
                tDecesY, tDecesM, tDecesD, tDecesPlace,
                iId, iNom, iPrenom, iSexe,
                iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
                score, record, msg
            );
            SELECT insee.formatDebugMessage(
                @current_cursor, tCle, iId, score, @total_matches
            ) as Debug;
            IF NOT EXISTS (SELECT 1 FROM temp_matches WHERE Id = iId) THEN
                INSERT INTO temp_matches (
                    Id, Score, OriginalScore,
                    SourceCursor, Record, OriginalRecord,
                    Msg, OriginalMsg
                )
                VALUES (
                    iId, score, score,
                    @current_cursor, record, record,
                    msg, msg
                );
            END IF;
            SET @total_matches = @total_matches + 1;
        END LOOP;

END IF;

IF @max_cursors >= 4 AND @continue_cursors THEN

    -- 5. Curseur DATE : recherche par date
        SET @current_cursor = 'DATE';
        SET theEnd = FALSE;
        OPEN cursorName;
        date_loop: LOOP

            FETCH cursorName INTO
                iId, iNom, iPrenom, iSexe,
                iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;
            IF theEnd OR @total_matches >= max_matches THEN
                CLOSE cursorName;
                LEAVE date_loop;
            END IF;

            IF @database_name IS NOT NULL THEN
                SET @id = iId;
                SET @todo_key = CONCAT_WS('|',
                    tNom, tPrenom, tSexe,
                    CONCAT('°', tNaissanceD, '/', tNaissanceM, '/', tNaissanceY),
                    tNaissancePlace,
                    CONCAT('+', tDecesD, '/', tDecesM, '/', tDecesY),
                    tDecesPlace
                );
                SET @gw_key = tCle;
                EXECUTE blacklist_stmt USING @id, @todo_key, @gw_key;
                IF @is_blacklisted > 0 THEN
                    ITERATE date_loop;
                    /* SELECT CONCAT('NAME | id', iId, , ' BLACKLISTED') as Debug; */
                END IF;
            END IF;

            CALL insee.compare(
                tNom, tPrenom, tSexe,
                tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
                tDecesY, tDecesM, tDecesD, tDecesPlace,
                iId, iNom, iPrenom, iSexe,
                iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
                iNaissanceLocalite, iNaissancePays,
                iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
                score, record, msg
            );
            SELECT insee.formatDebugMessage(
                @current_cursor, tCle, iId, score, @total_matches
            ) as Debug;
            IF NOT EXISTS (SELECT 1 FROM temp_matches WHERE Id = iId) THEN
                INSERT INTO temp_matches (
                    Id, Score, OriginalScore,
                    SourceCursor, Record, OriginalRecord,
                    Msg, OriginalMsg
                )
                VALUES (
                    iId, score, score,
                    @current_cursor, record, record,
                    msg, msg
                );
            END IF;
            SET @total_matches = @total_matches + 1;
        END LOOP;
    END IF;
END IF;
    -- 6. THE END
    IF @total_matches > 0 THEN
        SELECT CONCAT(
        'RÉSULTATS :' , tCle, ' |',
        'score final ', IFNULL(bestScore, 'aucun'),
        ', état ', etat, 'correspondances ', nbMatch
        ) as Summary;

        -- Récupération directe du meilleur résultat
        SELECT OriginalScore, Id, OriginalRecord, OriginalMsg, SourceCursor
        INTO bestScore, bestId, bestRecord, bestMsg, @source_cursor
        FROM temp_matches
        ORDER BY OriginalScore DESC
        LIMIT 1;

        -- Compte des correspondances au meilleur score
        SELECT COUNT(*) INTO NbMatch
        FROM temp_matches
        WHERE ABS(Score - bestScore) <= 1;

        -- Détermination de l'état final
        SET etat = CASE
            WHEN bestScore IS NULL AND nbMatch = 1 THEN 3  -- Full match (blacklisted)
            WHEN @source_cursor = 'EVENT' THEN 2           -- Event cursor match
            WHEN @source_cursor = 'PARTIAL' THEN 1         -- Partial cursor match
            WHEN @source_cursor = 'NAME' THEN 0            -- Name-only cursor match
            WHEN @source_cursor = 'DATE' THEN -1           -- Date cursor match
            ELSE -2                                        -- No match found
        END;

    ELSE
        -- Aucune correspondance trouvée
        SELECT CONCAT(tCle, ' | Aucune correspondance trouvée') as Debug;
        SET etat = -3;
        SET nbMatch = 0;
        SET bestScore = NULL;
        SET bestId = NULL;
        SET bestRecord = '';
        SET bestMsg = '';
    END IF;

    IF blacklist_prepared THEN
       DEALLOCATE PREPARE blacklist_stmt;
    END IF;
END//

CREATE PROCEDURE insee.processTodo()
BEGIN
    -- Existing declarations
    DECLARE tNom, tPrenom VARCHAR(80);
    DECLARE tSexe CHAR(1);
    DECLARE tNaissanceY, tDecesY CHAR(4);
    DECLARE tNaissanceM, tNaissanceD, tDecesM, tDecesD CHAR(2);
    DECLARE tNaissancePlace, tDecesPlace VARCHAR(500);
    DECLARE tCle VARCHAR(100);
    DECLARE myEtat, myNbMatch, myScore INTEGER;
    DECLARE tId, bestId INTEGER UNSIGNED;
    DECLARE myRecord, myMsg VARCHAR(1000);
    DECLARE progress_counter, total_records, elapsed_seconds,
            estimated_total_seconds, remaining_seconds, last_percentage, theEnd INT DEFAULT 0;
    DECLARE start_time, current_time_var TIMESTAMP;
    DECLARE batch_size INT DEFAULT 1000;
    DECLARE current_batch INT DEFAULT 0;
    DECLARE cursorTodo CURSOR FOR
        SELECT
            Id, Nom, Prenom, Sexe,
            NaissanceD, NaissanceM, NaissanceY, NaissancePlace,
            DecesD, DecesM, DecesY, DecesPlace,
            Cle
        FROM TODO
        WHERE Etat = 0;
    DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;
    DECLARE EXIT HANDLER FOR SQLEXCEPTION
BEGIN
    GET DIAGNOSTICS CONDITION 1
        @sqlstate = RETURNED_SQLSTATE,
        @errno = MYSQL_ERRNO,
        @text = MESSAGE_TEXT;

    SELECT
        CONCAT('SQL Error ', @errno, ' (', @sqlstate, '): ', @text) as ErrorDetails,
        CONCAT('Arrêt du traitement. Dernière ligne à traiter: ', Id) as Status,
        Id, Nom, Prenom, NaissancePlace
    FROM TODO
    WHERE Id = (SELECT MIN(Id) FROM TODO WHERE Etat = 0);
END;

    -- Create temporary table for batch results
    CREATE TEMPORARY TABLE IF NOT EXISTS batch_updates (
        Id INTEGER UNSIGNED PRIMARY KEY,
        Etat INTEGER,
        NbMatch INTEGER,
        Score INTEGER,
        IdInsee INTEGER UNSIGNED,
        Msg VARCHAR(1000),
        IsBlacklisted BOOLEAN DEFAULT FALSE
    ) ENGINE=MEMORY;

    -- Initialize counters and timer
    SELECT COUNT(*) INTO total_records FROM TODO WHERE Etat = 0;
    SET start_time = CURRENT_TIMESTAMP;

    -- Message de démarrage
    SELECT CONCAT('Traitement de ', total_records, ' enregistrement(s) :') as Status;


    SET theEnd = false;
    OPEN cursorTodo;

    -- Start batch processing
    TRUNCATE TABLE batch_updates;

    b1: LOOP
        -- Fetch record
        FETCH cursorTodo INTO tId,
            tNom, tPrenom, tSexe,
            tNaissanceD, tNaissanceM, tNaissanceY, tNaissancePlace,
            tDecesD, tDecesM, tDecesY, tDecesPlace, tCle;

        IF theEnd THEN
            -- Process any remaining records in the batch
            IF current_batch > 0 THEN
                -- Bulk update from temporary table
                UPDATE TODO t
                INNER JOIN batch_updates b ON t.Id = b.Id
                SET t.Etat = b.Etat,
                    t.NbMatch = b.NbMatch,
                    t.Score = b.Score,
                    t.IdInsee = b.IdInsee,
                    t.Msg = b.Msg;
            END IF;
            LEAVE b1;
        END IF;

        -- Process the record using processOne
        CALL insee.processOne(
            tNom, tPrenom, tSexe,
            tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
            tDecesY, tDecesM, tDecesD, tDecesPlace, tCle,
            myEtat, myNbMatch, myScore, bestId, myRecord, myMsg);

        -- Store result in batch table
        INSERT INTO batch_updates (Id, Etat, NbMatch, Score, IdInsee, Msg)
        VALUES (tId, myEtat, myNbMatch, myScore, bestId, CONCAT(myRecord, myMsg));

        SET current_batch = current_batch + 1;

        -- When batch is full, process it
        IF current_batch >= batch_size THEN
            -- Bulk update from temporary table
            UPDATE TODO t
            INNER JOIN batch_updates b ON t.Id = b.Id
            SET t.Etat = b.Etat,
                t.NbMatch = b.NbMatch,
                t.Score = b.Score,
                t.IdInsee = b.IdInsee,
                t.Msg = b.Msg;

            -- Reset batch counter and clear temporary table
            SET current_batch = 0;
            TRUNCATE TABLE batch_updates;
        END IF;

        -- Progress tracking
        SET progress_counter = progress_counter + 1;
        SET @current_percentage = FLOOR((progress_counter * 100) / total_records);

        IF @current_percentage >= last_percentage + 5 THEN
            SET current_time_var = CURRENT_TIMESTAMP;
            SET elapsed_seconds = TIMESTAMPDIFF(SECOND, start_time, current_time_var);

            IF elapsed_seconds > 0 THEN
                SET estimated_total_seconds = (elapsed_seconds * total_records) / progress_counter;
                SET remaining_seconds = estimated_total_seconds - elapsed_seconds;

                SELECT CONCAT(
                    @current_percentage, ' % (', progress_counter, '/', total_records,
                    ') — ~ ', ROUND(progress_counter - (last_percentage * total_records / 100)),
                    ' en ', FLOOR(elapsed_seconds/60), ' min',
                    ' — Temps restant estimé : ', FLOOR(remaining_seconds/60), ' min'
                ) as Progress;
            END IF;

            SET last_percentage = @current_percentage - MOD(@current_percentage, 5);
        END IF;
    END LOOP;

    CLOSE cursorTodo;

    /* Appliquer la blacklist sur les derniers enregistrements traités */
    IF @database_name IS NOT NULL AND current_batch > 0 THEN
        -- Appliquer la blacklist sur le dernier lot
        CALL insee.blacklist(
                'batch_updates',
                tNom, tPrenom, tSexe,
                tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
                tDecesY, tDecesM, tDecesD, tDecesPlace        );
        -- Faire la mise à jour finale
        UPDATE TODO t
        INNER JOIN batch_updates b ON t.Id = b.Id
        SET t.Etat = b.Etat,
            t.NbMatch = b.NbMatch,
            t.Score = b.Score,
            t.IdInsee = b.IdInsee,
            t.Msg = b.Msg;
    END IF;

    -- Affichage du résumé final
    SET current_time_var = CURRENT_TIMESTAMP;
    SET elapsed_seconds = TIMESTAMPDIFF(SECOND, start_time, current_time_var);
    SELECT CONCAT(
        'Traitement terminé - Durée totale: ',
        FLOOR(elapsed_seconds/60), 'm ', MOD(elapsed_seconds,60), 's'
    ) as Summary;
END//

DELIMITER ;
