drop procedure if exists insee.compare;
drop procedure if exists insee.processOne;
drop procedure if exists insee.processTodo;
drop function if exists insee.wordcount;
drop function if exists insee.getPlaceLibHistorique;
drop function if exists insee.compareCompoundNames;
delimiter //

CREATE FUNCTION insee.compareCompoundNames(
    name1 VARCHAR(80),
    name2 VARCHAR(80),
    isFirstName BOOLEAN
) RETURNS INTEGER
DETERMINISTIC
BEGIN
    DECLARE score INTEGER DEFAULT 0;
    DECLARE n1, n2 VARCHAR(80);
    DECLARE first_word1, first_word2, word1, word2, curr_word VARCHAR(80);
    DECLARE word_count1, word_count2 INTEGER DEFAULT 0;
    DECLARE matching_words INTEGER DEFAULT 0;
    DECLARE found_match BOOLEAN;
    DECLARE words_in_order BOOLEAN DEFAULT TRUE;
    
    -- Normalisation
    SET n1 = UPPER(name1);
    SET n2 = UPPER(name2);
    SET n1 = REPLACE(REPLACE(n1, '-', ' '), "'", ' ');
    SET n2 = REPLACE(REPLACE(n2, '-', ' '), "'", ' ');
    SET n1 = TRIM(REGEXP_REPLACE(n1, '[ ]+', ' '));
    SET n2 = TRIM(REGEXP_REPLACE(n2, '[ ]+', ' '));
    
    -- Si identique après normalisation
    IF n1 = n2 THEN
        RETURN CASE WHEN isFirstName THEN 3 ELSE 2 END;
    END IF;
    
    -- Comparer les premiers mots pour les prénoms
    SET first_word1 = SUBSTRING_INDEX(n1, ' ', 1);
    SET first_word2 = SUBSTRING_INDEX(n2, ' ', 1);

    -- Vérifier si les prénoms sont dans l'ordre
    SET word1 = n1;
    SET word2 = n2;
    WHILE LENGTH(word1) > 0 AND LENGTH(word2) > 0 DO
        SET first_word1 = SUBSTRING_INDEX(word1, ' ', 1);
        SET first_word2 = SUBSTRING_INDEX(word2, ' ', 1);
        IF first_word1 != first_word2 THEN
            SET words_in_order = FALSE;
        END IF;
        IF LOCATE(' ', word1) > 0 THEN
            SET word1 = SUBSTRING(word1, LOCATE(' ', word1) + 1);
        ELSE
            SET word1 = '';
        END IF;
        IF LOCATE(' ', word2) > 0 THEN
            SET word2 = SUBSTRING(word2, LOCATE(' ', word2) + 1);
        ELSE
            SET word2 = '';
        END IF;
    END WHILE;

    -- Compter les mots
    SET word_count1 = (LENGTH(n1) - LENGTH(REPLACE(n1, ' ', '')) + 1);
    SET word_count2 = (LENGTH(n2) - LENGTH(REPLACE(n2, ' ', '')) + 1);
    
    SET n1 = TRIM(n1);
    -- Réinitialiser score pour le comptage des correspondances
    SET matching_words = 0;

    -- Comparer chaque mot
    main_loop: WHILE LENGTH(n1) > 0 DO
        IF LOCATE(' ', n1) > 0 THEN
            SET word1 = SUBSTRING(n1, 1, LOCATE(' ', n1) - 1);
            SET n1 = SUBSTRING(n1, LOCATE(' ', n1) + 1);
        ELSE
            SET word1 = n1;
            SET n1 = '';
        END IF;
        
        SET word2 = n2;
        SET found_match = FALSE;
        
        inner_loop: WHILE LENGTH(word2) > 0 DO
            IF LOCATE(' ', word2) > 0 THEN
                SET curr_word = SUBSTRING(word2, 1, LOCATE(' ', word2) - 1);
                SET word2 = SUBSTRING(word2, LOCATE(' ', word2) + 1);
            ELSE
                SET curr_word = word2;
                SET word2 = '';
            END IF;
            
            IF word1 = curr_word THEN
                SET matching_words = matching_words + 1;
                SET found_match = TRUE;
                LEAVE inner_loop;
            END IF;
        END WHILE;
    END WHILE;

    -- Déterminer le score final
    IF matching_words = 0 THEN 
        RETURN -1;
    ELSEIF isFirstName THEN
        IF SUBSTRING_INDEX(n1, ' ', 1) = SUBSTRING_INDEX(n2, ' ', 1) THEN
            -- Premier prénom identique
            IF words_in_order THEN
                RETURN 2; -- Prénoms supplémentaires dans le même ordre
            ELSE
                RETURN 1; -- Prénoms dans un ordre différent
            END IF;
        ELSE
            RETURN 0; -- Prénoms différents
        END IF;
    END IF;
    
    RETURN matching_words;
END//

CREATE FUNCTION insee.getPlaceLibHistorique(
		inCode CHAR(5),
		inYear CHAR(4),
		inMonth CHAR(2),
		inDay CHAR(2)
) RETURNS VARCHAR(500)
DETERMINISTIC
READS SQL DATA
BEGIN
		DECLARE res VARCHAR(500);
		-- Pas besoin de conversion de date, simple comparaison de chaînes
		DECLARE searchDate CHAR(10);
		SET searchDate = CONCAT(inYear, '-', inMonth, '-', inDay);
		
		IF searchDate < '1943-01-01' THEN
				SELECT Libelle INTO res
				FROM PlaceNorme 
				WHERE Code = inCode
				ORDER BY DateDebut ASC
				LIMIT 1;
		ELSE
				SELECT Libelle INTO res
				FROM PlaceNorme
				WHERE Code = inCode
						AND DateDebut <= searchDate
						AND (DateFin > searchDate OR DateFin IS NULL)
				ORDER BY DateDebut DESC
				LIMIT 1;
		END IF;
		
		RETURN IFNULL(res, '');
END//

CREATE FUNCTION insee.wordcount(
	str LONGTEXT
)
	RETURNS INTEGER
	DETERMINISTIC
	SQL SECURITY INVOKER
	NO SQL
BEGIN
	DECLARE wordCnt, idx, maxIdx INT DEFAULT 0;
	DECLARE currChar, prevChar BOOL DEFAULT 0;
	SET maxIdx=char_length(str);
	SET idx = 1;
	WHILE idx <= maxIdx DO
		SET currChar=SUBSTRING(str, idx, 1) RLIKE '[[:alnum:]]';
		IF NOT prevChar AND currChar THEN
			SET wordCnt=wordCnt+1;
		END IF;
		SET prevChar=currChar;
		SET idx=idx+1;
	END WHILE;
	RETURN wordCnt;
END
//

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
	
	set score = 0;
	set msg = '';

/* Nom */
	SET score = score + insee.compareCompoundNames(tNom, iNom, FALSE);
	IF score < 0 THEN
			SET msg = concat(msg, '\n Nom : ', tNom, ' != ', iNom);
	END IF;

/* Prénom */
SET prenom_score = insee.compareCompoundNames(tPrenom, iPrenom, TRUE);
IF prenom_score >= 2 THEN
    SET score = score + prenom_score;
ELSEIF prenom_score = 1 THEN
    SET score = score + 1;
    SET msg = concat(msg, '\n Prénoms dans un ordre différent : ', tPrenom, ' -> ', iPrenom);
ELSEIF prenom_score = 0 THEN
    SET msg = concat(msg, '\n Prénoms différents : ', tPrenom, ' -> ', iPrenom);
ELSE
    SET score = score - 1;
    SET msg = concat(msg, '\n Prénoms : ', tPrenom, ' != ', iPrenom);
END IF;

/* Lieu de naissance */
	SET placeNaissance = getPlaceLibHistorique(
		iNaissanceCode,
		iNaissanceY,
		iNaissanceM,
		iNaissanceD
);

	IF tNaissancePlace = placeNaissance AND tNaissancePlace != "" THEN
			SET score = score + 1;
	ELSEIF LOCATE(CONCAT(tNaissancePlace, ' ('), placeNaissance) != 0 
					AND tNaissancePlace != "" THEN
			SET score = score + 1;
	ELSEIF LOCATE(tNaissancePlace, placeNaissance) != 0 THEN
			SET msg = CONCAT(msg, '\n Lieu naissance : ', 
											tNaissancePlace, ' -> ', placeNaissance);
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

	/* Date de naissance */
	IF tNaissanceD = iNaissanceD &&
		 tNaissanceM = iNaissanceM &&
		 tNaissanceY = iNaissanceY THEN
		set score = score + 1;
	ELSEIF tNaissanceY <> "0000" && iNaissanceY <> "0000" && abs(tNaissanceY-iNaissanceY) > 5 THEN
		set score = score - 2;
		set msg = concat( msg, '\n Date naissance : ',
			tNaissanceD, '/', tNaissanceM, '/', tNaissanceY, ' !=2 ',
			iNaissanceD, '/', iNaissanceM, '/', iNaissanceY );
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
		IF tNaissanceY = "0000" ||
			 iNaissanceY = "0000" ||
			 tNaissanceY = iNaissanceY THEN
			set scoreTmp = scoreTmp + 1;
		END IF;
		IF scoreTmp > 1 THEN
			set msg = concat( msg, '\n Date naissance : ',
				tNaissanceD, '/', tNaissanceM, '/', tNaissanceY, ' =~ ',
				iNaissanceD, '/', iNaissanceM, '/', iNaissanceY );
		ELSE
			set score = score - 1;
			set msg = concat( msg, '\n Date naissance : ',
				tNaissanceD, '/', tNaissanceM, '/', tNaissanceY, ' != ',
				iNaissanceD, '/', iNaissanceM, '/', iNaissanceY );
		END IF;
	END IF;


/* Lieu de décès */
	SET placeDeces = getPlaceLibHistorique(
    iDecesCode,
    iDecesY,
    iDecesM,
    iDecesD
);

	IF tDecesPlace = placeDeces AND tDecesPlace != "" THEN
			SET score = score + 1;
	ELSEIF LOCATE(CONCAT(tDecesPlace, ' ('), placeDeces) != 0 
					AND tDecesPlace != "" THEN
			SET score = score + 1;
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

	/* Date de décès */
	IF tDecesD = iDecesD &&
		 tDecesM = iDecesM &&
		 tDecesY = iDecesY THEN
		set score = score + 1;
	ELSEIF tDecesY <> "0000" && iDecesY <> "0000" && abs(tDecesY-iDecesY) > 5 THEN
		set score = score - 2;
		set msg = concat( msg, '\n Date décès : ',
			tDecesD, '/', tDecesM, '/', tDecesY, ' !=2 ',
			iDecesD, '/', iDecesM, '/', iDecesY );
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
		IF tDecesY = "0000" ||
			 iDecesY = "0000" ||
			 tDecesY = iDecesY THEN
			set scoreTmp = scoreTmp + 1;
		END IF;
		IF scoreTmp > 1 THEN
			set msg = concat( msg, '\n Date décès : ',
				tDecesD, '/', tDecesM, '/', tDecesY, ' =~ ',
				iDecesD, '/', iDecesM, '/', iDecesY );
		ELSE
			set score = score - 1;
			set msg = concat( msg, '\n Date décès : ',
				tDecesD, '/', tDecesM, '/', tDecesY, ' != ',
				iDecesD, '/', iDecesM, '/', iDecesY );
		END IF;
	END IF;
	
	set msg = concat( msg, '\nInsee (', InitCap(iPrenom), ', acte n<sup>o</sup> ', iNumActe, ')' );
	
	/* Record */
	SET record = CONCAT_WS('|',
			iNom, iPrenom, iSexe,
			CONCAT('°', iNaissanceD, '/', iNaissanceM, '/', iNaissanceY), 
			placeNaissance,
			CONCAT('+', iDecesD, '/', iDecesM, '/', iDecesY), 
			placeDeces,
			iNaissanceCode, iNaissanceLocalite, iNaissancePays, iDecesCode,
			CONCAT('acte n° ', iNumActe)
	);
END//

create procedure insee.processOne(
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
	DECLARE iId INTEGER UNSIGNED;
	DECLARE tPrenom2, iNom, iPrenom VARCHAR(80);
	DECLARE iSexe CHAR(1);
	DECLARE iNaissanceY, iDecesY, cNaisY CHAR(4);
	DECLARE iNaissanceM, iNaissanceD, iDecesM, iDecesD, cNaisD, cNaisM, cDesD, cDesM CHAR(2);
	DECLARE iNaissanceCode, iDecesCode CHAR(5);
	DECLARE iNaissanceLocalite, iNaissancePays VARCHAR(30);
	DECLARE iNaissancePlace, iDecesPlace VARCHAR(500);
	DECLARE iNumActe CHAR(9);
	DECLARE score, nbRows INTEGER;
	DECLARE record, msg VARCHAR(1000);

	DECLARE theEnd INT;
	DECLARE cursorNP CURSOR FOR
		select
		 Id,
		 Nom, Prenom, Sexe,
		 NaissanceD, NaissanceM, NaissanceY,
		 getPlaceLib(NaissanceCode, NaissanceY, NaissanceM, NaissanceD) as NaissancePlace,
		 NaissanceCode,
		 NaissanceLocalite, NaissancePays,
		 DecesD, DecesM, DecesY,
		 getPlaceLib(DecesCode, DecesY, DecesM, DecesD) as DecesPlace,
		 DecesCode,
		 NumeroActe
		from INSEE USE INDEX (idx_nom_prenom)
		where Nom = tNom
			and Prenom like concat('%', tPrenom2, '%')
;
	DECLARE cursorD CURSOR FOR
		select
		 Id,
		 Nom, Prenom, Sexe,
		 NaissanceD, NaissanceM, NaissanceY,
		 getPlaceLib(NaissanceCode, NaissanceY, NaissanceM, NaissanceD) as NaissancePlace,
		 NaissanceCode,
		 NaissanceLocalite, NaissancePays,
		 DecesD, DecesM, DecesY,
		 getPlaceLib(DecesCode, DecesY, DecesM, DecesD) as DecesPlace,
		 DecesCode,
		 NumeroActe
		from INSEE USE INDEX (idx_naissance, idx_deces)
		where NaissanceD like cNaisD
			and NaissanceM like cNaisM
			and NaissanceY like cNaisY
			and DecesD like cDesD
			and DecesM like cDesM
			and DecesY = tDecesY
;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;

-- Créer une table temporaire pour stocker toutes les correspondances potentielles
CREATE TEMPORARY TABLE IF NOT EXISTS temp_matches (
    Id INTEGER UNSIGNED,
    Score INTEGER,
    Record VARCHAR(1000),
    Msg VARCHAR(1000),
    IsBlacklisted BOOLEAN DEFAULT FALSE,
    PRIMARY KEY (Id),
    INDEX idx_score_blacklist (Score, IsBlacklisted)
) ENGINE=InnoDB;

	/* Look for exact match */
	set iId = 0;
	select Id INTO iId
	from INSEE
	where Nom = tNom
		and Prenom = tPrenom
		and Sexe = tSexe
		and NaissanceY = tNaissanceY
		and NaissanceM = tNaissanceM
		and NaissanceD = tNaissanceD
		and getPlaceLib(NaissanceCode, NaissanceY, NaissanceM, NaissanceD) = tNaissancePlace
		and DecesY = tDecesY
		and DecesM = tDecesM
		and DecesD = tDecesD
		and getPlaceLib(DecesCode, DecesY, DecesM, DecesD) = tDecesPlace
	;

	IF iId != 0 THEN
		set etat = 1;
		set nbMatch = 1;
		set bestScore = null;
		set bestId = iId;
		set bestRecord = "";
		set bestMsg = "";
	ELSE

		/* Look for Nom / Prenom */

		set tPrenom2 = replace( tPrenom, '-', '_' );
		set tPrenom2 = replace( tPrenom2, "'", '_' );
		set tPrenom2 = replace( tPrenom2, ' ', '_' );

		set bestScore = -10;
		set nbMatch = 0;
		set bestRecord = "";
		set bestMsg = "";
		set bestId = 0;
		set nbRows = 0;

		OPEN cursorNP;
		b2: LOOP
			set theEnd = false;
			FETCH cursorNP INTO iId,
			 iNom, iPrenom, iSexe,
			 iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
			 iNaissanceLocalite, iNaissancePays,
			 iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;

			IF theEnd THEN
				LEAVE b2;
			END IF;

			set nbRows = nbRows + 1;

			call insee.compare(
				tNom, tPrenom, tSexe,
				tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
				tDecesY, tDecesM, tDecesD, tDecesPlace,
				iId, iNom, iPrenom, iSexe,
				iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
				iNaissanceLocalite, iNaissancePays,
				iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
				score, record, msg);

			IF score >= -5 THEN
					INSERT INTO temp_matches (Id, Score, Record, Msg)
					VALUES (iId, score, record, msg);
			END IF;
		END LOOP;
		CLOSE cursorNP;

-- Vérification continue contre la blacklist pendant les recherches
			CREATE TEMPORARY TABLE IF NOT EXISTS temp_matches (
					Id INTEGER UNSIGNED,
					Score INTEGER,
					Record VARCHAR(1000),
					Msg VARCHAR(1000),
					IsBlacklisted BOOLEAN DEFAULT FALSE,
					PRIMARY KEY (Id)
			) ENGINE=InnoDB;

			-- Après chaque insertion dans temp_matches, ajouter :
						IF @database_name IS NOT NULL THEN
					SET @blacklist_table = CONCAT('blacklist_', @database_name);
					IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = @blacklist_table) THEN
							SET @sql = CONCAT('
									UPDATE temp_matches m
									SET m.IsBlacklisted = 1
									WHERE EXISTS (
											SELECT 1 FROM `', @blacklist_table, '` b
											WHERE b.IdInsee = m.Id
											AND b.TodoKey = ?
									)'
							);
							SET @todoKey = CONCAT(tNom, '|', tPrenom, '|', tSexe, '|',
																	 tNaissanceY, tNaissanceM, tNaissanceD, '|', 
																	 tNaissancePlace, '|',
																	 tDecesY, tDecesM, tDecesD, '|', 
																	 tDecesPlace);
							PREPARE stmt FROM @sql;
							EXECUTE stmt USING @todoKey;
							DEALLOCATE PREPARE stmt;
					END IF;
			END IF;

			/* Sélection du meilleur résultat non blacklisté */
			SET @max_score = (SELECT MAX(Score) FROM temp_matches WHERE NOT IsBlacklisted);

			-- D'abord, on vérifie si le score maximum est suffisant
			IF @max_score >= -2 THEN
					SELECT 
							CASE
									WHEN COUNT(*) = 0 THEN -3 
									WHEN COUNT(*) = 1 AND @max_score >= 4 THEN 
											CASE WHEN @max_score = 6 THEN 1 ELSE 2 END
									WHEN COUNT(*) > 1 AND @max_score >= 4 THEN -2
									ELSE -4
							END,
							COUNT(*),
							@max_score,
							MIN(CASE WHEN Score = @max_score THEN Id END),
							MIN(CASE WHEN Score = @max_score THEN Record END),
							MIN(CASE WHEN Score = @max_score THEN Msg END)
					INTO etat, nbMatch, bestScore, bestId, bestRecord, bestMsg
					FROM temp_matches
					WHERE NOT IsBlacklisted;
			ELSE
					-- Si le score est trop bas, on initialise avec des valeurs par défaut
					SET etat = -4;
					SET nbMatch = 0;
					SET bestScore = @max_score;
					SET bestId = NULL;
					SET bestRecord = '';
					SET bestMsg = '';
			END IF;

			-- Nettoyage
			DROP TEMPORARY TABLE IF EXISTS temp_matches;

		/* Bilan */
		IF bestScore > 1 THEN
			IF nbMatch = 1 THEN
				IF bestMsg = '' THEN
					set etat = 3;
				ELSE
					set etat = 2;
				END IF;
			ELSE
				set etat = -2;
			END IF;
		ELSEIF nbRows = 0 THEN
			IF tDecesY > "1969" THEN

				/* Look for dates */

				IF tNaissanceD = "00" THEN
					set cNaisD = "__";
				ELSE
					set cNaisD = tNaissanceD;
				END IF;
				IF tNaissanceM = "00" THEN
					set cNaisM = "__";
				ELSE
					set cNaisM = tNaissanceM;
				END IF;
				IF tNaissanceY = "0000" THEN
					set cNaisY = "__";
				ELSE
					set cNaisY = tNaissanceY;
				END IF;
				IF tDecesD = "00" THEN
					set cDesD = "__";
				ELSE
					set cDesD = tDecesD;
				END IF;
				IF tDecesM = "00" THEN
					set cDesM = "__";
				ELSE
					set cDesM = tDecesM;
				END IF;

				set bestScore = -10;
				set nbMatch = 0;
				set bestRecord = "";
				set bestMsg = "";
				set bestId = 0;
				set nbRows = 0;

				OPEN cursorD;
				b4: LOOP
					set theEnd = false;
					FETCH cursorD INTO iId,
					 iNom, iPrenom, iSexe,
					 iNaissanceD, iNaissanceM, iNaissanceY, iNaissancePlace, iNaissanceCode,
					 iNaissanceLocalite, iNaissancePays,
					 iDecesD, iDecesM, iDecesY, iDecesPlace, iDecesCode, iNumActe;

					IF theEnd THEN
						LEAVE b4;
					END IF;

					set nbRows = nbRows + 1;

					call insee.compare(
						tNom, tPrenom, tSexe,
						tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
						tDecesY, tDecesM, tDecesD, tDecesPlace,
						iId, iNom, iPrenom, iSexe,
						iNaissanceY, iNaissanceM, iNaissanceD, iNaissancePlace, iNaissanceCode,
						iNaissanceLocalite, iNaissancePays,
						iDecesY, iDecesM, iDecesD, iDecesPlace, iDecesCode, iNumActe,
						score, record, msg);

					IF score > bestScore THEN
						set bestScore = score;
						set bestRecord = record;
						set bestMsg = msg;
						set bestId = iId;
						set nbMatch = 1;
					ELSEIF score = bestScore THEN
						set nbMatch = nbMatch + 1;
					END IF;
				END LOOP;
				CLOSE cursorD;

				/* Nouveau bilan */
				IF bestScore > 1 THEN
					IF nbMatch = 1 THEN
						IF bestMsg = '' THEN
							set etat = 3;
						ELSE
							set etat = 2;
						END IF;
					ELSE
						set etat = -2;
					END IF;
				ELSEIF nbRows = 0 THEN
					set etat = -3;
				ELSE
					set etat = -5;
				END IF;

			ELSE
				set etat = -1;
			END IF;
		ELSE
			set etat = -4;
		END IF;
	END IF;
END//

CREATE PROCEDURE insee.processTodo()
BEGIN
		-- Déclarations pour le traitement des données
		DECLARE tNom, tPrenom VARCHAR(80);
		DECLARE tSexe CHAR(1);
		DECLARE tNaissanceY, tDecesY CHAR(4);
		DECLARE tNaissanceM, tNaissanceD, tDecesM, tDecesD CHAR(2);
		DECLARE tNaissancePlace, tDecesPlace VARCHAR(500);
		DECLARE tCle VARCHAR(100);
		DECLARE myEtat, myNbMatch, myScore INTEGER;
		DECLARE tId, bestId INTEGER UNSIGNED;
		DECLARE myRecord, myMsg VARCHAR(1000);
		DECLARE excluded_count, progress_counter, total_records, elapsed_seconds, 
						estimated_total_seconds, remaining_seconds, last_percentage, theEnd INT DEFAULT 0;
		DECLARE start_time, current_time_var TIMESTAMP;

		-- Définition du curseur
		DECLARE cursorTodo CURSOR FOR
				SELECT
						Id,
						Nom, Prenom, Sexe,
						NaissanceD, NaissanceM, NaissanceY, NaissancePlace,
						DecesD, DecesM, DecesY, DecesPlace,
						Cle
				FROM TODO
				WHERE Etat = 0;
		DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;

		DECLARE EXIT HANDLER FOR SQLEXCEPTION
		BEGIN
				SELECT 
						CONCAT('Arrêt du traitement. Dernière ligne à traiter: ', Id) as Status,
						Id, Nom, Prenom, NaissancePlace
				FROM TODO 
				WHERE Id = (SELECT MIN(Id) FROM TODO WHERE Etat = 0);
		END;

		-- Initialisation du compteur et du timer
		SELECT COUNT(*) INTO total_records FROM TODO WHERE Etat = 0;
		SET start_time = CURRENT_TIMESTAMP;
		
		-- Message de démarrage
		SELECT CONCAT('Début du traitement de ', total_records, ' enregistrements...') as Status;

		-- Traitement principal
		SET theEnd = false;
		OPEN cursorTodo;
		b1: LOOP
				FETCH cursorTodo INTO tId,
						tNom, tPrenom, tSexe,
						tNaissanceD, tNaissanceM, tNaissanceY, tNaissancePlace,
						tDecesD, tDecesM, tDecesY, tDecesPlace, tCle;

				IF theEnd THEN
						LEAVE b1;
				END IF;

				-- Appel de processOne pour le traitement de l'enregistrement
				CALL insee.processOne(
						tNom, tPrenom, tSexe,
						tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
						tDecesY, tDecesM, tDecesD, tDecesPlace, tCle,
						myEtat, myNbMatch, myScore, bestId, myRecord, myMsg);

				-- Mise à jour de l'enregistrement traité
				UPDATE TODO 
				SET Etat = myEtat, 
						NbMatch = myNbMatch, 
						Score = myScore, 
						IdInsee = bestId, 
						Msg = CONCAT(myRecord, myMsg) 
				WHERE Id = tId;

				-- Gestion de la progression
				SET progress_counter = progress_counter + 1;
				SET @current_percentage = FLOOR((progress_counter * 100) / total_records);
				
				-- Affichage tous les 5% de progression
				IF @current_percentage >= last_percentage + 5 THEN
						SET current_time_var = CURRENT_TIMESTAMP;
						SET elapsed_seconds = TIMESTAMPDIFF(SECOND, start_time, current_time_var);
						
						IF elapsed_seconds > 0 THEN
								SET estimated_total_seconds = (elapsed_seconds * total_records) / progress_counter;
								SET remaining_seconds = estimated_total_seconds - elapsed_seconds;
								
								SELECT CONCAT(
								@current_percentage, ' % (', progress_counter, '/', total_records, 
								' — ~ ', ROUND(progress_counter - (last_percentage * total_records / 100)), ')
								' en ', FLOOR(elapsed_seconds/60), ' min',
								' — Temps restant estimé : ', FLOOR(remaining_seconds/60), ' min'
						) as Progress;
						END IF;
						
						SET last_percentage = @current_percentage - MOD(@current_percentage, 5);
				END IF;
		END LOOP;
		CLOSE cursorTodo;

		-- Gestion de la blacklist (si activée)
		IF @database_name IS NOT NULL THEN
				SET @blacklist_table = CONCAT('blacklist_', @database_name);
				IF EXISTS (
						SELECT 1 
						FROM information_schema.tables 
						WHERE table_name = @blacklist_table
				) THEN
						-- Code existant pour la blacklist...
						SET @sql = CONCAT('
								SELECT COUNT(*) INTO @excluded_count
								FROM TODO t 
								INNER JOIN `', @blacklist_table, '` b
										ON b.IdInsee = t.IdInsee
										AND b.TodoKey = CONCAT(t.Nom, "|", t.Prenom, "|", t.Sexe, "|",
																				 t.NaissanceY, t.NaissanceM, t.NaissanceD, "|", 
																				 t.NaissancePlace, "|",
																				 t.DecesY, t.DecesM, t.DecesD, "|", 
																				 t.DecesPlace)
						');
						PREPARE stmt FROM @sql;
						EXECUTE stmt;
						DEALLOCATE PREPARE stmt;

						IF @excluded_count > 0 THEN
								SET @sql = CONCAT('
										DELETE t FROM TODO t 
										INNER JOIN `', @blacklist_table, '` b
												ON b.IdInsee = t.IdInsee
												AND b.TodoKey = CONCAT(t.Nom, "|", t.Prenom, "|", t.Sexe, "|",
																						 t.NaissanceY, t.NaissanceM, t.NaissanceD, "|", 
																						 t.NaissancePlace, "|",
																						 t.DecesY, t.DecesM, t.DecesD, "|", 
																						 t.DecesPlace)
								');
								PREPARE stmt FROM @sql;
								EXECUTE stmt;
								DEALLOCATE PREPARE stmt;

								SELECT CONCAT(@excluded_count, ' entrées trouvées dans la table blacklist-', @database_name, ' ont été exclues');
						END IF;
				END IF;
		END IF;

		-- Affichage du résumé final
		SET current_time_var = CURRENT_TIMESTAMP;
		SET elapsed_seconds = TIMESTAMPDIFF(SECOND, start_time, current_time_var);
		SELECT CONCAT(
				'Traitement terminé - Durée totale: ',
				FLOOR(elapsed_seconds/60), 'm ', MOD(elapsed_seconds,60), 's'
		) as Summary;
END//
delimiter ;
