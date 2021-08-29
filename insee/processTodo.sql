drop procedure if exists insee.processTodo;
drop procedure if exists insee.processOne;
drop procedure if exists insee.removeDuplicate;
drop procedure if exists insee.compare;
drop function if exists insee.wordcount;

delimiter //

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

create procedure insee.removeDuplicate(
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
	IN tDecesPlace VARCHAR(500)
)
BEGIN
	DECLARE IdDel INTEGER UNSIGNED;

	select t1.Id INTO IdDel
	from INSEE t1, INSEE t2
	where t1.Id > t2.Id
	  and t1.Nom = t2.Nom
	  and t1.Prenom = t2.Prenom
	  and t1.Sexe = t2.Sexe
	  and t1.NaissanceY = t2.NaissanceY
	  and t1.NaissanceM = t2.NaissanceM
	  and t1.NaissanceD = t2.NaissanceD
	  and t1.NaissanceCode = t2.NaissanceCode
	  and t1.NaissanceLocalite = t2.NaissanceLocalite
	  and t1.NaissancePays = t2.NaissancePays
	  and t1.DecesY = t2.DecesY
	  and t1.DecesM = t2.DecesM
	  and t1.DecesD = t2.DecesD
	  and t1.DecesCode = t2.DecesCode
	  -- and t1.NumeroActe = t2.NumeroActe
	  and t1.Id in (
		/* Exact Match TODO -> INSEE */
		select i.Id
		from INSEE i
		where i.Nom = tNom
		  and i.Prenom = tPrenom
		  and i.Sexe = tSexe
		  and i.NaissanceY = tNaissanceY
		  and i.NaissanceM = tNaissanceM
		  and i.NaissanceD = tNaissanceD
		  and getPlaceLib(i.NaissanceCode, i.NaissanceY, i.NaissanceM, i.NaissanceD) = tNaissancePlace
		  and i.DecesY = tDecesY
		  and i.DecesM = tDecesM
		  and i.DecesD = tDecesD
		  and getPlaceLib(i.DecesCode, i.DecesY, i.DecesM, i.DecesD) = tDecesPlace
	) limit 1;
	delete from INSEE where Id = IdDel;
	select 'WARNING', 'Removed duplicate entry in INSEE', IdDel;
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

	set score = 0;
	set msg = '';

	/* Nom */
	set iNom2 = replace( iNom, '-', ' ');
	set iNom2 = replace( iNom2, "'", ' ');
	set tNom2 = replace( tNom, '-', ' ');
	set tNom2 = replace( tNom2, "'", ' ');
	IF tNom2 = iNom2 THEN
		set score = score + 1;
	ELSE
		set score = score - 1;
		set msg = concat ( msg, '\n Nom : ', tNom, ' != ', iNom );
	END IF;

	/* Prénom */
	set iPrenom2 = replace( iPrenom, '-', ' ');
	set iPrenom2 = replace( iPrenom2, "'", ' ');
	set tPrenom2 = replace( tPrenom, '-', ' ');
	set tPrenom2 = replace( tPrenom2, "'", ' ');
	IF tPrenom2 = iPrenom2 THEN
		IF insee.wordcount( tPrenom2 ) >= 3 THEN
			set score = score + 2;
		ELSE
			set score = score + 1;
		END IF;
	ELSEIF locate( tPrenom2, iPrenom2 ) != 0 THEN
		set msg = concat ( msg, '\n Prénom : ', tPrenom, ' -> ', iPrenom );
	ELSE
		set score = score - 1;
		set msg = concat ( msg, '\n Prénom : ', tPrenom, ' != ', iPrenom );
	END IF;

	/* Sexe */
	IF tSexe != iSexe THEN
		set score = score - 1;
		set msg = concat( msg, '\n', ' Sexe différent' );
	END IF;

	/* Lieu de naissance */
	IF tNaissancePlace = iNaissancePlace && tNaissancePlace != "" THEN
		set score = score + 1;
	ELSEIF locate( tNaissancePlace, iNaissancePlace ) != 0 THEN
		set msg = concat( msg, '\n Lieu naissance : ', tNaissancePlace, ' -> ', iNaissancePlace );
	ELSEIF locate( iNaissancePlace, tNaissancePlace ) != 0 THEN
		IF substring(iNaissanceCode, 1, 2) != '99' THEN
			set msg = concat( msg, '\n Lieu naissance : ', tNaissancePlace, ' =~ ', iNaissancePlace );
		END IF;
	ELSE
		set score = score - 1;
		set msg = concat( msg, '\n Lieu naissance : ', tNaissancePlace, ' != ', iNaissancePlace );
	END IF;

	/* Date de naissance */
	IF tNaissanceD = iNaissanceD &&
	   tNaissanceM = iNaissanceM &&
	   tNaissanceY = iNaissanceY THEN
		set score = score + 1;
	ELSEIF tNaissanceY <> "0000" && iNaissanceY <> "0000" && abs(tNaissanceY-iNaissanceY) > 2 THEN
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
	IF tDecesPlace = iDecesPlace && tDecesPlace != "" THEN
		set score = score + 1;
	ELSEIF locate( tDecesPlace, iDecesPlace ) != 0 THEN
		set msg = concat( msg, '\n Lieu décès : ', tDecesPlace, ' -> ', iDecesPlace );
	ELSEIF locate( iDecesPlace, tDecesPlace ) != 0 THEN
		IF substring(iDecesCode, 1, 2) != '99' THEN
			set msg = concat( msg, '\n Lieu décès : ', tDecesPlace, ' =~ ', iDecesPlace );
		END IF;
	ELSE
		set score = score - 1;
		set msg = concat( msg, '\n Lieu décès : ', tDecesPlace, ' != ', iDecesPlace );
	END IF;

	/* Date de décès */
	IF tDecesD = iDecesD &&
	   tDecesM = iDecesM &&
	   tDecesY = iDecesY THEN
		set score = score + 1;
	ELSEIF tDecesY <> "0000" && iDecesY <> "0000" && abs(tDecesY-iDecesY) > 2 THEN
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

	/* Record */
	set record = concat_ws( '|',
		iNom, iPrenom, iSexe,
		concat( '°', iNaissanceD, '/', iNaissanceM, '/', iNaissanceY ), iNaissancePlace,
		concat( '+', iDecesD, '/', iDecesM, '/', iDecesY ), iDecesPlace,
		iNaissanceCode, iNaissanceLocalite, iNaissancePays, iDecesCode,
		concat( 'acte n°', iNumActe )
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
		 getPlaceLib( NaissanceCode, NaissanceY, NaissanceM, NaissanceD ), NaissanceCode,
		 NaissanceLocalite, NaissancePays,
		 DecesD, DecesM, DecesY,
		 getPlaceLib( DecesCode, DecesY, DecesM, DecesD ), DecesCode,
		 NumeroActe
		from INSEE
		where Nom = tNom
		  and Prenom like concat('%', tPrenom2, '%')
	;
	DECLARE cursorD CURSOR FOR
		select
		 Id,
		 Nom, Prenom, Sexe,
		 NaissanceD, NaissanceM, NaissanceY,
		 getPlaceLib( NaissanceCode, NaissanceY, NaissanceM, NaissanceD ), NaissanceCode,
		 NaissanceLocalite, NaissancePays,
		 DecesD, DecesM, DecesY,
		 getPlaceLib( DecesCode, DecesY, DecesM, DecesD ), DecesCode,
		 NumeroActe
		from INSEE
		where NaissanceD like cNaisD
		  and NaissanceM like cNaisM
		  and NaissanceY like cNaisY
		  and DecesD like cDesD
		  and DecesM like cDesM
		  and DecesY = tDecesY
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;
	DECLARE CONTINUE HANDLER FOR 1172 BEGIN
		call insee.removeDuplicate( tNom, tPrenom, tSexe,
			tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
			tDecesY, tDecesM, tDecesD, tDecesPlace );
	END;

	/* Look for extact match */
	set iId = 0;
	select Id INTO iId
	from INSEE
	where Nom = tNom
	  and Prenom = tPrenom
	  and Sexe = tSexe
	  and NaissanceY = tNaissanceY
	  and NaissanceM = tNaissanceM
	  and NaissanceD = tNaissanceD
	  and getPlaceLib(NaissanceCode,NaissanceY,NaissanceM,NaissanceD) = tNaissancePlace
	  and DecesY = tDecesY
	  and DecesM = tDecesM
	  and DecesD = tDecesD
	  and getPlaceLib(DecesCode,DecesY,DecesM,DecesD) = tDecesPlace
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

			IF score > bestScore THEN
				set bestScore = score;
				set bestRecord = record;
				set bestMsg = msg;
				set bestId = iId;
				set nbMatch = 1;
			ELSEIF score = bestScore THEN
				IF record = bestRecord THEN
					/* Un doublon ! */
					delete from INSEE where Id = iId;
					select 'WARNING', 'Removed duplicate entry in INSEE', iId;
				ELSE
					IF score > 3 THEN
						IF nbMatch = 1 THEN
							set bestMsg = concat( bestMsg, '\n Doublons INSEE ? : ', bestId );
						END IF;
						set bestMsg = concat( bestMsg, ', ', iId );
					END IF;
					set nbMatch = nbMatch + 1;
				END IF;
			END IF;
		END LOOP;
		CLOSE cursorNP;

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
						IF record = bestRecord THEN
							/* Un doublon ! */
							delete from INSEE where Id = iId;
							select 'WARNING', 'Removed duplicate entry in INSEE', iId;
						ELSE
							IF score > 3 THEN
								IF nbMatch = 1 THEN
									set bestMsg = concat( bestMsg, '\n Doublons INSEE ? : ', bestId );
								END IF;
								set bestMsg = concat( bestMsg, ', ', iId );
							END IF;
							set nbMatch = nbMatch + 1;
						END IF;
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

create procedure insee.processTodo()
BEGIN
	DECLARE tNom, tPrenom VARCHAR(80);
	DECLARE tSexe CHAR(1);
	DECLARE tNaissanceY, tDecesY CHAR(4);
	DECLARE tNaissanceM, tNaissanceD, tDecesM, tDecesD CHAR(2);
	DECLARE tNaissancePlace, tDecesPlace VARCHAR(500);
	DECLARE tCle VARCHAR(100);
	DECLARE myEtat, myNbMatch, myScore INTEGER;
	DECLARE tId, bestId INTEGER UNSIGNED;
	DECLARE myRecord, myMsg VARCHAR(1000);

	DECLARE theEnd INT;
	DECLARE cursorTodo CURSOR FOR
		select
		 Id,
		 Nom, Prenom, Sexe,
		 NaissanceD, NaissanceM, NaissanceY, NaissancePlace,
		 DecesD, DecesM, DecesY, DecesPlace,
		 Cle
		from TODO
		where Etat = 0
	;
	DECLARE CONTINUE HANDLER FOR NOT FOUND SET theEnd = TRUE;

	set theEnd = false;
	OPEN cursorTodo;
	b1: LOOP
		FETCH cursorTodo INTO tId,
		 tNom, tPrenom, tSexe,
		 tNaissanceD, tNaissanceM, tNaissanceY, tNaissancePlace,
		 tDecesD, tDecesM, tDecesY, tDecesPlace, tCle;

		IF theEnd THEN
			LEAVE b1;
		END IF;

		call insee.processOne(
			tNom, tPrenom, tSexe,
			tNaissanceY, tNaissanceM, tNaissanceD, tNaissancePlace,
			tDecesY, tDecesM, tDecesD, tDecesPlace, tCle,
			myEtat, myNbMatch, myScore, bestId, myRecord, myMsg );

		/* Store result */
		update TODO set Etat = myEtat, NbMatch = myNbMatch, Score = myScore, IdInsee = bestId, Msg = concat( myRecord, myMsg ) where Id = tId;

	END LOOP;
	CLOSE cursorTodo;
END//
delimiter ;
