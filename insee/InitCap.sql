DROP FUNCTION IF EXISTS insee.InitCap;

DELIMITER //
CREATE FUNCTION insee.InitCap( x char(80) )
RETURNS char(80)
BEGIN
	SET @str = '';
	SET @l_str = '';
	WHILE x REGEXP '[- ]' DO
		select locate(' ', x) into @pos1;
		select locate('-', x) into @pos2;
		if @pos1 = 0 then
			set @pos = @pos2;
			set @sep = '-';
		elseif @pos2 = 0 then
			set @pos = @pos1;
			set @sep = ' ';
		elseif @pos1 < @pos2 then
			set @pos = @pos1;
			set @sep = ' ';
		else
			set @pos = @pos2;
			set @sep = '-';
		end if;
		SELECT SUBSTRING(x, 1, @pos-1) INTO @l_str;
		SELECT SUBSTRING(x, @pos+1) INTO x;
		SELECT CONCAT(@str, UPPER(SUBSTRING(@l_str,1,1)), LOWER(SUBSTRING(@l_str,2)), @sep) INTO @str;
	END WHILE;
	RETURN CONCAT(@str, UPPER(SUBSTRING(x,1,1)), LOWER(SUBSTRING(x,2)));
END//
DELIMITER ;
