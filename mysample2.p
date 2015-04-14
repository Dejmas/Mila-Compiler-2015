Program kolecka;

var 
	Y, X, n, sx, sy : integer;
begin
	writeln( 'zadejte radius' );
	readln( n );
	writeln( 'zadejte x-sovou souradnici stredu.' );
	readln( sx );
	writeln( 'zadejte y-sovou souradnici stredu.' );
	readln( sy );

	for Y := 0 to 30 do
	begin
		for X := 0 to 50 do
		begin
			if (sy - Y)*(sy - Y) + (sx - X)*(sx - X) <= n*n then
			 begin
				write( '*' );
			 end
			else
			 begin
				write( ' ' );
			 end;
		end;
		writeln( ' ' );
	end;
end.