Program circle;

var 
	Y, X, n, sx, sy : integer;
begin
	writeln( 'enter radius' );
	readln( n );
	writeln( 'enter x coordinate of center.' );
	readln( sx );
	writeln( 'enter y coordinate of center.' );
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