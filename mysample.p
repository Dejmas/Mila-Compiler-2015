Program sieve_of_eratosthenes;

var
    pole : array [ 2 .. 100 ] of integer;
    i, j, k, b : integer;
begin
	writeln( 'Pocitani Eratosthenesova sita...' )
    for i := 2 to 100 do 
    begin
    	pole[ i ] := 1;
    end;
    for i := 2 to 100 do
    begin
    	if pole[ i ] = 1 then
    	begin 
	    	b := 100 div i;
	    	for j := 2 to b do
	    		pole[ j*i ] := 0;  
    	end;
    end;
    writeln( 'Prvocisla:' );
    for i := 2 to 100 do
    begin
    	if pole[ i ] then
    		writeln( i );
    end;
end.

