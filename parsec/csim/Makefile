all: Calc

Tokens.hs : Tokens.x
	alex Tokens.x

Grammar.hs : Grammar.y
	happy Grammar.y
    
Calc : Tokens.hs Grammar.hs Calc.hs Translate.hs
	ghc --make Calc -odir obj/
    
clean:
	rm -f Calc Grammar.hs Tokens.hs *.o *.hi
	rm -f obj/*

test:Calc
	./Calc test

png:Calc
	./Calc test > a.dot
	dot a.dot -T png > a.png
    

io:io.hs
	ghc io.hs