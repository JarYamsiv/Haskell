all: Calc

parser/Tokens.hs : parser/Tokens.x
	alex parser/Tokens.x

parser/Grammar.hs : parser/Grammar.y
	happy parser/Grammar.y

Calc : parser/Tokens.hs parser/Grammar.hs Calc.hs
	ghc --make Calc

clean:
	rm -f Calc Grammar.hs Tokens.hs *.o *.hi

test:Calc
	./Calc test

io:io.hs
	ghc io.hs