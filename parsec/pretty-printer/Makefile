all: Calc

Parser/Tokens.hs : Parser/Tokens.x
	alex Parser/Tokens.x

Parser/Grammar.hs : Parser/Grammar.y
	happy Parser/Grammar.y

Calc : Parser/Tokens.hs Parser/Grammar.hs Calc.hs Translator/Translate.hs
	ghc --make Calc

clean:
	rm -f Calc Grammar.hs Tokens.hs *.o *.hi

test:Calc
	./Calc test


io:io.hs
	ghc io.hs