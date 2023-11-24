all:
	bnfc KOTLIN.cf
	happy -gca ParKOTLIN.y
	alex -g LexKOTLIN.x 
	stack build
	echo '#!/bin/bash\nstack exec compiler $$1' > compiler
	chmod a+x compiler

build: all
	-rm -f DocKOTLIN.* LexKOTLIN.* ParKOTLIN.* LayoutKOTLIN.* SkelKOTLIN.* PrintKOTLIN.* TestKOTLIN.* AbsKOTLIN.* TestKOTLIN ErrM.* SharedString.* KOTLIN.dtd XMLKOTLIN.*

test: build
	chmod a+x test.sh
	./test.sh

clean:
	-rm -f compiler
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocKOTLIN.ps DocKOTLIN.txt compiler
	stack clean

distclean: clean
	-rm -f DocKOTLIN.* LexKOTLIN.* ParKOTLIN.* LayoutKOTLIN.* SkelKOTLIN.* PrintKOTLIN.* TestKOTLIN.* AbsKOTLIN.* TestKOTLIN ErrM.* SharedString.* KOTLIN.dtd XMLKOTLIN.*
	stack clean --full

