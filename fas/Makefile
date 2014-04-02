## For use with port
INCS=-I/opt/local/include
LIBS=-L/opt/local/lib -lgc

## No user-serviceable parts beyond this point. ##

BOOT_CFLAGS=${INCS} -Wall -O3
BOOT_LDFLAGS=${LIBS}

CFLAGS=${INCS}  -Wall -O6 -DMEMORY_BASE_CODE
LDFLAGS=${LIBS} -lgc


all: fas

fas: main.o fas.tab.o fas.lex.o
	${CC} ${CFLAGS} ${LDFLAGS} -o $@ $^

main.o: main.c fas.h

%.tab.c: %.y
	bison -d $<

%.lex.c: %.l
	flex -o$@ $<

clean:
	rm -f *.tab.* *.lex.* *.o fas
