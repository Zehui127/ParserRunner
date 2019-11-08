HERE = $(dir $(MAKEFILE_LIST))
VPATH = $(HERE) $(addprefix $(HERE), ../enc)

SHELL=/bin/sh
CC=gcc
FLEX = ./flex

CCOPT = -O
FLEXOPT =

.PRECIOUS: %.yy.c

%.yy.c: charclasses.flex %.flex
	$(FLEX) $(FLEXOPT) -o$@ $^

%.clean:
	/bin/rm -f $*.yy.c

%: $(HERE)%.yy.c
	$(CC) $(CCOPT) -o $@ $<
	strip $@
