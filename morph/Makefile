HERE = $(dir $(MAKEFILE_LIST))
VPATH = $(HERE) $(addprefix $(HERE), ../enc)

SHELL=/bin/sh
CC=gcc

# Note that FLEX must point to the 2.5.4a version of flex recompiled with
# larger internal limits -- see the README file

FLEX = ./flex

#
# Flex options -Cfe -8 give good speed.
# But for use via pipes inside other programs don't give flex any options.
#
#FLEXOPT = -Cfe -8
FLEXOPT =
CCOPT = -O

#.SECONDARY: # %.yy.c does not work with pattern rule
.PRECIOUS: %.yy.c

%.yy.c: charclasses.flex %.lex
	$(FLEX) -i $(FLEXOPT) -o$@ $^


%.clean:
	/bin/rm -f $*.yy.c

%: $(HERE)%.yy.c
	$(CC) $(CCOPT) -o $@ $< 
	strip $@

