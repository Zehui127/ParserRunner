VPATH = $(dir $(MAKEFILE_LIST)) $(addprefix $(dir $(MAKEFILE_LIST)), src)

# Makefile for labeller (standard version)
# Does not include some of the minor utilities.

CC = gcc
CCOPT = -g -ansi -W -Wall -DNo_Phrase -c
LINK = gcc
LINKOPT = -g
LINKLIB = 
LIB = ar
LIBOPT = rc
DOLINK = $(LINK) $(LINKOPT) -o $@
LABELLIB = labellib.a
LIBDB = libdb.a -lpthread

# Remove object files and labellib.a after compilation:
.INTERMEDIATE: analyse.o common.o diction.o io.o label.o list.o low.o mainl.o map.o stack.o trans.o unkcommon.o unkdiction.o labellib.a

# This is a suffix rule 
#.c.o: 
%.o: %.c
	$(CC) $(CCOPT) $<

# Main target
all:  	cmptran conversion dephrase dmerge dtinfo exdict label outcomp readtr transtr rules

# Library
labellib.a: common.o unkcommon.o diction.o unkdiction.o io.o list.o low.o map.o trans.o
	$(LIB) $(LIBOPT) $@ $?

# Targets
cmptran: cmptran.o $(LABELLIB)
	$(DOLINK) cmptran.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
conversion: conversion.o $(LABELLIB)
	$(DOLINK) conversion.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
dephrase: dephrase.o $(LABELLIB)
	$(DOLINK) dephrase.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
dmerge: dmerge.o $(LABELLIB)
	$(DOLINK) dmerge.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
dtinfo: dtinfo.o $(LABELLIB)
	$(DOLINK) dtinfo.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
exdict: exdict.o $(LABELLIB)
	$(DOLINK) exdict.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
label: analyse.o mainl.o label.o stack.o  $(LABELLIB)
	$(DOLINK) analyse.o mainl.o label.o stack.o  $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
outcomp: outcomp.o $(LABELLIB)
	$(DOLINK) outcomp.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
readtr: readtr.o $(LABELLIB)
	$(DOLINK) readtr.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
transtr: transtr.o $(LABELLIB)
	$(DOLINK) transtr.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm
rules: rules.o $(LABELLIB)
	$(DOLINK) rules.o $(LABELLIB) $(LINKLIB) $(LIBDB) -lm

# For cleanup purposes
clean:	rm_objects rm_executables

rm_objects:
	rm -f analyse.o common.o conversion.o unkcommon.o diction.o unkdiction.o io.o list.o low.o map.o trans.o label.o mainl.o stack.o cmptran.o dephrase.o dmerge.o dtinfo.o exdict.o outcomp.o readtr.o transtr.o rules.o

# For cleanup purposes
rm_executables:
	rm -f cmptran  conversion  dephrase  dmerge  dtinfo  exdict  label  outcomp  readtr  rules  transtr

# c -> o
analyse.o: analyse.c common.h map.h diction.h label.h analyse.h
cmptran.o: cmptran.c common.h trans.h map.h
common.o: common.c common.h
conversion.o: conversion.c
unkcommon.o: unkcommon.c common.h diction.h unkcommon.h unkdiction.h \
 map.h
dephrase.o: dephrase.c common.h map.h
diction.o: diction.c common.h diction.h map.h
unkdiction.o: unkdiction.c common.h diction.h unkcommon.h unkdiction.h \
 map.h
dmerge.o: dmerge.c common.h map.h diction.h trans.h
dtinfo.o: dtinfo.c common.h map.h diction.h trans.h
exdict.o: exdict.c common.h diction.h trans.h map.h
io.o: io.c common.h diction.h low.h map.h label.h stack.h unkcommon.h \
 unkdiction.h analyse.h
label.o: label.c common.h diction.h trans.h label.h map.h stack.h \
 analyse.h unkcommon.h
list.o: list.c common.h list.h
low.o: low.c common.h low.h map.h
mainl.o: mainl.c common.h diction.h trans.h label.h map.h low.h unkcommon.h \
 unkdiction.h analyse.h
map.o: map.c common.h list.h map.h
outcomp.o: outcomp.c
readtr.o: readtr.c common.h trans.h map.h
transtr.o: transtr.c common.h trans.h map.h
rules.o: rules.c common.h map.h low.h
stack.o: stack.c common.h map.h diction.h label.h stack.h unkcommon.h \
 unkdiction.h list.h
trans.o: trans.c common.h trans.h map.h
unkcommon.o: unkcommon.c common.h diction.h unkcommon.h unkdiction.h map.h
unkdiction.o: unkdiction.c common.h diction.h unkcommon.h unkdiction.h map.h
