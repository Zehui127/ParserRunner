ARCH = $(shell uname -m | sed "s/i.86/ix86/")_$(shell uname -s | tr "[:upper:]" "[:lower:]")
ifeq ($(ARCH), ix86_darwin)
   ifeq ($(shell sysctl -n hw.optional.x86_64), 1)
    ARCH = x86_64_darwin
  endif
endif
ifneq (,$(findstring darwin,$(ARCH)))
  SED_EXT_RE=E
else
  SED_EXT_RE=r
endif

BINS = aux sentence token label morpha gde

.PHONY: $(BINS)
all: $(BINS)

clean:
	rm -rf bin/$(ARCH)

$(BINS) $(addprefix bin/$(ARCH)/,flex libdb.a ccl ccl.image): | bin bin/$(ARCH)

bin:
	mkdir bin

bin/$(ARCH):
	mkdir bin/$(ARCH)

sentence:
	$(MAKE) -C bin/$(ARCH) -f ../../sentence/Makefile sentence

token:
	$(MAKE) -C bin/$(ARCH) -f ../../tokenise/Makefile token

label:
	$(MAKE) -C bin/$(ARCH) -f ../../tag/Makefile label

morpha:
	$(MAKE) -C bin/$(ARCH) -f ../../morph/Makefile morpha

gde:
	$(MAKE) -C bin/$(ARCH) -f ../../gde/Makefile gde

# $(CURDIR) might make more sense than ../..

# Note:  Nothing will happen if ../../sentence/sentence exists and is up to date
#        and there is no /bin/$(ARCH)/sentence.
#        Should the binary file be moved in that case?


# Auxiliary libraries and programs needed to compile RASP.

aux: $(addprefix bin/$(ARCH)/,flex libdb.a ccl ccl.image)

bin/flex-2.5.4a.tar.gz: | bin
	curl https://www.mirrorservice.org/sites/ftp.slackware.com/pub/slackware/slackware-7.1/source/d/flex/flex-2.5.4a.tar.gz -Ro $@

bin/bison-1.26a.tar.gz: | bin
	curl http://ftp.gnu.org/gnu/bison/bison-1.26a.tar.gz -Ro $@

bin/db-3.1.17.tar.gz: | bin
	curl https://download.oracle.com/berkeley-db/db-3.1.17.tar.gz -Ro $@

bin/$(ARCH)/flex: bin/flex-2.5.4a.tar.gz bin/bison-1.26a.tar.gz
	cd bin/$(ARCH); tar xzf ../flex-2.5.4a.tar.gz; tar xzf ../bison-1.26a.tar.gz
	sed -$(SED_EXT_RE) -i.orig -e '/#define (JAMSTATE|MAXIMUM_MNS|BAD_SUBSCRIPT)/ s/[0-9]+/800000/' bin/$(ARCH)/flex-2.5.4/flexdef.h
	cd bin/$(ARCH)/bison-1.26; ./configure; make
	cd bin/$(ARCH)/flex-2.5.4; YACC="BISON_SIMPLE=../bison-1.26/bison.simple ../bison-1.26/bison -y" ./configure; make
	mv bin/$(ARCH)/flex-2.5.4/flex $@

bin/$(ARCH)/libdb.a: bin/db-3.1.17.tar.gz
	cd bin/$(ARCH); tar xzf ../db-3.1.17.tar.gz
	cd bin/$(ARCH)/db-3.1.17/build_unix; ../dist/configure; make
	mv bin/$(ARCH)/db-3.1.17/build_unix/libdb.a $@

CCLVERSION = 1.11.5
CCLPREFIX = ccl-$(CCLVERSION)-
CCLARCHDIR = --strip-components 1 ccl/
ifeq ($(ARCH), ix86_linux)
  CCL = lx86cl
  CCLARCH = linuxx86
else ifeq ($(ARCH), x86_64_linux)
  CCL = lx86cl64
  CCLARCH = linuxx86
else ifeq ($(ARCH), ix86_darwin)
  CCL = dx86cl
  CCLARCH = darwinx86
else ifeq ($(ARCH), x86_64_darwin)
  CCL = dx86cl64
  CCLARCH = darwinx86
  # Modified version required for Mojave for the time being,
  # check for macOS versions 14--19:
  ifeq ($(shell defaults read loginwindow SystemVersionStampAsString | grep -c '^10\.1[4-9]'), 1)
    CCLVERSION = 1.12-dev.4
    CCLPREFIX =
    CCLARCHDIR =
  endif
else
$(error Unknown architecture $(ARCH))
endif

bin/$(ARCH)/ccl: bin/ccl-$(CCLVERSION)-$(CCLARCH).tar.gz
	cd bin/$(ARCH); tar xmf ../$(notdir $<) $(CCLARCHDIR)$(CCL); mv $(CCL) ccl

bin/$(ARCH)/ccl.image: bin/ccl-$(CCLVERSION)-$(CCLARCH).tar.gz
	cd bin/$(ARCH); tar xmf ../$(notdir $<) $(CCLARCHDIR)$(CCL).image; mv $(CCL).image ccl.image

bin/ccl-$(CCLVERSION)-$(CCLARCH).tar.gz: | bin
	curl https://github.com/Clozure/ccl/releases/download/v$(CCLVERSION)/$(CCLPREFIX)$(CCLARCH).tar.gz -LRo $@

