VPATH = $(addprefix $(dir $(MAKEFILE_LIST)), cgde cpar ../prob/code)

LISP = ./ccl

CGDE = load custom hdr records defname toploop comment command order flags printer syntax files alias realias lpexp metaexp compile dictint dtree parse genrate semantics defns view names invalid graminp gramdel gramed gramove #+gde-morph ("dictfns" "dpatch")
CPAR = load hdr gindex unify parse
PROB = load hdr tools readcirc install in-text ask-stats dparse partial vphrasal vsubcat out-analysis gramrel extract-grs xml-output

# The following line does not distinguish between the different
# load.lsp, hdr.lsp and parse.lsp files, ignoring the cpar and prob ones
gde: $(addsuffix .lsp, $(CGDE) $(CPAR) $(PROB))
	$(LISP) \
	-e '(load "$(addprefix $(dir $(MAKEFILE_LIST)),cgde/load.lsp)")' \
	-e '(load "$(addprefix $(dir $(MAKEFILE_LIST)),cpar/load.lsp)")' \
	-e '(load "$(addprefix $(dir $(MAKEFILE_LIST)),../prob/code/load.lsp)")' \
	-e '(save-gde-image "gde" :error-handler :quit :prepend-kernel t)' \
	-e '(quit)'

