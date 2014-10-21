#################################################################################
#                Ojs-base                                                       #
#                                                                               #
#    Copyright (C) 2014 INRIA. All rights reserved.                             #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include ../master.Makefile

P=#p -p
PBYTE=#p -p a

OF_FLAGS=-package $(PACKAGES)

COMPFLAGS=-I +ocamldoc -I example -annot -g -thread -verbose #-w +K
OCAMLPP=

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

# Base

LIB=ojs-base.cmxa
LIB_A=$(LIB:.cmxa=.a)
LIB_CMXS=$(LIB:.cmxs)
LIB_BYTE=$(LIB:.cmxa=.cma)

LIB_CMXFILES=ojs_misc.cmx ojs_server.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

LIBJS=ojs-base-js.cma

LIBJS_CMOFILES=	ojs_js.cmo ojsmsg_js.cmo

LIBJS_CMIFILES=$(LIBJS_CMOFILES:.cmo=.cmi)

# Filetree

LIBFT=ojs-filetree.cmxa
LIBFT_A=$(LIBFT:.cmxa=.a)
LIBFT_CMXS=$(LIBFT:.cmxs)
LIBFT_BYTE=$(LIBFT:.cmxa=.cma)

LIBFT_CMXFILES=ojsft_types.cmx \
	ojsft_find.cmx \
	ojsft_files.cmx \
	ojsft_server.cmx

LIBFT_CMOFILES=$(LIBFT_CMXFILES:.cmx=.cmo)
LIBFT_CMIFILES=$(LIBFT_CMXFILES:.cmx=.cmi)
LIBFT_OFILES=$(LIBFT_CMXFILES:.cmx=.o)

LIBFTJS=ojs-filetree-js.cma

LIBFTJS_CMOFILES=ojsft_types.cmo \
	ojsft_js.cmo

LIBFTJS_CMIFILES=$(LIBFTJS_CMOFILES:.cmx=.cmi)

# Editor

LIBED=ojs-editor.cmxa
LIBED_A=$(LIBED:.cmxa=.a)
LIBED_CMXS=$(LIBED:.cmxs)
LIBED_BYTE=$(LIBED:.cmxa=.cma)

LIBED_CMXFILES=ojsed_types.cmx \
	ojsed_files.cmx \
	ojsed_server.cmx

LIBED_CMOFILES=$(LIBED_CMXFILES:.cmx=.cmo)
LIBED_CMIFILES=$(LIBED_CMXFILES:.cmx=.cmi)
LIBED_OFILES=$(LIBED_CMXFILES:.cmx=.o)

LIBEDJS=ojs-editor-js.cma

LIBEDJS_CMOFILES=ojsed_types.cmo \
	ojs_ace.cmo \
	ojsed_js.cmo

LIBEDJS_CMIFILES=$(LIBEDJS_CMOFILES:.cmx=.cmi)

#

all: opt byte

opt: $(LIB) $(LIB_CMXS) $(LIBFT) $(LIBFT_CMXS) $(LIBED) $(LIBED_CMXS)

byte: $(LIB_BYTE) $(LIBJS) $(LIBFT_BYTE) $(LIBFTJS) $(LIBED_BYTE) $(LIBEDJS)

# Base

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIB_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIB_CMOFILES)

$(LIBJS): $(LIBJS_CMIFILES) $(LIBJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBJS_CMOFILES)

# Filetree

$(LIBFT): $(LIBFT_CMIFILES) $(LIBFT_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBFT_CMXFILES)

$(LIBFT_CMXS): $(LIBFT_CMIFILES) $(LIBFT_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBFT_CMXFILES)

$(LIBFT_BYTE): $(LIBFT_CMIFILES) $(LIBFT_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBFT_CMOFILES)

$(LIBFTJS): $(LIBFTJS_CMIFILES) $(LIBFTJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBFTJS_CMOFILES)

# Editor

$(LIBED): $(LIBED_CMIFILES) $(LIBED_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBED_CMXFILES)

$(LIBED_CMXS): $(LIBED_CMIFILES) $(LIBED_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBED_CMXFILES)

$(LIBED_BYTE): $(LIBED_CMIFILES) $(LIBED_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBED_CMOFILES)

$(LIBEDJS): $(LIBEDJS_CMIFILES) $(LIBEDJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBEDJS_CMOFILES)

# Example

.PHONY: example
example: example/example-server example/example.js

example/example-server: $(LIB) $(LIBFT) $(LIBED) \
	example/example_types.cmx example/example.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^
example/example.js: $(LIBJS) $(LIBFTJS) $(LIBEDJS) \
	example/example_types.cmo example/example_js.cmo
	$(OCAMLFIND) ocamlc -o t.x $^ -package yojson,js_of_ocaml -linkpkg
	$(JS_OF_OCAML) t.x -o $@

##########
.PHONY: doc webdoc ocamldoc

doc:
	$(OCAMLFIND) ocamldoc -d ocamldoc -html $(OF_FLAGS) -verbose  `ls ojs_*.ml | grep -v _js.ml`

doc: dump.odoc
	$(MKDIR) doc
	$(OCAMLFIND) ocamldoc -load dump.odoc -t Ojs-base -d doc -html

dump.odoc:
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) `ls ojs_*.ml | grep -v _js.ml` -dump dump.odoc

docstog: dump.odoc
	$(MKDIR) ../web/refdoc
	$(OCAMLFIND) ocamldoc -load dump.odoc \
	-t Ojs-base -d ../web/refdoc -g odoc_stog.cmo


##########
install: install-lib

install-lib:
	@$(OCAMLFIND) install $(PACKAGE) META \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_OFILES) \
		$(LIB_BYTE) $(LIB) $(LIB_A) $(LIB_CMXS) \
		$(LIBJS) $(LIBJS_CMIFILES) \
		$(LIBFT_CMIFILES) $(LIBFT_CMXFILES) $(LIBFT_OFILES) \
		$(LIBFT_BYTE) $(LIBFT) $(LIBFT_A) $(LIBFT_CMXS) \
		$(LIBFTJS) $(LIBFTJS_CMIFILES) \
		$(LIBED_CMIFILES) $(LIBED_CMXFILES) $(LIBED_OFILES) \
		$(LIBED_BYTE) $(LIBED) $(LIBED_A) $(LIBED_CMXS) \
		$(LIBEDJS) $(LIBEDJS_CMIFILES)


uninstall: uninstall-lib

uninstall-lib:
	@$(OCAMLFIND) remove $(PACKAGE)

#####
clean:
	$(RM) *.cm* *.o *.a *.x *.annot
	(cd example ; $(RM) *.cm* *.o *.a *.x *.annot)

distclean: clean

#############
.PRECIOUS:

.PHONY: clean depend

.depend depend:
	$(OCAMLFIND) ocamldep -package js_of_ocaml.syntax -syntax camlp4o \
	`ls ojs*.ml ojs*.mli example/*.ml | grep -v types.ml`  > .depend

ojsft_server.cmo: ojsft_types.cmi
ojsft_server.cmx: ojsft_types.cmi ojsft_types.cmx
ojsft_js.cmo: ojsft_types.cmi

ojsed_server.cmo: ojsed_types.cmi
ojsed_server.cmx: ojsed_types.cmi ojsed_types.cmx
ojsed_js.cmo: ojsed_types.cmi

include .depend

############

