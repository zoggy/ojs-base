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

OF_FLAGS=-package $(PACKAGES),xtmpl

COMPFLAGS=-I +ocamldoc -I example -annot -g -thread -rectypes -verbose -annot #-w +K
OCAMLPP=

RM=rm -f
CP=cp -f
MKDIR=mkdir -p

# Base

LIB=ojs-base.cmxa
LIB_A=$(LIB:.cmxa=.a)
LIB_CMXS=$(LIB:.cmxa=.cmxs)
LIB_BYTE=$(LIB:.cmxa=.cma)

LIB_CMXFILES= \
	ojs_misc.cmx \
	ojs_types.cmx \
	ojs_rpc.cmx \
	ojs_path.cmx

LIB_CMOFILES=$(LIB_CMXFILES:.cmx=.cmo)
LIB_CMIFILES=$(LIB_CMXFILES:.cmx=.cmi)
LIB_OFILES=$(LIB_CMXFILES:.cmx=.o)

LIB_SERVER=ojs-base-server.cmxa
LIB_SERVER_A=$(LIB_SERVER:.cmxa=.a)
LIB_SERVER_CMXS=$(LIB_SERVER:.cmxs)
LIB_SERVER_BYTE=$(LIB_SERVER:.cmxa=.cma)

LIB_SERVER_CMXFILES= \
	ojs_tmpl.cmx \
	ojs_server.cmx

LIB_SERVER_CMOFILES=$(LIB_SERVER_CMXFILES:.cmx=.cmo)
LIB_SERVER_CMIFILES=$(LIB_SERVER_CMXFILES:.cmx=.cmi)
LIB_SERVER_OFILES=$(LIB_SERVER_CMXFILES:.cmx=.o)

LIBJS=ojs-base-js.cma

LIBJS_CMOFILES=	\
	ojs_js.cmo \
	ojsmsg_js.cmo

LIBJS_CMIFILES=$(LIBJS_CMOFILES:.cmo=.cmi)

# Filetree

LIBFT=ojs-filetree.cmxa
LIBFT_A=$(LIBFT:.cmxa=.a)
LIBFT_CMXS=$(LIBFT:.cmxs)
LIBFT_BYTE=$(LIBFT:.cmxa=.cma)

LIBFT_CMXFILES=\
	ojsft_types.cmx

LIBFT_CMOFILES=$(LIBFT_CMXFILES:.cmx=.cmo)
LIBFT_CMIFILES=$(LIBFT_CMXFILES:.cmx=.cmi)
LIBFT_OFILES=$(LIBFT_CMXFILES:.cmx=.o)

LIBFT_SERVER=ojs-filetree-server.cmxa
LIBFT_SERVER_A=$(LIBFT_SERVER:.cmxa=.a)
LIBFT_SERVER_CMXS=$(LIBFT_SERVER:.cmxs)
LIBFT_SERVER_BYTE=$(LIBFT_SERVER:.cmxa=.cma)

LIBFT_SERVER_CMXFILES=\
	ojsft_find.cmx \
	ojsft_files.cmx \
	ojsft_server.cmx

LIBFT_SERVER_CMOFILES=$(LIBFT_SERVER_CMXFILES:.cmx=.cmo)
LIBFT_SERVER_CMIFILES=$(LIBFT_SERVER_CMXFILES:.cmx=.cmi)
LIBFT_SERVER_OFILES=$(LIBFT_SERVER_CMXFILES:.cmx=.o)

LIBFTJS=ojs-filetree-js.cma

LIBFTJS_CMOFILES= \
	ojsft_js.cmo

LIBFTJS_CMIFILES=$(LIBFTJS_CMOFILES:.cmo=.cmi)

# Editor

LIBED=ojs-editor.cmxa
LIBED_A=$(LIBED:.cmxa=.a)
LIBED_CMXS=$(LIBED:.cmxs)
LIBED_BYTE=$(LIBED:.cmxa=.cma)

LIBED_CMXFILES=\
	ojsed_types.cmx

LIBED_CMOFILES=$(LIBED_CMXFILES:.cmx=.cmo)
LIBED_CMIFILES=$(LIBED_CMXFILES:.cmx=.cmi)
LIBED_OFILES=$(LIBED_CMXFILES:.cmx=.o)

LIBED_SERVER=ojs-editor-server.cmxa
LIBED_SERVER_A=$(LIBED_SERVER:.cmxa=.a)
LIBED_SERVER_CMXS=$(LIBED_SERVER:.cmxs)
LIBED_SERVER_BYTE=$(LIBED_SERVER:.cmxa=.cma)

LIBED_SERVER_CMXFILES=\
	ojsed_files.cmx \
	ojsed_server.cmx

LIBED_SERVER_CMOFILES=$(LIBED_SERVER_CMXFILES:.cmx=.cmo)
LIBED_SERVER_CMIFILES=$(LIBED_SERVER_CMXFILES:.cmx=.cmi)
LIBED_SERVER_OFILES=$(LIBED_SERVER_CMXFILES:.cmx=.o)

LIBEDJS=ojs-editor-js.cma

LIBEDJS_CMOFILES=\
	ojs_ace.cmo \
	ojsed_js.cmo

LIBEDJS_CMIFILES=$(LIBEDJS_CMOFILES:.cmo=.cmi)

# Lists

LIBL=ojs-list.cmxa
LIBL_A=$(LIBL:.cmxa=.a)
LIBL_CMXS=$(LIBL:.cmxs)
LIBL_BYTE=$(LIBL:.cmxa=.cma)

LIBL_CMXFILES=\
	ojsl_types.cmx

LIBL_CMOFILES=$(LIBL_CMXFILES:.cmx=.cmo)
LIBL_CMIFILES=$(LIBL_CMXFILES:.cmx=.cmi)
LIBL_OFILES=$(LIBL_CMXFILES:.cmx=.o)

LIBL_SERVER=ojs-list-server.cmxa
LIBL_SERVER_A=$(LIBL_SERVER:.cmxa=.a)
LIBL_SERVER_CMXS=$(LIBL_SERVER:.cmxs)
LIBL_SERVER_BYTE=$(LIBL_SERVER:.cmxa=.cma)

LIBL_SERVER_CMXFILES=\
	ojsl_server.cmx

LIBL_SERVER_CMOFILES=$(LIBL_SERVER_CMXFILES:.cmx=.cmo)
LIBL_SERVER_CMIFILES=$(LIBL_SERVER_CMXFILES:.cmx=.cmi)
LIBL_SERVER_OFILES=$(LIBL_SERVER_CMXFILES:.cmx=.o)

LIBLJS=ojs-list-js.cma

LIBLJS_CMOFILES= \
	ojsl_js.cmo

LIBLJS_CMIFILES=$(LIBLJS_CMOFILES:.cmo=.cmi)

# PPX

PPX=ppx_ojs
PPX_BYTE=$(PPX).byte
PPX_CMXFILES=ppx_ojs.cmx
PPX_CMOFILES=$(PPX_CMXFILES:.cmx=.cmo)

#

all: opt byte

opt: $(LIB) $(LIB_SERVER) $(LIB_CMXS) $(LIB_SERVER_CMXS) \
	$(PPX) \
	$(LIBED) $(LIBED_SERVER) $(LIBED_CMXS) $(LIBED_SERVER_CMXS) \
	$(LIBFT) $(LIBFT_SERVER) $(LIBFT_CMXS) $(LIBFT_SERVER_CMXS) \
	$(LIBL) $(LIBL_SERVER) $(LIBL_CMXS) $(LIBL_SERVER_CMXS)

byte: $(LIB_BYTE) $(LIB_SERVER_BYTE) $(LIBJS) \
	$(PPX_BYTE) \
	$(LIBED_BYTE) $(LIBED_SERVER_BYTE) $(LIBEDJS) \
	$(LIBFT_BYTE) $(LIBFT_SERVER_BYTE) $(LIBFTJS) \
	$(LIBL_BYTE) $(LIBL_SERVER_BYTE) $(LIBLJS)

# Base

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIB_CMXFILES)

$(LIB_SERVER): $(LIB) $(LIB_SERVER_CMIFILES) $(LIB_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIB_SERVER_CMXFILES)

$(LIB_CMXS): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIB_CMXFILES)

$(LIB_SERVER_CMXS): $(LIB) $(LIB_SERVER_CMIFILES) $(LIB_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIB_SERVER_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIB_CMOFILES)

$(LIB_SERVER_BYTE): $(LIB_BYTE) $(LIB_SERVER_CMIFILES) $(LIB_SERVER_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIB_SERVER_CMOFILES)

$(LIBJS): $(LIB_BYTE) $(LIBJS_CMIFILES) $(LIBJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBJS_CMOFILES)

# PPX

$(PPX): $(PPX_CMXFILES)
	$(OCAMLFIND) ocamlopt -o $@ -package ppx_tools.metaquot,str,xmlm,xtmpl \
	$(COMPFLAGS) -linkpkg $<

$(PPX_BYTE): $(PPX_CMOFILES)
	$(OCAMLFIND) ocamlc -o $@ -package ppx_tools.metaquot,str,xmlm,xtmpl \
	$(COMPFLAGS) -linkpkg $<

ppx_ojs.cmx: ppx_ojs.ml
	$(OCAMLFIND) ocamlopt -c -rectypes -package ppx_tools.metaquot,xtmpl $<

ppx_ojs.cmo: ppx_ojs.ml
	$(OCAMLFIND) ocamlc -c -rectypes -package ppx_tools.metaquot,xtmpl $<

# Filetree

$(LIBFT): $(LIBFT_CMIFILES) $(LIBFT_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBFT_CMXFILES)

$(LIBFT_SERVER): $(LIBFT) $(LIBFT_SERVER_CMIFILES) $(LIBFT_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBFT_SERVER_CMXFILES)

$(LIBFT_CMXS): $(LIBFT_CMIFILES) $(LIBFT_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBFT_CMXFILES)

$(LIBFT_SERVER_CMXS): $(LIB) $(LIBFT_SERVER_CMIFILES) $(LIBFT_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBFT_SERVER_CMXFILES)

$(LIBFT_BYTE): $(LIBFT_CMIFILES) $(LIBFT_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBFT_CMOFILES)

$(LIBFT_SERVER_BYTE): $(LIB_BYTE) $(LIBFT_SERVER_CMIFILES) $(LIBFT_SERVER_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBFT_SERVER_CMOFILES)

$(LIBFTJS): $(LIB_BYTE) $(LIBFTJS_CMIFILES) $(LIBFTJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBFTJS_CMOFILES)

# Editor

$(LIBED): $(LIBED_CMIFILES) $(LIBED_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBED_CMXFILES)

$(LIBED_SERVER): $(LIB) $(LIBED_SERVER_CMIFILES) $(LIBED_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBED_SERVER_CMXFILES)

$(LIBED_CMXS): $(LIBED_CMIFILES) $(LIBED_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBED_CMXFILES)

$(LIBED_SERVER_CMXS): $(LIB) $(LIBED_SERVER_CMIFILES) $(LIBED_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBED_SERVER_CMXFILES)

$(LIBED_BYTE): $(LIBED_CMIFILES) $(LIBED_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBED_CMOFILES)

$(LIBED_SERVER_BYTE): $(LIB_BYTE) $(LIBED_SERVER_CMIFILES) $(LIBED_SERVER_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBED_SERVER_CMOFILES)

$(LIBEDJS): $(LIB_BYTE) $(LIBEDJS_CMIFILES) $(LIBEDJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBEDJS_CMOFILES)

# Lists

$(LIBL): $(LIBL_CMIFILES) $(LIBL_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBL_CMXFILES)

$(LIBL_SERVER): $(LIB) $(LIBL_SERVER_CMIFILES) $(LIBL_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -a -o $@ $(LIBL_SERVER_CMXFILES)

$(LIBL_CMXS): $(LIBL_CMIFILES) $(LIBL_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBL_CMXFILES)

$(LIBL_SERVER_CMXS): $(LIB) $(LIBL_SERVER_CMIFILES) $(LIBL_SERVER_CMXFILES)
	$(OCAMLFIND) ocamlopt$(P) -shared -o $@ $(LIBL_SERVER_CMXFILES)

$(LIBL_BYTE): $(LIBL_CMIFILES) $(LIBL_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBL_CMOFILES)

$(LIBL_SERVER_BYTE): $(LIB_BYTE) $(LIBL_SERVER_CMIFILES) $(LIBL_SERVER_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBL_SERVER_CMOFILES)

$(LIBLJS): $(LIB_BYTE) $(LIBLJS_CMIFILES) $(LIBLJS_CMOFILES)
	$(OCAMLFIND) ocamlc$(PBYTE) -a -o $@ $(LIBLJS_CMOFILES)

# Example

.PHONY: example
example: example/example-server example/example.js

example/example-server: $(LIB) $(LIB_SERVER) $(LIBED) $(LIBED_SERVER) \
		$(LIBFT) $(LIBFT_SERVER) $(LIBL) $(LIBL_SERVER) \
	example/example_types.cmx example/example.cmx
	$(OCAMLFIND) ocamlopt -o $@ $(OF_FLAGS) -linkpkg $^
example/example.js: $(LIB_BYTE) $(LIBJS) $(LIBED_BYTE) $(LIBEDJS) \
		$(LIBFT_BYTE) $(LIBFTJS) $(LIBL_BYTE) $(LIBLJS)  \
	example/example_types.cmo example/example_js.cmo
	$(OCAMLFIND) ocamlc -o t.x $^ -package yojson,ppx_deriving_yojson,js_of_ocaml -linkpkg
	$(JS_OF_OCAML) t.x -o $@

test_ppx: test_ppx_ojs
.PHONY: test_ppx_ojs

test_ppx_ojs:
	$(OCAMLFIND) ocamlopt -o $@ -dsource -rectypes -package xtmpl,cohttp -linkpkg \
	-ppx ./ppx_ojs ojs-base.cmxa test_ppx_ojs.ml
	@echo "======"
	$(OCAMLFIND) ocamlopt -i -rectypes -package xtmpl,cohttp -ppx ./ppx_ojs test_ppx_ojs.ml
	@echo "======"
	./$@

##########
.PHONY: doc webdoc ocamldoc

doc: dump.odoc dump_js.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc -d ocamldoc -html $(OF_FLAGS) -verbose \
	-load dump.odoc -load dump_js.odoc -sort -t Ojs-base


dump.odoc:
	$(OCAMLFIND) ocamldoc -v $(OF_FLAGS) `ls ojs*.ml ojs*.mli | grep -v _js.ml` -rectypes -dump $@
dump_js.odoc:
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(JSOC_SYNTAX) `ls *_js.ml` -rectypes -dump $@

docstog: dump.odoc dump_js.odoc
	$(MKDIR) ../web/refdoc
	$(OCAMLFIND) ocamldoc 	-load dump.odoc -load dump_js.odoc \
	-t Ojs-base -d ../web/refdoc -g odoc_stog.cmo


##########
install: install-lib

install-lib:
	@$(OCAMLFIND) install $(PACKAGE) META \
		$(LIB_CMIFILES) $(LIB_CMXFILES) $(LIB_OFILES) \
		$(LIB_BYTE) $(LIB) $(LIB_A) $(LIB_CMXS) \
		$(LIB_SERVER_CMIFILES) $(LIB_SERVER_CMXFILES) $(LIB_SERVER_OFILES) \
		$(LIB_SERVER_BYTE) $(LIB_SERVER) $(LIB_SERVER_A) $(LIB_SERVER_CMXS) \
		$(PPX) $(PPX_BYTE) \
		$(LIBJS) $(LIBJS_CMIFILES) \
		$(LIBFT_CMIFILES) $(LIBFT_CMXFILES) $(LIBFT_OFILES) \
		$(LIBFT_BYTE) $(LIBFT) $(LIBFT_A) $(LIBFT_CMXS) \
		$(LIBFT_SERVER_CMIFILES) $(LIBFT_SERVER_CMXFILES) $(LIBFT_SERVER_OFILES) \
		$(LIBFT_SERVER_BYTE) $(LIBFT_SERVER) $(LIBFT_SERVER_A) $(LIBFT_SERVER_CMXS) \
		$(LIBFTJS) $(LIBFTJS_CMIFILES) \
		$(LIBED_CMIFILES) $(LIBED_CMXFILES) $(LIBED_OFILES) \
		$(LIBED_BYTE) $(LIBED) $(LIBED_A) $(LIBED_CMXS) \
		$(LIBED_SERVER_CMIFILES) $(LIBED_SERVER_CMXFILES) $(LIBED_SERVER_OFILES) \
		$(LIBED_SERVER_BYTE) $(LIBED_SERVER) $(LIBED_SERVER_A) $(LIBED_SERVER_CMXS) \
		$(LIBEDJS) $(LIBEDJS_CMIFILES) \
		$(LIBL_CMIFILES) $(LIBL_CMXFILES) $(LIBL_OFILES) \
		$(LIBL_BYTE) $(LIBL) $(LIBL_A) $(LIBL_CMXS) \
		$(LIBL_SERVER_CMIFILES) $(LIBL_SERVER_CMXFILES) $(LIBL_SERVER_OFILES) \
		$(LIBL_SERVER_BYTE) $(LIBL_SERVER) $(LIBL_SERVER_A) $(LIBL_SERVER_CMXS) \
		$(LIBLJS) $(LIBLJS_CMIFILES)


uninstall: uninstall-lib

uninstall-lib:
	@$(OCAMLFIND) remove $(PACKAGE)

#####
clean:
	$(RM) *.cm* *.o *.a *.x *.annot dump.odoc
	(cd example ; $(RM) *.cm* *.o *.a *.x *.annot)

distclean: clean

#############
.PRECIOUS:

.PHONY: clean depend

.depend depend:
	$(OCAMLFIND) ocamldep \
	`ls ojs*.mli ojs_*.ml example/*.ml | grep -v -E '_js.ml$$' `  > .depend

ojsft_js.cmi: ojsft_types.cmi
ojsft_js.cmo: ojsft_types.cmi ojsft_js.ml

ojsed_js.cmi: ojsed_types.cmi
ojsed_js.cmo: ojsed_types.cmi ojsed_js.ml

ojs_tmpl.cm*: templates/*.xml

include .depend

############

