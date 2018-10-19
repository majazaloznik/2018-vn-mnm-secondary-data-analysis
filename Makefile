# VARIABLE DEFINITIONS  #######################################################
###############################################################################
# folders #####################################################################
DIR:= .#
CODE:= $(DIR)/code

DATA:= $(DIR)/data

FIG:= $(DIR)/figures

DT/P:= $(DATA)/processed
DT/R:= $(DATA)/raw
DT/I:= $(DATA)/interim

DOC:= $(DIR)/docs
JRN:= $(DOC)/journals
RPRT:= $(DOC)/reports

# FILES #######################################################################
# plot  files
FIG/plots := $(FIG)/pyramid*.eps $(FIG)/prop*.eps $(FIG)/thresh*.eps

# maps files
FIG/maps := $(FIG)/map*.eps

# all interim data filee
DT/I/.rds :=  $(DT/I)/*.rds

# all processed files
DT/P/.rds := $(DT/P)/*.rds

# poster filename
POSTER := $(DIR)/docs/presentations/PH14.02.Factsheet

# final human readable data 
RESULTS := $(DIR)/results/human-readable/final.data.csv

# COMMANDS ####################################################################
# recipe to make .dot file  of this makefile
define make2dot
	@echo creating the .dot file from the dependencies in this makefile ----------
	python $(DIR)/code/makefile2dot.py < $< > $@
	sed -i 's/rankdir="BT"/rankdir="TB"/' $(DT/P)/make.dot	
	@echo done -------------------------------------------------------------------
endef 

# recipe to make .png file  from the dot file
define dot2png
	@echo Creating the .png from the .dot ----------------------------------------
  Rscript -e "source('$<')"
	@echo done -------------------------------------------------------------------
endef

# recipe to knit pdf from first prerequisite
define rmd2pdf
	@echo creating the $(@F) file by knitting it in R. ---------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown)));\
	render('$<', output_dir = '$(@D)', output_format = 'pdf_document',\
	quiet = TRUE )"
	-rm $(wildcard $(@D)/tex2pdf*) -fr
endef 

# recipe to knit html from first prerequisite
define rmd2html
	@echo creating the $(@F) file by knitting it in R.---------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown))); \
	render('$<', output_dir = '$(@D)', output_format = 'html_document',\
	quiet = TRUE )"
endef 

# recipe run latex with bibtex
define tex2dvi
	latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $< 
  bibtex $(basename $@)
	latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $<
  latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $<
endef 

# recipe run dvips for a0poster i.e. move the header file
define dvi2ps
	cp docs/presentations/a0header.ps a0header.ps
	dvips  -Pdownload35 -q -o $@ $<
  rm a0header.ps
endef

# recipe for creating pdf from ps file
define ps2pdf
	ps2pdf $< $@
endef

# recipe for sourcing the prerequisite R file
define sourceR
	Rscript -e "source('$<')"
endef

# DEPENDENCIES   ##############################################################
###############################################################################

all: journal readme dot analysis

.PHONY: all

analysis: $(DT/R)/urbanization.rates.csv

# make chart from .dot #########################################################
dot: $(FIG)/make.png

# make chart from .dot
$(FIG)/make.png: $(CODE)/dot2png.R $(DT/P)/make.dot
	@$(dot2png)

# make file .dot from the .makefile
$(DT/P)/make.dot: $(DIR)/Makefile
	@$(make2dot)


# journals from Rmds ###########################################################
journal: $(JRN)/journal.html $(JRN)/journal.pdf 

# journal (with graph) render to  pdf
$(JRN)/journal.pdf:  $(JRN)/journal.Rmd 
	$(rmd2pdf)

# journal (with graph) render to  html
$(JRN)/journal.html:  $(JRN)/journal.Rmd 
	$(rmd2html)


# README from Rmds #############################################################
readme: README.html

README.html: README.md 
	$(rmd2html)

	
# download data on urbanization rates
$(DT/R)/urbanization.rates.xls:
	curl -o $@ "https://population.un.org/wup/Download/Files/WUP2018-F09-Urbanization_Rate.xls"


