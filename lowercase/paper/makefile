default : lowercase.pdf

MAKEPDF=../../pactom/rsvg/rsvg-convert -f pdf

GRAPHICS=weirdchar-lo.pdf weirdchar-up.pdf training.pdf lowerlowera.pdf comicsands.pdf samecasepr.pdf perfecthallucination.pdf sdfmergederror.pdf connected.pdf

lowercase.pdf : lowercase.tex lowercase.bib $(GRAPHICS)
	pdflatex lowercase
	bibtex lowercase
	pdflatex lowercase
	pdflatex lowercase

# With generated PGN including graphical chesspieces, this needs more
# memory for tex. With miktex, try
#   initexmf --edit-config-file=pdflatex
# then add these lines
#   main_memory=5000000
#   extra_mem_bot=5000000
#   font_mem_size=5000000
#   pool_size=5000000
#   buf_size=5000000
# then run
#   initexmf --dump=pdflatex

%.pdf : %.svg
	${MAKEPDF} $< -o $@
