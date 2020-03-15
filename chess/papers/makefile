default : longest.pdf
all : survival.pdf blind.pdf weak.pdf longest.pdf

PIECE_PDFS=piece0.pdf piece1.pdf piece2.pdf piece3.pdf piece4.pdf piece5.pdf piece6.pdf piece7.pdf piece8.pdf piece9.pdf piece10.pdf piece11.pdf piece12.pdf piece13.pdf piece14.pdf piece15.pdf  piece16.pdf piece17.pdf piece18.pdf piece19.pdf piece20.pdf piece21.pdf piece22.pdf piece23.pdf piece24.pdf piece25.pdf piece26.pdf piece27.pdf piece28.pdf piece29.pdf piece30.pdf piece31.pdf

MAKEPDF=../../pactom/rsvg/rsvg-convert -f pdf

survival.pdf : survival.tex chess.bib piece-survival-export.pdf ${PIECE_PDFS}
	pdflatex survival
	bibtex survival
	pdflatex survival
	pdflatex survival

blind.pdf : blind.tex chess.bib blind-allon.png
	pdflatex blind
	bibtex blind
	pdflatex blind
	pdflatex blind

weak.pdf : weak.tex chess.bib
	pdflatex weak
	bibtex weak
	pdflatex weak
	pdflatex weak

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

longest.pdf : longest.tex chess.bib slow.tex
	pdflatex longest
	bibtex longest
	pdflatex longest
	pdflatex longest

%.pdf : pieces_good/%.svg
	${MAKEPDF} $< -o $@
