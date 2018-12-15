default : survival.pdf

PIECE_PDFS=piece0.pdf piece1.pdf piece2.pdf piece3.pdf piece4.pdf piece5.pdf piece6.pdf piece7.pdf piece8.pdf piece9.pdf piece10.pdf piece11.pdf piece12.pdf piece13.pdf piece14.pdf piece15.pdf  piece16.pdf piece17.pdf piece18.pdf piece19.pdf piece20.pdf piece21.pdf piece22.pdf piece23.pdf piece24.pdf piece25.pdf piece26.pdf piece27.pdf piece28.pdf piece29.pdf piece30.pdf piece31.pdf

MAKEPDF=../../pactom/rsvg/rsvg-convert -f pdf

survival.pdf : survival.tex chess.bib piece-survival-export.pdf ${PIECE_PDFS}
	pdflatex survival
	bibtex survival
	pdflatex survival
	pdflatex survival

%.pdf : ../%.svg
	${MAKEPDF} $< -o $@
