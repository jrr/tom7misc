default : markers.pdf


MAKEPDF=../../pactom/rsvg/rsvg-convert -f pdf
markers.pdf : page1.svg page2.svg page3.svg page4.svg page5.svg page6.svg page7.svg page8.svg page9.svg page10.svg page11.svg
	${MAKEPDF} $^ -o $@
