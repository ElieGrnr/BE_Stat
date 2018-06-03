LC = latexmk
FLAGS = -shell-escape -lualatex -synctex=1 -silent

all: rapport.pdf annexe.pdf

%.pdf: %.tex bibli.bib
	$(LC) $(FLAGS) $<

clean:
	$(LC) -c
