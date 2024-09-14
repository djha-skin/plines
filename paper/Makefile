


pennant-lines.pdf: pennant-lines.bib pennant-lines.tex img/pennant-lines.png img/Halyard.png
	pdflatex pennant-lines.tex
	bibtex pennant-lines.aux
	dot -Tpdf -o CompleteTree.pdf CompleteTree.dot
	dot -Tpdf -o BrokenTree.pdf BrokenTree.dot
	dot -Tpdf -o PennantTree.pdf PennantTree.dot
	dot -Tpdf -o CompletePennantTree.pdf CompletePennantTree.dot
	dot -Tpdf -o SplitTree.pdf SplitTree.dot
	#dot -Tpdf -o SpineReversedTree.pdf SpineReversedTree.dot
	#dot -Tpdf -o PennantLines.pdf PennantLines.dot
	pdflatex pennant-lines.tex
