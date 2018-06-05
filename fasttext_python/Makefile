SRCDIR=src

requirements.txt:
	conda list -e > requirements.txt

fasttext_python.yml:
	conda env export > fasttext_python.yml

all: requirements.txt fasttext_python.yml

clean:
	rm -rf requirements.txt
	rm -rf fasttext_python.yml

.PHONY: all clean
