## Regenerate the html files.

all: data_input.html sttc.html meaRtoolsGeneralUsage.html 


%.html: %.Rmd
	Rscript -e 'require(devtools); load_all(); rmarkdown::render("$^")'


.PHONY: clean

clean:
	rm -fr *_cache *_files

