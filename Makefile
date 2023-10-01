RSCRIPT=Rscript

.PHONY: build

build: devinstall.R
	$(RSCRIPT)  devinstall.R


