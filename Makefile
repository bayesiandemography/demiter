
.PHONY: all
all: README.md \
     documentation

## Create README

README.md : README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'

## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

# ## Vignette

# vignettes/a.png : vignettes/a.pdf
# 	convert -trim -density 500 -resize 20% -quality 100 -background white -alpha remove -alpha  off $< $@

# vignettes/demiter.md : vignettes/demiter.Rmd
# 	cd vignettes; R -e 'knitr::knit("demiter.Rmd")'
