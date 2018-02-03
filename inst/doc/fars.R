## ----echo=TRUE-----------------------------------------------------------
library(fars)

file = make_filename(2013)
data = fars_read(file)
dim(data)

## ----echo=TRUE-----------------------------------------------------------
library(fars)
data = fars_read_years(2013:2014)

data[[1]]


## ------------------------------------------------------------------------
library(fars)
fars_summarize_years(2013:2015)

## ---- echo=TRUE, results='asis'------------------------------------------
library(fars)
fars_map_state("4", "2013")

