# datapaperchecks

Local package with shared helper functions used by the Quarto data-quality chapters.

## Install (local)

From the project root:

```r
Rscript("R/install_datapaperchecks.R")
```

## Use in chapters

```r
.libPaths(c(".Rlib", .libPaths()))
library(datapaperchecks)
```
