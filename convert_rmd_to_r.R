# this file converts an rmarkdown rmd to r
library(knitr)
# Extract R Code only
purl("thomas_bahng_hw5.Rmd", output = "thomas_bahng_hw5.R")