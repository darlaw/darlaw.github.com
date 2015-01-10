#Import libraries
if("knitr" %in% rownames(installed.packages()) == FALSE) {
    install.packages("knitr",
                     repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(knitr) #to plot data

setwd("C:/_portfolio/courses/_coursera.reproducibleresearch/assign01")
knit2html("PA1_template.Rmd",options = c('toc', markdown::markdownHTMLOptions(TRUE)))
browseURL("PA1_template.html")
