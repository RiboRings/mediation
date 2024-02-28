library(rmarkdown)
library(quarto)

source("funs.R")

quarto_render("example.qmd",
              output_format = "md")
