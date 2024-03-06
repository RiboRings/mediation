library(rmarkdown)
library(quarto)

quarto_render("example.qmd",
              output_format = "md")

quarto_render("comparison.qmd",
              output_format = "md")
