library(rmarkdown)
library(quarto)

quarto_render("example.qmd",
              output_format = "md")
