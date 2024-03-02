library(mia)
library(scater)

tse <- estimateDiversity(tse,
                         index = "shannon")

outcome <- "Phenotype"
treatment <- "Diet"
mediator <- "shannon"

med_out <- mediate_coldata(tse,
                           outcome = outcome,
                           treatment = treatment,
                           mediator = mediator,
                           family = binomial("logit"),
                           boot = TRUE)

summary(med_out)
plot(med_out)
