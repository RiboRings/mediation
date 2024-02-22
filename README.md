# Mediation Analysis in the miaverse

There are two functions available in the funs.R script:
- `mediate_assay` perfoms mediation analysis for every taxon in an assay of choice or for every component of a reducedDim of choice
- `mediate_coldata` performs mediation analysis for a variable in the colData

In either case, the treatment and the outcome variables are taken from the colData.

## Prepare for examples

The following code can also be found in the tutorial.R script.

```
library(mia)
library(dplyr)
library(tidyr)
library(mediation)
library(scater)
library(ggplot2)
library(ggrepel)

source("./funs.R")

data("enterotype", package = "mia")
tse <- enterotype

tse <- tse[ , tse$ClinicalStatus %in% c("healthy", "obese")]

tse <- subsetByPrevalentFeatures(tse,
                                 prevalence = 0.1)

tse <- transformAssay(tse, method = "relabundance")
```

## Example: alpha diversity as mediator

Here the question is: does shannon diversity mediate the effect of age on clinical status?

```
tse <- estimateDiversity(tse, index = "shannon")

med_out <- mediate_coldata(tse,
                           outcome = "ClinicalStatus",
                           treatment = "Age",
                           mediator = "shannon",
                           family = binomial("logit"),
                           boot = TRUE, sims = 1000)

summary(med_out)
plot(med_out)
```

## Example: reduced dimension as mediator

Here the question is: does any of the reduced components mediate the effect of gender on clinical status?

```
tse <- runMDS(tse,
              assay.type = "relabundance",
              ncomponents = 5)

med_df <- mediate_assay(tse,
                        outcome = "ClinicalStatus",
                        treatment = "Gender",
                        dim.type = "MDS",
                        family = binomial("logit"),
                        boot = TRUE, sims = 300)

p <- ggplot(med_df, aes(x = ACME_estimate,
                        y = -log10(ACME_pval))) +
  geom_point() +
  scale_x_continuous(limits = c(-0.11, 0.11)) +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed",
             colour = "red") +
  geom_text_repel(data = subset(med_df, ACME_pval < 0.05),
                  aes(label = Mediator),
                  colour = "black") +
  labs(x = "Mediated Effect",
       y = "Negative Decadic Logarithm of P-value") +
  theme_classic()

p
```

## Example: taxa in assay as mediator

Here the question is: does any of taxa mediate the effect of age on clinical status?

```
med_df <- mediate_assay(tse,
                        outcome = "ClinicalStatus",
                        treatment = "Age",
                        assay.type = "relabundance",
                        family = binomial("logit"),
                        boot = TRUE, sims = 100)

p <- ggplot(med_df, aes(x = ACME_estimate,
                        y = -log10(ACME_pval))) +
  geom_point() +
  scale_x_continuous(limits = c(-0.05, 0.05)) +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed",
             colour = "red") +
  geom_text_repel(data = subset(med_df, ACME_pval < 0.05),
                  aes(label = Mediator),
                  colour = "black") +
  labs(x = "Mediated Effect",
       y = "Negative Decadic Logarithm of P-value") +
  theme_classic()

p
```
