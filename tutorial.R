library(mia)
library(tidyr)
library(mediation)
library(scater)
library(ggplot2)
library(ggrepel)

data("enterotype", package = "mia")
tse <- enterotype

tse <- tse[ , tse$ClinicalStatus %in% c("healthy", "obese")]

tse <- subsetByPrevalentFeatures(tse,
                                 prevalence = 0.1)

tse <- transformAssay(tse, method = "relabundance")

tse <- estimateDiversity(tse, index = "shannon")

med_out <- mediate_coldata(tse,
                           outcome = "ClinicalStatus",
                           treatment = "Age",
                           mediator = "shannon",
                           family = binomial("logit"),
                           boot = TRUE, sims = 1000)

summary(med_out)
plot(med_out)





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
