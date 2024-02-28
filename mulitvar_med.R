outcome <- "Phenotype"
treatment <- "Diet"

species_of_interest <- c("Bacteroides", "Alkaliphilus", "Cronobacter")

tse_sub1 <- tse[rowData(tse)$Genus %in% species_of_interest, ]
tse_sub2 <- tse[rowData(tse)$Genus %in% getTopFeatures(tse, 3), ]

med_df <- mediate_assay(tse_sub1,
                        outcome = outcome,
                        treatment = treatment,
                        assay.type = "clr",
                        family = binomial("logit"),
                        boot = TRUE, sims = 1000)

p <- ggplot(med_df, aes(x = ACME_estimate,
                        y = -log10(ACME_adjpval))) +
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