outcome <- "Phenotype"
treatment <- "Diet"

med_df <- mediate_assay(tse,
                        outcome = outcome,
                        treatment = treatment,
                        assay.type = "relabundance",
                        family = binomial("logit"),
                        boot = TRUE)

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
