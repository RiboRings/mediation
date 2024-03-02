library(mia)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(tibble)
library(mediation)
library(hdmed)

coldata <- read_tsv("data/metadata.tsv")
count_assay <- read_tsv("data/species.counts.tsv")

count_assay <- count_assay %>%
  column_to_rownames("Sample") %>%
  as.matrix() %>%
  t()

rowdata <- data.frame(Taxonomy = rownames(count_assay)) %>%
  separate(col = "Taxonomy",
           into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
           sep = ";")

rownames(count_assay) <- rowdata$Species

tse <- TreeSummarizedExperiment(assays = list(counts = count_assay),
                                colData = coldata,
                                rowData = rowdata)

tse <- subsetByPrevalentFeatures(tse, prevalence = 0.1)

tse <- transformAssay(tse,
                      method = c("relabundance"))

tse <- transformAssay(tse,
                      method = "clr",
                      pseudocount = 1)

tse <- estimateDiversity(tse,
                         assay.type = "relabundance",
                         index = c("shannon", "coverage", "gini_simpson"))

fit_t <- lm(BMI ~ Glucose + Total_Cholesterol + Age + Gender + SmokingStatus,
            data = as.data.frame(colData(tse)))
summary(fit_t)

fit_t <- lm(shannon ~ Glucose + Total_Cholesterol + Age + Gender + SmokingStatus, data = as.data.frame(colData(tse)))
summary(fit_t)

tse <- tse[ , tse$Study.Group == "Healthy"]
colData(tse)$Study.Group <- factor(tse$Study.Group)
med_df <- mediate_coldata(tse,
                          outcome = "BMI",
                          treatment = "Total_Cholesterol",
                          mediator = "shannon",
                          boot = TRUE, sims = 300)

tse_sub <- tse[ , !is.na(tse$Total_Cholesterol)]

res <- mediate_hdma(A = tse_sub$Total_Cholesterol,
                    M = t(assay(tse_sub, "clr")),
                    Y = tse_sub$BMI)
                   # C1 = as.matrix(final_df))


library(caret)

#define one-hot encoding function
dummy <- dummyVars(" ~ .", data=as.data.frame(tse_sub$Study.Group))

#perform one-hot encoding on data frame
final_df <- data.frame(predict(dummy, newdata=as.data.frame(tse_sub$Study.Group)))

                       