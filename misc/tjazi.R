library(Tjazi)
library(stringr)
library(dplyr)
library(mediation)

set.seed(1)

data(guidebook_data)

counts <- counts ; metadata <- metadata ; diet <- diet
#Repeat the cleaning and transformation steps from part 1
metadata$master_ID <- gsub(metadata$master_ID, pattern = "-", replacement = ".")
counts <- as.matrix(counts[ , metadata$master_ID])

diet.pca = diet %>%
  #Replace text with numbers
  mutate(across(everything(), ~str_replace( ., "hardly",                      "1" )),
         across(everything(), ~str_replace( ., "often",                       "2" )),
         across(everything(), ~str_replace( ., "twice or three times a week", "3" )),
         across(everything(), ~str_replace( ., "every day",                   "4" )),
         across(everything(), as.numeric)) %>%
  #Center and rescale data, then perform a principal component analysis
  scale() %>%
  prcomp()
#Let's check out the loadings of the first principal component using a histogram
PC1 <- diet.pca$x[,1]
hist(PC1)

rowdata <- data.frame(Taxon = rownames(counts)) %>%
  separate(Taxon, into = c("Family", "Genus"), sep = "_", extra = "merge") %>%
  mutate(Genus = str_remove_all(Genus, "unclassified_|Family_XIII_"))

rownames(counts) <- rowdata$Genus

tse <- TreeSummarizedExperiment(assays = list(counts = counts),
                                colData = metadata,
                                rowData = rowdata)

tse <- subsetByPrevalentFeatures(tse, prevalence = 0.1)

tse <- transformAssay(tse,
                      method = "relabundance")

tse <- transformAssay(tse,
                      method = "clr",
                      pseudocount = 1)

colData(tse)$Diet <- PC1
colData(tse)$Phenotype <- tse$Group == "schizophrenia"

altExp(tse, "family") <- mergeFeaturesByRank(tse, rank = "Family")
