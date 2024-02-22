library(Tjazi)
library(stringr)
library(dplyr)
library(mediation)

set.seed(1)

data(guidebook_data)

counts <- counts ; metadata <- metadata ; diet <- diet
#Repeat the cleaning and transformation steps from part 1
metadata$master_ID <- gsub(metadata$master_ID, pattern = "-", replacement = ".")
counts <- counts[,metadata$master_ID]
#Fork off your count data so that you always have an untouched version handy.
genus <- counts
#make sure our count data is all numbers
genus <- apply(genus,c(1,2),function(x) as.numeric(as.character(x)))
#Remove features with prevalence < 10% in two steps:
#First, determine how often every feature is absent in a sample
n_zeroes <- rowSums(genus == 0)
#Then, remove features that are absent in more than your threshold (90% in this case).
genus <- genus[n_zeroes <= round(ncol(genus) * 0.90),]
#Perform a CLR transformation
genus.exp <- clr_c(genus)

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

tse <- TreeSummarizedExperiment(assays = list(counts = as.matrix(genus),
                                              clr = as.matrix(genus.exp)),
                                colData = metadata)

tse <- transformAssay(tse,
                      method = "relabundance")

colData(tse)$Diet <- PC1
colData(tse)$Phenotype <- tse$Group == "schizophrenia"

