library(MarZIC)

tse <- LahtiWAData()

tse <- transformAssay(tse,
                      assay.type = "counts",
                      method = "relabundance")

colData(tse)$bmi_group <- as.numeric(tse$bmi_group)

tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

tse <- tse[ , tse$nationality %in% c("CentralEurope", "Scandinavia")]
colData(tse)$nationality <- as.numeric(factor(tse$nationality)) - 1

tse$lib_size <- colSums(assay(tse, "counts"))

tse_agg <- mergeFeaturesByRank(tse, rank = "Phylum")

MarZIC(MicrobData = t(assay(tse_agg, "relabundance")),
       CovData = as.data.frame(colData(tse_agg)),
       lib_name = "lib_size",
       y_name = "bmi_group",
       x_name = "nationality")
