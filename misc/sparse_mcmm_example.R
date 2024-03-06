library(SparseMCMM)

##### Simulation data
Treatment <- SimulatedData$Treatment
otu.com <- SimulatedData$otu.com
outcome <- SimulatedData$outcome

##### SparseMCMM function
SparseMCMM(Treatment,
           otu.com,
           outcome,
           n.split = 1,
           num.per = 10)



tse <- LahtiWAData()

tse <- transformAssay(tse,
                      assay.type = "counts",
                      method = "relabundance")

colData(tse)$bmi_group <- as.numeric(tse$bmi_group)

tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

tse <- tse[ , tse$nationality %in% c("CentralEurope", "Scandinavia")]
colData(tse)$nationality <- as.numeric(factor(tse$nationality)) - 1

tse_agg <- mergeFeaturesByRank(tse, rank = "Family")

SparseMCMM(Treatment = tse_agg$nationality,
           otu.com = t(assay(tse_agg, "relabundance")),
           outcome = tse_agg$bmi_group,
           n.split = 1, num.per = 5)
