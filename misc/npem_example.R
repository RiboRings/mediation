source("NPEM/NPEMv1.R")

tse <- LahtiWAData()

tse <- transformAssay(tse,
                      method = "relabundance")

tse <- transformAssay(tse,
                      method = "clr",
                      pseudocount = 1)

colData(tse)$bmi_group <- as.numeric(tse$bmi_group)

tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

tse <- tse[ , tse$nationality %in% c("CentralEurope", "Scandinavia")]
colData(tse)$nationality <- as.numeric(factor(tse$nationality)) - 1

tse <- mergeFeaturesByRank(tse, rank = "Family")

X_arr <- data.frame(X1 = tse$nationality)
rownames(X_arr) <- colnames(tse)

Y_arr <- data.frame(Diagnosis = tse$bmi_group)
rownames(Y_arr) <- colnames(tse)

M_arr <- apply(t(assay(tse, "counts")),
               c(1, 2), function(s) log(s + 1))

npem_res <- NPEM(X = X_arr,
                 M = M_arr,
                 Y = Y_arr,
                 method = "UV")
