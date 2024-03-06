library(matrixStats)
library(vegan)
library(GUniFrac)
library(MedTest)

data(aofib)
data(bmi)
data(tree.rooted)
data(otu.tab)

unifracs <- GUniFrac(otu.tab, tree.rooted)$unifracs
m.list <- list(BC=vegdist(otu.tab, method="bray"), 
               JAC=as.matrix(vegdist(otu.tab, 'jaccard', binary=TRUE)),
               UniFrac=unifracs[, , c('d_UW')],
               GUniFrac=unifracs[, , c('d_0.5')], 
               WUniFrac=unifracs[, , c('d_1')])

set.seed(12345)
MedOmniTest(aofib, bmi, m.list)





tse <- LahtiWAData()

tse <- transformAssay(tse,
                      assay.type = "counts",
                      method = "relabundance")

tse <- transformAssay(tse,
                      assay.type = "relabundance",
                      method = "clr",
                      pseudocount = TRUE)


tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

colData(tse)$bmi_group <- as.numeric(tse$bmi_group)
tse <- tse[ , tse$nationality %in% c("CentralEurope", "Scandinavia")]
colData(tse)$nationality <- as.numeric(factor(tse$nationality)) - 1


d_bray <- as.matrix(vegdist(t(assay(tse, "relabundance")), method = "bray"))
d_jaccard <- as.matrix(vegdist(t(assay(tse, "relabundance")), method = "jaccard"))
d_aitchison <- as.matrix(vegdist(t(assay(tse, "clr")), method = "euclidean"))

m_list <- list(bray = d_bray, jaccard = d_jaccard, aitchison = d_aitchison)

set.seed(12345)
MedOmniTest(x = tse$nationality,
            y = tse$bmi_group,
            m.list = m_list,
            nperm = 1000)

# $margPs
# [1] 0.000999001 0.000999001 0.000999001

# $permP
# [1] 0.000999001
