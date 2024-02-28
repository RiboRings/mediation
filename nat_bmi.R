library(mia)
library(microbiomeDataSets)

mae <- LahtiMLData()

tse <- getWithColData(mae, "microbiome")

lip <- getWithColData(mae, "lipids")      

colData(tse)$Lipids <- colSums(assay(lip))

tse <- transformAssay(tse,
                      method = c("relabundance"))

tse <- estimateDiversity(tse,
                         assay.type = "relabundance",
                         index = "shannon")

fit_t <- lm(Lipids ~ group,
            data = as.data.frame(colData(tse)))
summary(fit_t)

med_df <- mediate_coldata(tse,
                          outcome = "Lipids",
                          treatment = "group",
                          mediator = "shannon",
                          boot = TRUE, sims = 300)




tse <- GrieneisenTSData()



tse <- LahtiWAData()

tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

colData(tse)$bmi.lev <- as.numeric(tse$bmi_group)

tse2 <- tse[ , tse$nationality %in% c("CentralEurope", "Scandinavia")]
tse2$nationality <- as.numeric(factor(tse2$nationality))

med_df <- mediate_coldata(tse2,
                          outcome = "bmi.lev",
                          treatment = "nationality",
                          mediator = "diversity",
                          boot = TRUE, sims = 300)

summary(med_df)
plot(med_df)

tse2 <- transformAssay(tse2, method = "clr", pseudocount = 1)

res <- mediate_hdma(A = tse2$nationality,
                    M = t(assay(tse2, "clr")),
                    Y = tse2$bmi.lev)



tse <- OKeefeDSData()

tse <- transformAssay(tse, method = "relabundance")
tse <- estimateDiversity(tse, index = "shannon", assay.type = "relabundance")

tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

colData(tse)$bmi.lev <- as.numeric(tse$bmi_group)

tse$nationality <- as.numeric(factor(tse$nationality))

med_df <- mediate_coldata(tse,
                          outcome = "bmi.lev",
                          treatment = "nationality",
                          mediator = "shannon",
                          boot = TRUE, sims = 300)


summary(med_df)
plot(med_df)

tse <- transformAssay(tse, method = "clr", pseudocount = 1)

res <- mediate_hdma(A = tse$nationality,
                    M = t(assay(tse, "clr")),
                    Y = tse$bmi.lev)
# C1 = as.matrix(final_df))



