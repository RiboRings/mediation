# Comparison of high-dimensional Methods
Giulio Benedetti

``` r
tse <- OKeefeDSData()
```

``` r
tse <- transformAssay(tse,
                      method = "relabundance")

tse <- estimateDiversity(tse,
                         index = "shannon",
                         assay.type = "relabundance")
```

``` r
tse <- tse[ , !is.na(tse$bmi_group)]
tse <- tse[ , !is.na(tse$nationality)]

colData(tse)$bmi_group <- as.numeric(tse$bmi_group)
colData(tse)$nationality <- as.numeric(factor(tse$nationality)) - 1

tse <- transformAssay(tse,
                      method = "clr",
                      pseudocount = 1)

tse <- tse[ , tse$timepoint.within.group == 2]
```

``` r
med_res <- mediateAssay(tse,
                        outcome = "bmi_group",
                        treatment = "nationality",
                        assay.type = "clr",
                        boot = TRUE, sims = 300)
```

    [1] "130 left"
    [1] "Current mediator: Actinomycetaceae"

    Running nonparametric bootstrap

    [1] "129 left"
    [1] "Current mediator: Aerococcus"

    Running nonparametric bootstrap

    [1] "128 left"
    [1] "Current mediator: Aeromonas"

    Running nonparametric bootstrap

    [1] "127 left"
    [1] "Current mediator: Akkermansia"

    Running nonparametric bootstrap

    [1] "126 left"
    [1] "Current mediator: Alcaligenes faecalis et rel."

    Running nonparametric bootstrap

    [1] "125 left"
    [1] "Current mediator: Allistipes et rel."

    Running nonparametric bootstrap

    [1] "124 left"
    [1] "Current mediator: Anaerobiospirillum"

    Running nonparametric bootstrap

    [1] "123 left"
    [1] "Current mediator: Anaerofustis"

    Running nonparametric bootstrap

    [1] "122 left"
    [1] "Current mediator: Anaerostipes caccae et rel."

    Running nonparametric bootstrap

    [1] "121 left"
    [1] "Current mediator: Anaerotruncus colihominis et rel."

    Running nonparametric bootstrap

    [1] "120 left"
    [1] "Current mediator: Anaerovorax odorimutans et rel."

    Running nonparametric bootstrap

    [1] "119 left"
    [1] "Current mediator: Aneurinibacillus"

    Running nonparametric bootstrap

    [1] "118 left"
    [1] "Current mediator: Aquabacterium"

    Running nonparametric bootstrap

    [1] "117 left"
    [1] "Current mediator: Asteroleplasma et rel."

    Running nonparametric bootstrap

    [1] "116 left"
    [1] "Current mediator: Atopobium"

    Running nonparametric bootstrap

    [1] "115 left"
    [1] "Current mediator: Bacillus"

    Running nonparametric bootstrap

    [1] "114 left"
    [1] "Current mediator: Bacteroides fragilis et rel."

    Running nonparametric bootstrap

    [1] "113 left"
    [1] "Current mediator: Bacteroides intestinalis et rel."

    Running nonparametric bootstrap

    [1] "112 left"
    [1] "Current mediator: Bacteroides ovatus et rel."

    Running nonparametric bootstrap

    [1] "111 left"
    [1] "Current mediator: Bacteroides plebeius et rel."

    Running nonparametric bootstrap

    [1] "110 left"
    [1] "Current mediator: Bacteroides splachnicus et rel."

    Running nonparametric bootstrap

    [1] "109 left"
    [1] "Current mediator: Bacteroides stercoris et rel."

    Running nonparametric bootstrap

    [1] "108 left"
    [1] "Current mediator: Bacteroides uniformis et rel."

    Running nonparametric bootstrap

    [1] "107 left"
    [1] "Current mediator: Bacteroides vulgatus et rel."

    Running nonparametric bootstrap

    [1] "106 left"
    [1] "Current mediator: Bifidobacterium"

    Running nonparametric bootstrap

    [1] "105 left"
    [1] "Current mediator: Bilophila et rel."

    Running nonparametric bootstrap

    [1] "104 left"
    [1] "Current mediator: Brachyspira"

    Running nonparametric bootstrap

    [1] "103 left"
    [1] "Current mediator: Bryantella formatexigens et rel."

    Running nonparametric bootstrap

    [1] "102 left"
    [1] "Current mediator: Bulleidia moorei et rel."

    Running nonparametric bootstrap

    [1] "101 left"
    [1] "Current mediator: Burkholderia"

    Running nonparametric bootstrap

    [1] "100 left"
    [1] "Current mediator: Butyrivibrio crossotus et rel."

    Running nonparametric bootstrap

    [1] "99 left"
    [1] "Current mediator: Campylobacter"

    Running nonparametric bootstrap

    [1] "98 left"
    [1] "Current mediator: Catenibacterium mitsuokai et rel."

    Running nonparametric bootstrap

    [1] "97 left"
    [1] "Current mediator: Clostridium (sensu stricto)"

    Running nonparametric bootstrap

    [1] "96 left"
    [1] "Current mediator: Clostridium cellulosi et rel."

    Running nonparametric bootstrap

    [1] "95 left"
    [1] "Current mediator: Clostridium colinum et rel."

    Running nonparametric bootstrap

    [1] "94 left"
    [1] "Current mediator: Clostridium difficile et rel."

    Running nonparametric bootstrap

    [1] "93 left"
    [1] "Current mediator: Clostridium felsineum et rel."

    Running nonparametric bootstrap

    [1] "92 left"
    [1] "Current mediator: Clostridium leptum et rel."

    Running nonparametric bootstrap

    [1] "91 left"
    [1] "Current mediator: Clostridium nexile et rel."

    Running nonparametric bootstrap

    [1] "90 left"
    [1] "Current mediator: Clostridium orbiscindens et rel."

    Running nonparametric bootstrap

    [1] "89 left"
    [1] "Current mediator: Clostridium ramosum et rel."

    Running nonparametric bootstrap

    [1] "88 left"
    [1] "Current mediator: Clostridium sphenoides et rel."

    Running nonparametric bootstrap

    [1] "87 left"
    [1] "Current mediator: Clostridium stercorarium et rel."

    Running nonparametric bootstrap

    [1] "86 left"
    [1] "Current mediator: Clostridium symbiosum et rel."

    Running nonparametric bootstrap

    [1] "85 left"
    [1] "Current mediator: Clostridium thermocellum et rel."

    Running nonparametric bootstrap

    [1] "84 left"
    [1] "Current mediator: Collinsella"

    Running nonparametric bootstrap

    [1] "83 left"
    [1] "Current mediator: Coprobacillus catenaformis et rel."

    Running nonparametric bootstrap

    [1] "82 left"
    [1] "Current mediator: Coprococcus eutactus et rel."

    Running nonparametric bootstrap

    [1] "81 left"
    [1] "Current mediator: Corynebacterium"

    Running nonparametric bootstrap

    [1] "80 left"
    [1] "Current mediator: Desulfovibrio et rel."

    Running nonparametric bootstrap

    [1] "79 left"
    [1] "Current mediator: Dialister"

    Running nonparametric bootstrap

    [1] "78 left"
    [1] "Current mediator: Dorea formicigenerans et rel."

    Running nonparametric bootstrap

    [1] "77 left"
    [1] "Current mediator: Eggerthella lenta et rel."

    Running nonparametric bootstrap

    [1] "76 left"
    [1] "Current mediator: Enterobacter aerogenes et rel."

    Running nonparametric bootstrap

    [1] "75 left"
    [1] "Current mediator: Enterococcus"

    Running nonparametric bootstrap

    [1] "74 left"
    [1] "Current mediator: Escherichia coli et rel."

    Running nonparametric bootstrap

    [1] "73 left"
    [1] "Current mediator: Eubacterium biforme et rel."

    Running nonparametric bootstrap

    [1] "72 left"
    [1] "Current mediator: Eubacterium cylindroides et rel."

    Running nonparametric bootstrap

    [1] "71 left"
    [1] "Current mediator: Eubacterium hallii et rel."

    Running nonparametric bootstrap

    [1] "70 left"
    [1] "Current mediator: Eubacterium limosum et rel."

    Running nonparametric bootstrap

    [1] "69 left"
    [1] "Current mediator: Eubacterium rectale et rel."

    Running nonparametric bootstrap

    [1] "68 left"
    [1] "Current mediator: Eubacterium siraeum et rel."

    Running nonparametric bootstrap

    [1] "67 left"
    [1] "Current mediator: Eubacterium ventriosum et rel."

    Running nonparametric bootstrap

    [1] "66 left"
    [1] "Current mediator: Faecalibacterium prausnitzii et rel."

    Running nonparametric bootstrap

    [1] "65 left"
    [1] "Current mediator: Fusobacteria"

    Running nonparametric bootstrap

    [1] "64 left"
    [1] "Current mediator: Gemella"

    Running nonparametric bootstrap

    [1] "63 left"
    [1] "Current mediator: Granulicatella"

    Running nonparametric bootstrap

    [1] "62 left"
    [1] "Current mediator: Haemophilus"

    Running nonparametric bootstrap

    [1] "61 left"
    [1] "Current mediator: Helicobacter"

    Running nonparametric bootstrap

    [1] "60 left"
    [1] "Current mediator: Klebisiella pneumoniae et rel."

    Running nonparametric bootstrap

    [1] "59 left"
    [1] "Current mediator: Lachnobacillus bovis et rel."

    Running nonparametric bootstrap

    [1] "58 left"
    [1] "Current mediator: Lachnospira pectinoschiza et rel."

    Running nonparametric bootstrap

    [1] "57 left"
    [1] "Current mediator: Lactobacillus catenaformis et rel."

    Running nonparametric bootstrap

    [1] "56 left"
    [1] "Current mediator: Lactobacillus gasseri et rel."

    Running nonparametric bootstrap

    [1] "55 left"
    [1] "Current mediator: Lactobacillus plantarum et rel."

    Running nonparametric bootstrap

    [1] "54 left"
    [1] "Current mediator: Lactobacillus salivarius et rel."

    Running nonparametric bootstrap

    [1] "53 left"
    [1] "Current mediator: Lactococcus"

    Running nonparametric bootstrap

    [1] "52 left"
    [1] "Current mediator: Leminorella"

    Running nonparametric bootstrap

    [1] "51 left"
    [1] "Current mediator: Megamonas hypermegale et rel."

    Running nonparametric bootstrap

    [1] "50 left"
    [1] "Current mediator: Megasphaera elsdenii et rel."

    Running nonparametric bootstrap

    [1] "49 left"
    [1] "Current mediator: Methylobacterium"

    Running nonparametric bootstrap

    [1] "48 left"
    [1] "Current mediator: Micrococcaceae"

    Running nonparametric bootstrap

    [1] "47 left"
    [1] "Current mediator: Mitsuokella multiacida et rel."

    Running nonparametric bootstrap

    [1] "46 left"
    [1] "Current mediator: Moraxellaceae"

    Running nonparametric bootstrap

    [1] "45 left"
    [1] "Current mediator: Novosphingobium"

    Running nonparametric bootstrap

    [1] "44 left"
    [1] "Current mediator: Oceanospirillum"

    Running nonparametric bootstrap

    [1] "43 left"
    [1] "Current mediator: Oscillospira guillermondii et rel."

    Running nonparametric bootstrap

    [1] "42 left"
    [1] "Current mediator: Outgrouping clostridium cluster XIVa"

    Running nonparametric bootstrap

    [1] "41 left"
    [1] "Current mediator: Oxalobacter formigenes et rel."

    Running nonparametric bootstrap

    [1] "40 left"
    [1] "Current mediator: Papillibacter cinnamivorans et rel."

    Running nonparametric bootstrap

    [1] "39 left"
    [1] "Current mediator: Parabacteroides distasonis et rel."

    Running nonparametric bootstrap

    [1] "38 left"
    [1] "Current mediator: Peptococcus niger et rel."

    Running nonparametric bootstrap

    [1] "37 left"
    [1] "Current mediator: Peptostreptococcus anaerobius et rel."

    Running nonparametric bootstrap

    [1] "36 left"
    [1] "Current mediator: Peptostreptococcus micros et rel."

    Running nonparametric bootstrap

    [1] "35 left"
    [1] "Current mediator: Phascolarctobacterium faecium et rel."

    Running nonparametric bootstrap

    [1] "34 left"
    [1] "Current mediator: Prevotella melaninogenica et rel."

    Running nonparametric bootstrap

    [1] "33 left"
    [1] "Current mediator: Prevotella oralis et rel."

    Running nonparametric bootstrap

    [1] "32 left"
    [1] "Current mediator: Prevotella ruminicola et rel."

    Running nonparametric bootstrap

    [1] "31 left"
    [1] "Current mediator: Prevotella tannerae et rel."

    Running nonparametric bootstrap

    [1] "30 left"
    [1] "Current mediator: Propionibacterium"

    Running nonparametric bootstrap

    [1] "29 left"
    [1] "Current mediator: Proteus et rel."

    Running nonparametric bootstrap

    [1] "28 left"
    [1] "Current mediator: Pseudomonas"

    Running nonparametric bootstrap

    [1] "27 left"
    [1] "Current mediator: Roseburia intestinalis et rel."

    Running nonparametric bootstrap

    [1] "26 left"
    [1] "Current mediator: Ruminococcus bromii et rel."

    Running nonparametric bootstrap

    [1] "25 left"
    [1] "Current mediator: Ruminococcus callidus et rel."

    Running nonparametric bootstrap

    [1] "24 left"
    [1] "Current mediator: Ruminococcus gnavus et rel."

    Running nonparametric bootstrap

    [1] "23 left"
    [1] "Current mediator: Ruminococcus lactaris et rel."

    Running nonparametric bootstrap

    [1] "22 left"
    [1] "Current mediator: Ruminococcus obeum et rel."

    Running nonparametric bootstrap

    [1] "21 left"
    [1] "Current mediator: Serratia"

    Running nonparametric bootstrap

    [1] "20 left"
    [1] "Current mediator: Sporobacter termitidis et rel."

    Running nonparametric bootstrap

    [1] "19 left"
    [1] "Current mediator: Staphylococcus"

    Running nonparametric bootstrap

    [1] "18 left"
    [1] "Current mediator: Streptococcus bovis et rel."

    Running nonparametric bootstrap

    [1] "17 left"
    [1] "Current mediator: Streptococcus intermedius et rel."

    Running nonparametric bootstrap

    [1] "16 left"
    [1] "Current mediator: Streptococcus mitis et rel."

    Running nonparametric bootstrap

    [1] "15 left"
    [1] "Current mediator: Subdoligranulum variable at rel."

    Running nonparametric bootstrap

    [1] "14 left"
    [1] "Current mediator: Sutterella wadsworthia et rel."

    Running nonparametric bootstrap

    [1] "13 left"
    [1] "Current mediator: Tannerella et rel."

    Running nonparametric bootstrap

    [1] "12 left"
    [1] "Current mediator: Uncultured Bacteroidetes"

    Running nonparametric bootstrap

    [1] "11 left"
    [1] "Current mediator: Uncultured Chroococcales"

    Running nonparametric bootstrap

    [1] "10 left"
    [1] "Current mediator: Uncultured Clostridiales I"

    Running nonparametric bootstrap

    [1] "9 left"
    [1] "Current mediator: Uncultured Clostridiales II"

    Running nonparametric bootstrap

    [1] "8 left"
    [1] "Current mediator: Uncultured Mollicutes"

    Running nonparametric bootstrap

    [1] "7 left"
    [1] "Current mediator: Uncultured Selenomonadaceae"

    Running nonparametric bootstrap

    [1] "6 left"
    [1] "Current mediator: Veillonella"

    Running nonparametric bootstrap

    [1] "5 left"
    [1] "Current mediator: Vibrio"

    Running nonparametric bootstrap

    [1] "4 left"
    [1] "Current mediator: Weissella et rel."

    Running nonparametric bootstrap

    [1] "3 left"
    [1] "Current mediator: Wissella et rel."

    Running nonparametric bootstrap

    [1] "2 left"
    [1] "Current mediator: Xanthomonadaceae"

    Running nonparametric bootstrap

    [1] "1 left"
    [1] "Current mediator: Yersinia et rel."

    Running nonparametric bootstrap

``` r
hima_res <- mediate_hima(A = tse$nationality,
                         M = t(assay(tse, "clr")),
                         Y = tse$bmi_group)
```

    Screening mediators...

    Fitting outcome model with MCP...

    Fitting mediator models...

``` r
hdma_res <- mediate_hdma(A = tse$nationality,
                         M = t(assay(tse, "clr")),
                         Y = tse$bmi_group)
```

    Screening mediators...

    Fitting outcome model with de-biased LASSO...

    Fitting mediator models...

``` r
bslmm_res <- mediate_bslmm(A = tse$nationality,
                           M = t(assay(tse, "clr")),
                           Y = tse$bmi_group)
```

``` r
med_res %>%
  filter(ACME_adjpval > 0.05) %>%
  knitr::kable()
```

<table style="width:100%;">
<colgroup>
<col style="width: 7%" />
<col style="width: 23%" />
<col style="width: 6%" />
<col style="width: 8%" />
<col style="width: 8%" />
<col style="width: 6%" />
<col style="width: 6%" />
<col style="width: 8%" />
<col style="width: 7%" />
<col style="width: 8%" />
<col style="width: 8%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">Treatment</th>
<th style="text-align: left;">Mediator</th>
<th style="text-align: left;">Outcome</th>
<th style="text-align: right;">ACME_estimate</th>
<th style="text-align: right;">ADE_estimate</th>
<th style="text-align: right;">ACME_pval</th>
<th style="text-align: right;">ADE_pval</th>
<th style="text-align: right;">ACME_adjpval</th>
<th style="text-align: right;">ADE_adjpval</th>
<th style="text-align: right;">ACME_CI_lower</th>
<th style="text-align: right;">ADE_CI_lower</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Mollicutes</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.1700134</td>
<td style="text-align: right;">-0.1986718</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.2133333</td>
<td style="text-align: right;">0.2888889</td>
<td style="text-align: right;">0.2133333</td>
<td style="text-align: right;">-0.0469092</td>
<td style="text-align: right;">0.1062497</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium ventriosum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.1087084</td>
<td style="text-align: right;">-0.2599768</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.0866667</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0901333</td>
<td style="text-align: right;">-0.0100311</td>
<td style="text-align: right;">0.0607181</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium cylindroides et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1010478</td>
<td style="text-align: right;">-0.4697330</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.2347390</td>
<td style="text-align: right;">-0.1674882</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lactobacillus salivarius et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0816559</td>
<td style="text-align: right;">-0.4503411</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1957793</td>
<td style="text-align: right;">-0.1609730</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides uniformis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.2371214</td>
<td style="text-align: right;">-0.6058066</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.4490582</td>
<td style="text-align: right;">-0.2087500</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Selenomonadaceae</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1084270</td>
<td style="text-align: right;">-0.4771122</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.2127692</td>
<td style="text-align: right;">-0.1225544</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Parabacteroides distasonis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.2731733</td>
<td style="text-align: right;">-0.6418585</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.4814815</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.5877062</td>
<td style="text-align: right;">-0.2688401</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Catenibacterium mitsuokai et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0810081</td>
<td style="text-align: right;">-0.2876771</td>
<td style="text-align: right;">0.0466667</td>
<td style="text-align: right;">0.0800000</td>
<td style="text-align: right;">0.5515152</td>
<td style="text-align: right;">0.0845528</td>
<td style="text-align: right;">-0.0009472</td>
<td style="text-align: right;">0.0257356</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Veillonella</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1051677</td>
<td style="text-align: right;">-0.4738529</td>
<td style="text-align: right;">0.0466667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.5515152</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.2257384</td>
<td style="text-align: right;">-0.1171286</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides ovatus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.2913258</td>
<td style="text-align: right;">-0.6600110</td>
<td style="text-align: right;">0.0533333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.5777778</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.6598722</td>
<td style="text-align: right;">-0.1669522</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Vibrio</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0881878</td>
<td style="text-align: right;">-0.2804974</td>
<td style="text-align: right;">0.0666667</td>
<td style="text-align: right;">0.0733333</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0781421</td>
<td style="text-align: right;">0.0005820</td>
<td style="text-align: right;">0.0456743</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Subdoligranulum variable at rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0576737</td>
<td style="text-align: right;">-0.3110115</td>
<td style="text-align: right;">0.0800000</td>
<td style="text-align: right;">0.0400000</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0468468</td>
<td style="text-align: right;">0.0041679</td>
<td style="text-align: right;">-0.0069662</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Moraxellaceae</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0680072</td>
<td style="text-align: right;">-0.3006780</td>
<td style="text-align: right;">0.0933333</td>
<td style="text-align: right;">0.0533333</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0592593</td>
<td style="text-align: right;">0.0078491</td>
<td style="text-align: right;">0.0075063</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Coprobacillus catenaformis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0729253</td>
<td style="text-align: right;">-0.4416105</td>
<td style="text-align: right;">0.0933333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1800332</td>
<td style="text-align: right;">-0.1490534</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Faecalibacterium prausnitzii et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0807692</td>
<td style="text-align: right;">-0.4494544</td>
<td style="text-align: right;">0.0933333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1967889</td>
<td style="text-align: right;">-0.1626121</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Clostridiales I</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0710622</td>
<td style="text-align: right;">-0.2976229</td>
<td style="text-align: right;">0.1000000</td>
<td style="text-align: right;">0.0866667</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0901333</td>
<td style="text-align: right;">0.0051463</td>
<td style="text-align: right;">0.0262994</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Aquabacterium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0636127</td>
<td style="text-align: right;">-0.3050725</td>
<td style="text-align: right;">0.1000000</td>
<td style="text-align: right;">0.0733333</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0781421</td>
<td style="text-align: right;">0.0033213</td>
<td style="text-align: right;">0.0155824</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Akkermansia</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0619875</td>
<td style="text-align: right;">-0.4306726</td>
<td style="text-align: right;">0.1000000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1492730</td>
<td style="text-align: right;">-0.1034638</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bifidobacterium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0666756</td>
<td style="text-align: right;">-0.3020096</td>
<td style="text-align: right;">0.1066667</td>
<td style="text-align: right;">0.0466667</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0532164</td>
<td style="text-align: right;">0.0247529</td>
<td style="text-align: right;">-0.0093178</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Outgrouping clostridium cluster XIVa</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0713252</td>
<td style="text-align: right;">-0.4400104</td>
<td style="text-align: right;">0.1066667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.6303030</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.1848104</td>
<td style="text-align: right;">-0.1343877</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Anaerostipes caccae et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.1375087</td>
<td style="text-align: right;">-0.2311765</td>
<td style="text-align: right;">0.1400000</td>
<td style="text-align: right;">0.1666667</td>
<td style="text-align: right;">0.7583333</td>
<td style="text-align: right;">0.1692708</td>
<td style="text-align: right;">0.0817682</td>
<td style="text-align: right;">0.0902894</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Fusobacteria</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0654805</td>
<td style="text-align: right;">-0.4341657</td>
<td style="text-align: right;">0.1400000</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.7583333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1745841</td>
<td style="text-align: right;">-0.1466444</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Ruminococcus lactaris et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0688234</td>
<td style="text-align: right;">-0.2998618</td>
<td style="text-align: right;">0.1666667</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.8666667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0166896</td>
<td style="text-align: right;">-0.0325939</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium ramosum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0737938</td>
<td style="text-align: right;">-0.4424790</td>
<td style="text-align: right;">0.1733333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.8666667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1722299</td>
<td style="text-align: right;">-0.1284211</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Sporobacter termitidis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0491274</td>
<td style="text-align: right;">-0.4178125</td>
<td style="text-align: right;">0.2066667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1429040</td>
<td style="text-align: right;">-0.1209991</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Clostridiales II</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0924870</td>
<td style="text-align: right;">-0.2761981</td>
<td style="text-align: right;">0.2200000</td>
<td style="text-align: right;">0.1066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.1100529</td>
<td style="text-align: right;">0.0500721</td>
<td style="text-align: right;">0.0450655</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium nexile et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0448333</td>
<td style="text-align: right;">-0.4135185</td>
<td style="text-align: right;">0.2266667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1372085</td>
<td style="text-align: right;">-0.0878507</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium biforme et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.1456649</td>
<td style="text-align: right;">-0.2230203</td>
<td style="text-align: right;">0.2533333</td>
<td style="text-align: right;">0.1933333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.1948320</td>
<td style="text-align: right;">0.0873355</td>
<td style="text-align: right;">0.1671488</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Mitsuokella multiacida et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1371318</td>
<td style="text-align: right;">-0.5058169</td>
<td style="text-align: right;">0.2533333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.3901005</td>
<td style="text-align: right;">-0.1245955</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Bacteroidetes</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0329445</td>
<td style="text-align: right;">-0.4016297</td>
<td style="text-align: right;">0.2733333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1516519</td>
<td style="text-align: right;">-0.1182239</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium rectale et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0373185</td>
<td style="text-align: right;">-0.4060037</td>
<td style="text-align: right;">0.2933333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1273699</td>
<td style="text-align: right;">-0.0876591</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Pseudomonas</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0303800</td>
<td style="text-align: right;">-0.3383052</td>
<td style="text-align: right;">0.3000000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0243551</td>
<td style="text-align: right;">-0.0324886</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Tannerella et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1015194</td>
<td style="text-align: right;">-0.4702046</td>
<td style="text-align: right;">0.3200000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.4300943</td>
<td style="text-align: right;">-0.1068021</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Peptostreptococcus micros et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0228727</td>
<td style="text-align: right;">-0.3458124</td>
<td style="text-align: right;">0.3266667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0228009</td>
<td style="text-align: right;">-0.0642397</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Wissella et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0220317</td>
<td style="text-align: right;">-0.3907169</td>
<td style="text-align: right;">0.3533333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0896389</td>
<td style="text-align: right;">-0.1070718</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Anaerovorax odorimutans et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0552828</td>
<td style="text-align: right;">-0.4239680</td>
<td style="text-align: right;">0.3533333</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.1878242</td>
<td style="text-align: right;">-0.1069029</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium (sensu stricto)</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1059500</td>
<td style="text-align: right;">-0.4746351</td>
<td style="text-align: right;">0.3533333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.3278987</td>
<td style="text-align: right;">-0.1128634</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lactobacillus catenaformis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0238394</td>
<td style="text-align: right;">-0.3925246</td>
<td style="text-align: right;">0.3600000</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.0834191</td>
<td style="text-align: right;">-0.0989814</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides plebeius et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1394144</td>
<td style="text-align: right;">-0.5080995</td>
<td style="text-align: right;">0.3666667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.5251940</td>
<td style="text-align: right;">-0.0697707</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Enterococcus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0184348</td>
<td style="text-align: right;">-0.3502504</td>
<td style="text-align: right;">0.3800000</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0161950</td>
<td style="text-align: right;">-0.0541898</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Prevotella ruminicola et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0400929</td>
<td style="text-align: right;">-0.4087781</td>
<td style="text-align: right;">0.3800000</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1874458</td>
<td style="text-align: right;">-0.1172571</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Escherichia coli et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0171406</td>
<td style="text-align: right;">-0.3515445</td>
<td style="text-align: right;">0.4066667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0329922</td>
<td style="text-align: right;">-0.0519915</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides splachnicus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0581411</td>
<td style="text-align: right;">-0.4268263</td>
<td style="text-align: right;">0.4066667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.2053635</td>
<td style="text-align: right;">-0.1257274</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium symbiosum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0235306</td>
<td style="text-align: right;">-0.3451546</td>
<td style="text-align: right;">0.4133333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0356797</td>
<td style="text-align: right;">-0.0709783</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium stercorarium et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0608823</td>
<td style="text-align: right;">-0.4295675</td>
<td style="text-align: right;">0.4133333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.2043017</td>
<td style="text-align: right;">-0.0740700</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Coprococcus eutactus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0227789</td>
<td style="text-align: right;">-0.3914641</td>
<td style="text-align: right;">0.4200000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.1042625</td>
<td style="text-align: right;">-0.0437030</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bulleidia moorei et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0524521</td>
<td style="text-align: right;">-0.4211373</td>
<td style="text-align: right;">0.4200000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1796617</td>
<td style="text-align: right;">-0.0609233</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Ruminococcus obeum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0240364</td>
<td style="text-align: right;">-0.3927216</td>
<td style="text-align: right;">0.4533333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1150707</td>
<td style="text-align: right;">-0.0693023</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Butyrivibrio crossotus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0162043</td>
<td style="text-align: right;">-0.3524809</td>
<td style="text-align: right;">0.4666667</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0226103</td>
<td style="text-align: right;">-0.0661650</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Prevotella tannerae et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0671689</td>
<td style="text-align: right;">-0.4358540</td>
<td style="text-align: right;">0.4666667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.2924273</td>
<td style="text-align: right;">-0.0906168</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Sutterella wadsworthia et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0204602</td>
<td style="text-align: right;">-0.3482250</td>
<td style="text-align: right;">0.4733333</td>
<td style="text-align: right;">0.0466667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0532164</td>
<td style="text-align: right;">0.0519577</td>
<td style="text-align: right;">-0.0381470</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Weissella et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0236232</td>
<td style="text-align: right;">-0.3923083</td>
<td style="text-align: right;">0.4733333</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.1119960</td>
<td style="text-align: right;">-0.1121379</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium orbiscindens et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0433191</td>
<td style="text-align: right;">-0.3253661</td>
<td style="text-align: right;">0.4800000</td>
<td style="text-align: right;">0.0533333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0592593</td>
<td style="text-align: right;">0.0731316</td>
<td style="text-align: right;">0.0058615</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Anaerofustis</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0198228</td>
<td style="text-align: right;">-0.3488624</td>
<td style="text-align: right;">0.4933333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0312179</td>
<td style="text-align: right;">-0.0399034</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Brachyspira</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0110066</td>
<td style="text-align: right;">-0.3576786</td>
<td style="text-align: right;">0.4933333</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0138037</td>
<td style="text-align: right;">-0.0354221</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides vulgatus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.1140091</td>
<td style="text-align: right;">-0.4826942</td>
<td style="text-align: right;">0.4933333</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.5040200</td>
<td style="text-align: right;">-0.0499070</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Collinsella</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0495148</td>
<td style="text-align: right;">-0.4182000</td>
<td style="text-align: right;">0.5400000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.2835373</td>
<td style="text-align: right;">-0.1128923</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Anaerotruncus colihominis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0344459</td>
<td style="text-align: right;">-0.3342393</td>
<td style="text-align: right;">0.5466667</td>
<td style="text-align: right;">0.0533333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0592593</td>
<td style="text-align: right;">0.0792840</td>
<td style="text-align: right;">0.0010980</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Oscillospira guillermondii et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0409181</td>
<td style="text-align: right;">-0.4096033</td>
<td style="text-align: right;">0.5466667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.2068265</td>
<td style="text-align: right;">-0.0843820</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lachnospira pectinoschiza et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0148027</td>
<td style="text-align: right;">-0.3538825</td>
<td style="text-align: right;">0.5533333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0352349</td>
<td style="text-align: right;">-0.0614745</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Prevotella melaninogenica et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0505853</td>
<td style="text-align: right;">-0.4192704</td>
<td style="text-align: right;">0.5733333</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.1849644</td>
<td style="text-align: right;">-0.0677586</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Leminorella</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0165557</td>
<td style="text-align: right;">-0.3852408</td>
<td style="text-align: right;">0.5800000</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0926021</td>
<td style="text-align: right;">-0.0482234</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Prevotella oralis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0266790</td>
<td style="text-align: right;">-0.3953641</td>
<td style="text-align: right;">0.6000000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1744174</td>
<td style="text-align: right;">-0.0843732</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium hallii et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0123497</td>
<td style="text-align: right;">-0.3810348</td>
<td style="text-align: right;">0.6066667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0768191</td>
<td style="text-align: right;">-0.0822367</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium limosum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0106560</td>
<td style="text-align: right;">-0.3793412</td>
<td style="text-align: right;">0.6200000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0691956</td>
<td style="text-align: right;">-0.0825364</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Roseburia intestinalis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0137926</td>
<td style="text-align: right;">-0.3548926</td>
<td style="text-align: right;">0.6266667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0451731</td>
<td style="text-align: right;">-0.0410518</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Peptococcus niger et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0128096</td>
<td style="text-align: right;">-0.3558755</td>
<td style="text-align: right;">0.6600000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0517664</td>
<td style="text-align: right;">-0.0546970</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium cellulosi et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0141688</td>
<td style="text-align: right;">-0.3828540</td>
<td style="text-align: right;">0.6600000</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0945312</td>
<td style="text-align: right;">-0.0652904</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium difficile et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0553960</td>
<td style="text-align: right;">-0.4240812</td>
<td style="text-align: right;">0.6600000</td>
<td style="text-align: right;">0.0400000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0468468</td>
<td style="text-align: right;">0.3310551</td>
<td style="text-align: right;">-0.0196195</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Oxalobacter formigenes et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0117605</td>
<td style="text-align: right;">-0.3804456</td>
<td style="text-align: right;">0.6666667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0766744</td>
<td style="text-align: right;">-0.0967187</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Helicobacter</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0077597</td>
<td style="text-align: right;">-0.3609254</td>
<td style="text-align: right;">0.6933333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0417985</td>
<td style="text-align: right;">-0.0871358</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Haemophilus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0117372</td>
<td style="text-align: right;">-0.3804223</td>
<td style="text-align: right;">0.6933333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1001194</td>
<td style="text-align: right;">-0.0810392</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Oceanospirillum</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0192127</td>
<td style="text-align: right;">-0.3494725</td>
<td style="text-align: right;">0.7000000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.1199549</td>
<td style="text-align: right;">-0.0258638</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Yersinia et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0114788</td>
<td style="text-align: right;">-0.3572064</td>
<td style="text-align: right;">0.7000000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0372692</td>
<td style="text-align: right;">-0.0796140</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacillus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0088320</td>
<td style="text-align: right;">-0.3598532</td>
<td style="text-align: right;">0.7000000</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0246483</td>
<td style="text-align: right;">-0.0314224</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Xanthomonadaceae</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0094046</td>
<td style="text-align: right;">-0.3780898</td>
<td style="text-align: right;">0.7133333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0646698</td>
<td style="text-align: right;">-0.0596643</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Gemella</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0053426</td>
<td style="text-align: right;">-0.3633426</td>
<td style="text-align: right;">0.7400000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0220134</td>
<td style="text-align: right;">-0.1146250</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides intestinalis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0368383</td>
<td style="text-align: right;">-0.3318469</td>
<td style="text-align: right;">0.7466667</td>
<td style="text-align: right;">0.0600000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0655462</td>
<td style="text-align: right;">0.2356433</td>
<td style="text-align: right;">0.0270679</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Burkholderia</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0079128</td>
<td style="text-align: right;">-0.3765980</td>
<td style="text-align: right;">0.7466667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0000000</td>
<td style="text-align: right;">0.0687836</td>
<td style="text-align: right;">-0.0518300</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Ruminococcus callidus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0151452</td>
<td style="text-align: right;">-0.3838303</td>
<td style="text-align: right;">0.7600000</td>
<td style="text-align: right;">0.0400000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0468468</td>
<td style="text-align: right;">0.0864179</td>
<td style="text-align: right;">-0.0161894</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium colinum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0055411</td>
<td style="text-align: right;">-0.3742263</td>
<td style="text-align: right;">0.7666667</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0551185</td>
<td style="text-align: right;">-0.0273244</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lachnobacillus bovis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0057211</td>
<td style="text-align: right;">-0.3744063</td>
<td style="text-align: right;">0.7666667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0414530</td>
<td style="text-align: right;">-0.0744500</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bilophila et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0111722</td>
<td style="text-align: right;">-0.3575130</td>
<td style="text-align: right;">0.7933333</td>
<td style="text-align: right;">0.0600000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0655462</td>
<td style="text-align: right;">0.0797752</td>
<td style="text-align: right;">0.0129599</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Methylobacterium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.8066667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0325928</td>
<td style="text-align: right;">-0.0504707</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Proteus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0006735</td>
<td style="text-align: right;">-0.3680117</td>
<td style="text-align: right;">0.8133333</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0234697</td>
<td style="text-align: right;">-0.0484837</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Dialister</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0038718</td>
<td style="text-align: right;">-0.3725570</td>
<td style="text-align: right;">0.8200000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0487567</td>
<td style="text-align: right;">-0.0474292</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Actinomycetaceae</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0040535</td>
<td style="text-align: right;">-0.3646316</td>
<td style="text-align: right;">0.8266667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0375649</td>
<td style="text-align: right;">-0.1114547</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Staphylococcus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0043939</td>
<td style="text-align: right;">-0.3642913</td>
<td style="text-align: right;">0.8400000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0512816</td>
<td style="text-align: right;">-0.0856615</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Peptostreptococcus anaerobius et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.8466667</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0334936</td>
<td style="text-align: right;">-0.0444806</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Klebisiella pneumoniae et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0183558</td>
<td style="text-align: right;">-0.3503293</td>
<td style="text-align: right;">0.8600000</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1326905</td>
<td style="text-align: right;">-0.0622061</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Aerococcus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.8600000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0382899</td>
<td style="text-align: right;">-0.0766503</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium felsineum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.8666667</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0357058</td>
<td style="text-align: right;">-0.0304379</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Serratia</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0123754</td>
<td style="text-align: right;">-0.3810606</td>
<td style="text-align: right;">0.8666667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.1325317</td>
<td style="text-align: right;">-0.0471117</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Alcaligenes faecalis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0007673</td>
<td style="text-align: right;">-0.3679179</td>
<td style="text-align: right;">0.8733333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0191946</td>
<td style="text-align: right;">-0.0528163</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Propionibacterium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0121702</td>
<td style="text-align: right;">-0.3565150</td>
<td style="text-align: right;">0.8800000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.1096501</td>
<td style="text-align: right;">-0.0665660</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium sphenoides et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0021794</td>
<td style="text-align: right;">-0.3708646</td>
<td style="text-align: right;">0.8800000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0291286</td>
<td style="text-align: right;">-0.0719287</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Campylobacter</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0052035</td>
<td style="text-align: right;">-0.3738887</td>
<td style="text-align: right;">0.8800000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0705234</td>
<td style="text-align: right;">-0.0358812</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Aeromonas</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0000862</td>
<td style="text-align: right;">-0.3685990</td>
<td style="text-align: right;">0.8866667</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0261850</td>
<td style="text-align: right;">-0.0413728</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Ruminococcus gnavus et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0038597</td>
<td style="text-align: right;">-0.3725449</td>
<td style="text-align: right;">0.8866667</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0459640</td>
<td style="text-align: right;">-0.0709946</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Streptococcus mitis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0039853</td>
<td style="text-align: right;">-0.3726704</td>
<td style="text-align: right;">0.8866667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0520430</td>
<td style="text-align: right;">-0.0685653</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Streptococcus intermedius et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0045051</td>
<td style="text-align: right;">-0.3641801</td>
<td style="text-align: right;">0.8933333</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0296610</td>
<td style="text-align: right;">-0.0566826</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Megasphaera elsdenii et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0088168</td>
<td style="text-align: right;">-0.3598684</td>
<td style="text-align: right;">0.9000000</td>
<td style="text-align: right;">0.0466667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0532164</td>
<td style="text-align: right;">0.1317738</td>
<td style="text-align: right;">-0.0079247</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Allistipes et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0077629</td>
<td style="text-align: right;">-0.3609222</td>
<td style="text-align: right;">0.9000000</td>
<td style="text-align: right;">0.0666667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0722222</td>
<td style="text-align: right;">0.3114176</td>
<td style="text-align: right;">0.0441529</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Micrococcaceae</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.9000000</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0302505</td>
<td style="text-align: right;">-0.0897296</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Ruminococcus bromii et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0074579</td>
<td style="text-align: right;">-0.3612272</td>
<td style="text-align: right;">0.9066667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0974795</td>
<td style="text-align: right;">-0.0848720</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium thermocellum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.9066667</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0403502</td>
<td style="text-align: right;">-0.0274845</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Enterobacter aerogenes et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0124183</td>
<td style="text-align: right;">-0.3811034</td>
<td style="text-align: right;">0.9066667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.1164188</td>
<td style="text-align: right;">-0.0936790</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Atopobium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0025358</td>
<td style="text-align: right;">-0.3661494</td>
<td style="text-align: right;">0.9133333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0374358</td>
<td style="text-align: right;">-0.0958986</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Corynebacterium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.9266667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0383133</td>
<td style="text-align: right;">-0.0575314</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eubacterium siraeum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0047040</td>
<td style="text-align: right;">-0.3733892</td>
<td style="text-align: right;">0.9266667</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0542076</td>
<td style="text-align: right;">-0.0452036</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Granulicatella</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0007740</td>
<td style="text-align: right;">-0.3694592</td>
<td style="text-align: right;">0.9333333</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0317960</td>
<td style="text-align: right;">-0.0506668</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bacteroides stercoris et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0013727</td>
<td style="text-align: right;">-0.3700579</td>
<td style="text-align: right;">0.9333333</td>
<td style="text-align: right;">0.1266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.1296588</td>
<td style="text-align: right;">0.2652816</td>
<td style="text-align: right;">0.0563904</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Phascolarctobacterium faecium et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0016171</td>
<td style="text-align: right;">-0.3670681</td>
<td style="text-align: right;">0.9466667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0311594</td>
<td style="text-align: right;">-0.0949069</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Megamonas hypermegale et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0043221</td>
<td style="text-align: right;">-0.3730073</td>
<td style="text-align: right;">0.9466667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0617295</td>
<td style="text-align: right;">-0.0875987</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Bryantella formatexigens et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0018073</td>
<td style="text-align: right;">-0.3704925</td>
<td style="text-align: right;">0.9533333</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0962781</td>
<td style="text-align: right;">-0.0870306</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Eggerthella lenta et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0018860</td>
<td style="text-align: right;">-0.3705711</td>
<td style="text-align: right;">0.9533333</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.1155429</td>
<td style="text-align: right;">-0.0590148</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Uncultured Chroococcales</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0021692</td>
<td style="text-align: right;">-0.3708543</td>
<td style="text-align: right;">0.9533333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0435110</td>
<td style="text-align: right;">-0.0920718</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Anaerobiospirillum</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0027313</td>
<td style="text-align: right;">-0.3714165</td>
<td style="text-align: right;">0.9533333</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0313140</td>
<td style="text-align: right;">-0.0543191</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Novosphingobium</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0034040</td>
<td style="text-align: right;">-0.3720892</td>
<td style="text-align: right;">0.9533333</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0366744</td>
<td style="text-align: right;">-0.0175572</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Desulfovibrio et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0004363</td>
<td style="text-align: right;">-0.3682489</td>
<td style="text-align: right;">0.9600000</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0515415</td>
<td style="text-align: right;">-0.0630910</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Asteroleplasma et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.9666667</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0359708</td>
<td style="text-align: right;">-0.0790038</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Aneurinibacillus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0000738</td>
<td style="text-align: right;">-0.3687590</td>
<td style="text-align: right;">0.9666667</td>
<td style="text-align: right;">0.0133333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0279570</td>
<td style="text-align: right;">0.0390957</td>
<td style="text-align: right;">-0.0751073</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lactobacillus plantarum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0041206</td>
<td style="text-align: right;">-0.3728058</td>
<td style="text-align: right;">0.9666667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0240167</td>
<td style="text-align: right;">-0.0506696</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lactobacillus gasseri et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0009784</td>
<td style="text-align: right;">-0.3677068</td>
<td style="text-align: right;">0.9733333</td>
<td style="text-align: right;">0.0266667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0357388</td>
<td style="text-align: right;">0.0971511</td>
<td style="text-align: right;">-0.0313899</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Dorea formicigenerans et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0008543</td>
<td style="text-align: right;">-0.3695395</td>
<td style="text-align: right;">0.9733333</td>
<td style="text-align: right;">0.0333333</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0401235</td>
<td style="text-align: right;">0.0669091</td>
<td style="text-align: right;">-0.1011312</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Lactococcus</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">0.0014758</td>
<td style="text-align: right;">-0.3701610</td>
<td style="text-align: right;">0.9733333</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0604953</td>
<td style="text-align: right;">-0.0686680</td>
</tr>
<tr class="odd">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Clostridium leptum et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0016329</td>
<td style="text-align: right;">-0.3670523</td>
<td style="text-align: right;">0.9800000</td>
<td style="text-align: right;">0.0200000</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0305882</td>
<td style="text-align: right;">0.0439087</td>
<td style="text-align: right;">-0.0630362</td>
</tr>
<tr class="even">
<td style="text-align: left;">nationality</td>
<td style="text-align: left;">Streptococcus bovis et rel.</td>
<td style="text-align: left;">bmi_group</td>
<td style="text-align: right;">-0.0033257</td>
<td style="text-align: right;">-0.3653594</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0066667</td>
<td style="text-align: right;">0.9866667</td>
<td style="text-align: right;">0.0222222</td>
<td style="text-align: right;">0.0678576</td>
<td style="text-align: right;">-0.0876568</td>
</tr>
</tbody>
</table>

``` r
hima_res$contributions %>% knitr::kable()
```

<table style="width:100%;">
<colgroup>
<col style="width: 37%" />
<col style="width: 11%" />
<col style="width: 10%" />
<col style="width: 11%" />
<col style="width: 10%" />
<col style="width: 11%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">mediator</th>
<th style="text-align: right;">alpha</th>
<th style="text-align: right;">alpha_pv</th>
<th style="text-align: right;">beta</th>
<th style="text-align: right;">beta_pv</th>
<th style="text-align: right;">alpha_beta</th>
<th style="text-align: right;">ab_pv</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Bifidobacterium</td>
<td style="text-align: right;">-0.4380697</td>
<td style="text-align: right;">0.0105576</td>
<td style="text-align: right;">0.1238433</td>
<td style="text-align: right;">0.0171596</td>
<td style="text-align: right;">-0.0542520</td>
<td style="text-align: right;">0.0171596</td>
</tr>
<tr class="even">
<td style="text-align: left;">Burkholderia</td>
<td style="text-align: right;">0.0392608</td>
<td style="text-align: right;">0.6915671</td>
<td style="text-align: right;">0.3731936</td>
<td style="text-align: right;">0.0006277</td>
<td style="text-align: right;">0.0146519</td>
<td style="text-align: right;">0.6915671</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Coprobacillus catenaformis et rel.</td>
<td style="text-align: right;">-0.1979582</td>
<td style="text-align: right;">0.0009740</td>
<td style="text-align: right;">-0.1939620</td>
<td style="text-align: right;">0.0630029</td>
<td style="text-align: right;">0.0383964</td>
<td style="text-align: right;">0.0630029</td>
</tr>
<tr class="even">
<td style="text-align: left;">Desulfovibrio et rel.</td>
<td style="text-align: right;">0.0012995</td>
<td style="text-align: right;">0.9886573</td>
<td style="text-align: right;">-0.4233297</td>
<td style="text-align: right;">0.0013529</td>
<td style="text-align: right;">-0.0005501</td>
<td style="text-align: right;">0.9886573</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Faecalibacterium prausnitzii et rel.</td>
<td style="text-align: right;">0.2876538</td>
<td style="text-align: right;">0.0979448</td>
<td style="text-align: right;">0.1771072</td>
<td style="text-align: right;">0.0095581</td>
<td style="text-align: right;">0.0509455</td>
<td style="text-align: right;">0.0979448</td>
</tr>
<tr class="even">
<td style="text-align: left;">Moraxellaceae</td>
<td style="text-align: right;">0.2317972</td>
<td style="text-align: right;">0.0072155</td>
<td style="text-align: right;">-0.6367405</td>
<td style="text-align: right;">0.0001697</td>
<td style="text-align: right;">-0.1475946</td>
<td style="text-align: right;">0.0072155</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Oxalobacter formigenes et rel.</td>
<td style="text-align: right;">-0.0656589</td>
<td style="text-align: right;">0.7032167</td>
<td style="text-align: right;">-0.0539696</td>
<td style="text-align: right;">0.0806198</td>
<td style="text-align: right;">0.0035436</td>
<td style="text-align: right;">0.7032167</td>
</tr>
<tr class="even">
<td style="text-align: left;">Papillibacter cinnamivorans et rel.</td>
<td style="text-align: right;">0.4028622</td>
<td style="text-align: right;">0.0004404</td>
<td style="text-align: right;">0.5006851</td>
<td style="text-align: right;">0.0000079</td>
<td style="text-align: right;">0.2017071</td>
<td style="text-align: right;">0.0004404</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pseudomonas</td>
<td style="text-align: right;">-0.0631210</td>
<td style="text-align: right;">0.1785891</td>
<td style="text-align: right;">1.5889309</td>
<td style="text-align: right;">0.0000001</td>
<td style="text-align: right;">-0.1002948</td>
<td style="text-align: right;">0.1785891</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ruminococcus lactaris et rel.</td>
<td style="text-align: right;">0.4230369</td>
<td style="text-align: right;">0.0050106</td>
<td style="text-align: right;">-0.3220227</td>
<td style="text-align: right;">0.0001546</td>
<td style="text-align: right;">-0.1362275</td>
<td style="text-align: right;">0.0050106</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Uncultured Bacteroidetes</td>
<td style="text-align: right;">-0.2816935</td>
<td style="text-align: right;">0.0492007</td>
<td style="text-align: right;">-0.1840729</td>
<td style="text-align: right;">0.0112849</td>
<td style="text-align: right;">0.0518521</td>
<td style="text-align: right;">0.0492007</td>
</tr>
</tbody>
</table>

``` r
hdma_res$contributions %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 36%" />
<col style="width: 11%" />
<col style="width: 10%" />
<col style="width: 11%" />
<col style="width: 10%" />
<col style="width: 11%" />
<col style="width: 10%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">mediator</th>
<th style="text-align: right;">alpha</th>
<th style="text-align: right;">alpha_pv</th>
<th style="text-align: right;">beta</th>
<th style="text-align: right;">beta_pv</th>
<th style="text-align: right;">alpha_beta</th>
<th style="text-align: right;">ab_pv</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Burkholderia</td>
<td style="text-align: right;">0.0392608</td>
<td style="text-align: right;">0.6915671</td>
<td style="text-align: right;">0.3053534</td>
<td style="text-align: right;">0.0206205</td>
<td style="text-align: right;">0.0119884</td>
<td style="text-align: right;">0.6915671</td>
</tr>
<tr class="even">
<td style="text-align: left;">Moraxellaceae</td>
<td style="text-align: right;">0.2317972</td>
<td style="text-align: right;">0.0072155</td>
<td style="text-align: right;">-0.5291301</td>
<td style="text-align: right;">0.0011169</td>
<td style="text-align: right;">-0.1226508</td>
<td style="text-align: right;">0.0072155</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Oxalobacter formigenes et rel.</td>
<td style="text-align: right;">-0.0656589</td>
<td style="text-align: right;">0.7032167</td>
<td style="text-align: right;">-0.1706228</td>
<td style="text-align: right;">0.0473212</td>
<td style="text-align: right;">0.0112029</td>
<td style="text-align: right;">0.7032167</td>
</tr>
<tr class="even">
<td style="text-align: left;">Papillibacter cinnamivorans et rel.</td>
<td style="text-align: right;">0.4028622</td>
<td style="text-align: right;">0.0004404</td>
<td style="text-align: right;">0.2827309</td>
<td style="text-align: right;">0.0251952</td>
<td style="text-align: right;">0.1139016</td>
<td style="text-align: right;">0.0251952</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pseudomonas</td>
<td style="text-align: right;">-0.0631210</td>
<td style="text-align: right;">0.1785891</td>
<td style="text-align: right;">1.4132186</td>
<td style="text-align: right;">0.0000438</td>
<td style="text-align: right;">-0.0892037</td>
<td style="text-align: right;">0.1785891</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ruminococcus lactaris et rel.</td>
<td style="text-align: right;">0.4230369</td>
<td style="text-align: right;">0.0050106</td>
<td style="text-align: right;">-0.2918649</td>
<td style="text-align: right;">0.0024087</td>
<td style="text-align: right;">-0.1234696</td>
<td style="text-align: right;">0.0050106</td>
</tr>
</tbody>
</table>

``` r
bslmm_res$contributions %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 32%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 9%" />
<col style="width: 13%" />
<col style="width: 9%" />
<col style="width: 10%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">mediator</th>
<th style="text-align: right;">alpha</th>
<th style="text-align: right;">beta</th>
<th style="text-align: right;">alpha_beta</th>
<th style="text-align: right;">ab_posterior_sd</th>
<th style="text-align: right;">ab_cl_2.5%</th>
<th style="text-align: right;">ab_cl_97.5%</th>
<th style="text-align: right;">ab_pip</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">Actinomycetaceae</td>
<td style="text-align: right;">-0.0064004</td>
<td style="text-align: right;">0.0187832</td>
<td style="text-align: right;">-0.0000683</td>
<td style="text-align: right;">0.0095187</td>
<td style="text-align: right;">-0.0166228</td>
<td style="text-align: right;">0.0174167</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Aerococcus</td>
<td style="text-align: right;">-0.0323810</td>
<td style="text-align: right;">0.0041305</td>
<td style="text-align: right;">-0.0005145</td>
<td style="text-align: right;">0.0100180</td>
<td style="text-align: right;">-0.0183961</td>
<td style="text-align: right;">0.0150430</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Aeromonas</td>
<td style="text-align: right;">-0.0188035</td>
<td style="text-align: right;">0.0019263</td>
<td style="text-align: right;">0.0000076</td>
<td style="text-align: right;">0.0080310</td>
<td style="text-align: right;">-0.0155614</td>
<td style="text-align: right;">0.0163953</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Akkermansia</td>
<td style="text-align: right;">-0.5998447</td>
<td style="text-align: right;">-0.0259923</td>
<td style="text-align: right;">0.0156638</td>
<td style="text-align: right;">0.0275887</td>
<td style="text-align: right;">-0.0354481</td>
<td style="text-align: right;">0.0752743</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Alcaligenes faecalis et rel.</td>
<td style="text-align: right;">0.0479666</td>
<td style="text-align: right;">0.0053314</td>
<td style="text-align: right;">0.0003200</td>
<td style="text-align: right;">0.0066041</td>
<td style="text-align: right;">-0.0130006</td>
<td style="text-align: right;">0.0155319</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Allistipes et rel.</td>
<td style="text-align: right;">-1.5959369</td>
<td style="text-align: right;">0.0359392</td>
<td style="text-align: right;">-0.0572973</td>
<td style="text-align: right;">0.0903676</td>
<td style="text-align: right;">-0.2654565</td>
<td style="text-align: right;">0.0954810</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Anaerobiospirillum</td>
<td style="text-align: right;">-0.0200114</td>
<td style="text-align: right;">0.0006208</td>
<td style="text-align: right;">-0.0001429</td>
<td style="text-align: right;">0.0078417</td>
<td style="text-align: right;">-0.0172144</td>
<td style="text-align: right;">0.0160103</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Anaerofustis</td>
<td style="text-align: right;">-0.0666768</td>
<td style="text-align: right;">0.0110202</td>
<td style="text-align: right;">-0.0007352</td>
<td style="text-align: right;">0.0093324</td>
<td style="text-align: right;">-0.0198113</td>
<td style="text-align: right;">0.0160019</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Anaerostipes caccae et rel.</td>
<td style="text-align: right;">-0.8366101</td>
<td style="text-align: right;">0.0152104</td>
<td style="text-align: right;">-0.0128101</td>
<td style="text-align: right;">0.0440746</td>
<td style="text-align: right;">-0.1073684</td>
<td style="text-align: right;">0.0712619</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Anaerotruncus colihominis et rel.</td>
<td style="text-align: right;">0.7692119</td>
<td style="text-align: right;">-0.0136616</td>
<td style="text-align: right;">-0.0106663</td>
<td style="text-align: right;">0.0383260</td>
<td style="text-align: right;">-0.0912960</td>
<td style="text-align: right;">0.0656233</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Anaerovorax odorimutans et rel.</td>
<td style="text-align: right;">0.3310435</td>
<td style="text-align: right;">0.0143366</td>
<td style="text-align: right;">0.0047027</td>
<td style="text-align: right;">0.0207938</td>
<td style="text-align: right;">-0.0341946</td>
<td style="text-align: right;">0.0519864</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Aneurinibacillus</td>
<td style="text-align: right;">-0.0354397</td>
<td style="text-align: right;">0.0032450</td>
<td style="text-align: right;">0.0001572</td>
<td style="text-align: right;">0.0082044</td>
<td style="text-align: right;">-0.0165601</td>
<td style="text-align: right;">0.0177338</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Aquabacterium</td>
<td style="text-align: right;">0.1909403</td>
<td style="text-align: right;">-0.0425102</td>
<td style="text-align: right;">-0.0083027</td>
<td style="text-align: right;">0.0177334</td>
<td style="text-align: right;">-0.0524203</td>
<td style="text-align: right;">0.0145563</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Asteroleplasma et rel.</td>
<td style="text-align: right;">-0.0358603</td>
<td style="text-align: right;">0.0044718</td>
<td style="text-align: right;">-0.0002716</td>
<td style="text-align: right;">0.0086307</td>
<td style="text-align: right;">-0.0178276</td>
<td style="text-align: right;">0.0165794</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Atopobium</td>
<td style="text-align: right;">-0.0173005</td>
<td style="text-align: right;">0.0085729</td>
<td style="text-align: right;">-0.0001897</td>
<td style="text-align: right;">0.0082855</td>
<td style="text-align: right;">-0.0170467</td>
<td style="text-align: right;">0.0160060</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bacillus</td>
<td style="text-align: right;">-0.0385777</td>
<td style="text-align: right;">0.0106564</td>
<td style="text-align: right;">-0.0005606</td>
<td style="text-align: right;">0.0112072</td>
<td style="text-align: right;">-0.0182151</td>
<td style="text-align: right;">0.0160541</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bacteroides fragilis et rel.</td>
<td style="text-align: right;">-1.0999840</td>
<td style="text-align: right;">-0.0339800</td>
<td style="text-align: right;">0.0373694</td>
<td style="text-align: right;">0.0539590</td>
<td style="text-align: right;">-0.0642749</td>
<td style="text-align: right;">0.1499854</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bacteroides intestinalis et rel.</td>
<td style="text-align: right;">-1.5431270</td>
<td style="text-align: right;">0.0092984</td>
<td style="text-align: right;">-0.0143010</td>
<td style="text-align: right;">0.0780936</td>
<td style="text-align: right;">-0.1739648</td>
<td style="text-align: right;">0.1393527</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bacteroides ovatus et rel.</td>
<td style="text-align: right;">-1.6032258</td>
<td style="text-align: right;">-0.0180355</td>
<td style="text-align: right;">0.0288639</td>
<td style="text-align: right;">0.0848599</td>
<td style="text-align: right;">-0.1357026</td>
<td style="text-align: right;">0.2077539</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bacteroides plebeius et rel.</td>
<td style="text-align: right;">-1.2657064</td>
<td style="text-align: right;">0.0125737</td>
<td style="text-align: right;">-0.0158778</td>
<td style="text-align: right;">0.0693837</td>
<td style="text-align: right;">-0.1700896</td>
<td style="text-align: right;">0.1117357</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bacteroides splachnicus et rel.</td>
<td style="text-align: right;">-0.6293967</td>
<td style="text-align: right;">0.0029128</td>
<td style="text-align: right;">-0.0019287</td>
<td style="text-align: right;">0.0340573</td>
<td style="text-align: right;">-0.0709686</td>
<td style="text-align: right;">0.0651441</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bacteroides stercoris et rel.</td>
<td style="text-align: right;">-1.1760351</td>
<td style="text-align: right;">0.0065142</td>
<td style="text-align: right;">-0.0076254</td>
<td style="text-align: right;">0.0609081</td>
<td style="text-align: right;">-0.1319886</td>
<td style="text-align: right;">0.1159688</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bacteroides uniformis et rel.</td>
<td style="text-align: right;">-1.6549459</td>
<td style="text-align: right;">-0.0535385</td>
<td style="text-align: right;">0.0885860</td>
<td style="text-align: right;">0.0841609</td>
<td style="text-align: right;">-0.0530632</td>
<td style="text-align: right;">0.2795924</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bacteroides vulgatus et rel.</td>
<td style="text-align: right;">-2.2127110</td>
<td style="text-align: right;">-0.0117578</td>
<td style="text-align: right;">0.0260319</td>
<td style="text-align: right;">0.1041893</td>
<td style="text-align: right;">-0.1743165</td>
<td style="text-align: right;">0.2346749</td>
<td style="text-align: right;">6e-04</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bifidobacterium</td>
<td style="text-align: right;">-0.4167209</td>
<td style="text-align: right;">0.0569632</td>
<td style="text-align: right;">-0.0237199</td>
<td style="text-align: right;">0.0281106</td>
<td style="text-align: right;">-0.0911346</td>
<td style="text-align: right;">0.0156240</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bilophila et rel.</td>
<td style="text-align: right;">-0.1947374</td>
<td style="text-align: right;">0.0139574</td>
<td style="text-align: right;">-0.0027296</td>
<td style="text-align: right;">0.0138831</td>
<td style="text-align: right;">-0.0371368</td>
<td style="text-align: right;">0.0225061</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Brachyspira</td>
<td style="text-align: right;">0.0581524</td>
<td style="text-align: right;">-0.0087681</td>
<td style="text-align: right;">-0.0004448</td>
<td style="text-align: right;">0.0081292</td>
<td style="text-align: right;">-0.0180576</td>
<td style="text-align: right;">0.0154492</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Bryantella formatexigens et rel.</td>
<td style="text-align: right;">-0.4201066</td>
<td style="text-align: right;">-0.0155371</td>
<td style="text-align: right;">0.0064174</td>
<td style="text-align: right;">0.0225010</td>
<td style="text-align: right;">-0.0362045</td>
<td style="text-align: right;">0.0574402</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Bulleidia moorei et rel.</td>
<td style="text-align: right;">0.4456013</td>
<td style="text-align: right;">0.0362958</td>
<td style="text-align: right;">0.0162840</td>
<td style="text-align: right;">0.0281451</td>
<td style="text-align: right;">-0.0281470</td>
<td style="text-align: right;">0.0829743</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Burkholderia</td>
<td style="text-align: right;">0.0346825</td>
<td style="text-align: right;">0.0299625</td>
<td style="text-align: right;">0.0011937</td>
<td style="text-align: right;">0.0087990</td>
<td style="text-align: right;">-0.0154936</td>
<td style="text-align: right;">0.0219366</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Butyrivibrio crossotus et rel.</td>
<td style="text-align: right;">-0.1249935</td>
<td style="text-align: right;">0.0392071</td>
<td style="text-align: right;">-0.0048962</td>
<td style="text-align: right;">0.0113658</td>
<td style="text-align: right;">-0.0335325</td>
<td style="text-align: right;">0.0115994</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Campylobacter</td>
<td style="text-align: right;">-0.0706628</td>
<td style="text-align: right;">0.0025942</td>
<td style="text-align: right;">-0.0003329</td>
<td style="text-align: right;">0.0090231</td>
<td style="text-align: right;">-0.0190081</td>
<td style="text-align: right;">0.0178663</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Catenibacterium mitsuokai et rel.</td>
<td style="text-align: right;">0.4587087</td>
<td style="text-align: right;">-0.0526787</td>
<td style="text-align: right;">-0.0242725</td>
<td style="text-align: right;">0.0272492</td>
<td style="text-align: right;">-0.0888166</td>
<td style="text-align: right;">0.0168791</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium (sensu stricto)</td>
<td style="text-align: right;">0.5370610</td>
<td style="text-align: right;">0.0150382</td>
<td style="text-align: right;">0.0079447</td>
<td style="text-align: right;">0.0310684</td>
<td style="text-align: right;">-0.0502054</td>
<td style="text-align: right;">0.0775736</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium cellulosi et rel.</td>
<td style="text-align: right;">0.5070559</td>
<td style="text-align: right;">0.0316208</td>
<td style="text-align: right;">0.0158640</td>
<td style="text-align: right;">0.0236585</td>
<td style="text-align: right;">-0.0232673</td>
<td style="text-align: right;">0.0711286</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium colinum et rel.</td>
<td style="text-align: right;">0.0728706</td>
<td style="text-align: right;">-0.0018382</td>
<td style="text-align: right;">-0.0001897</td>
<td style="text-align: right;">0.0074117</td>
<td style="text-align: right;">-0.0159672</td>
<td style="text-align: right;">0.0156452</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium difficile et rel.</td>
<td style="text-align: right;">0.8142574</td>
<td style="text-align: right;">-0.0026570</td>
<td style="text-align: right;">-0.0021154</td>
<td style="text-align: right;">0.0438097</td>
<td style="text-align: right;">-0.0934076</td>
<td style="text-align: right;">0.0833859</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium felsineum et rel.</td>
<td style="text-align: right;">-0.0340414</td>
<td style="text-align: right;">0.0020414</td>
<td style="text-align: right;">-0.0000636</td>
<td style="text-align: right;">0.0077441</td>
<td style="text-align: right;">-0.0162744</td>
<td style="text-align: right;">0.0162776</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium leptum et rel.</td>
<td style="text-align: right;">0.0255534</td>
<td style="text-align: right;">-0.0139850</td>
<td style="text-align: right;">-0.0001405</td>
<td style="text-align: right;">0.0074711</td>
<td style="text-align: right;">-0.0164577</td>
<td style="text-align: right;">0.0153352</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium nexile et rel.</td>
<td style="text-align: right;">0.3368878</td>
<td style="text-align: right;">-0.0043116</td>
<td style="text-align: right;">-0.0014438</td>
<td style="text-align: right;">0.0186145</td>
<td style="text-align: right;">-0.0426096</td>
<td style="text-align: right;">0.0354906</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium orbiscindens et rel.</td>
<td style="text-align: right;">0.4712390</td>
<td style="text-align: right;">-0.0083084</td>
<td style="text-align: right;">-0.0038912</td>
<td style="text-align: right;">0.0253153</td>
<td style="text-align: right;">-0.0584026</td>
<td style="text-align: right;">0.0457048</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium ramosum et rel.</td>
<td style="text-align: right;">-0.1274005</td>
<td style="text-align: right;">-0.0122230</td>
<td style="text-align: right;">0.0016370</td>
<td style="text-align: right;">0.0130515</td>
<td style="text-align: right;">-0.0187000</td>
<td style="text-align: right;">0.0262375</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium sphenoides et rel.</td>
<td style="text-align: right;">-0.0486632</td>
<td style="text-align: right;">-0.0206447</td>
<td style="text-align: right;">0.0008828</td>
<td style="text-align: right;">0.0080279</td>
<td style="text-align: right;">-0.0147508</td>
<td style="text-align: right;">0.0192576</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium stercorarium et rel.</td>
<td style="text-align: right;">-0.7553026</td>
<td style="text-align: right;">-0.0145801</td>
<td style="text-align: right;">0.0108460</td>
<td style="text-align: right;">0.0382216</td>
<td style="text-align: right;">-0.0615138</td>
<td style="text-align: right;">0.0945224</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Clostridium symbiosum et rel.</td>
<td style="text-align: right;">-0.2074546</td>
<td style="text-align: right;">0.0157156</td>
<td style="text-align: right;">-0.0030865</td>
<td style="text-align: right;">0.0127837</td>
<td style="text-align: right;">-0.0339735</td>
<td style="text-align: right;">0.0211497</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Clostridium thermocellum et rel.</td>
<td style="text-align: right;">-0.0322474</td>
<td style="text-align: right;">0.0016729</td>
<td style="text-align: right;">0.0000163</td>
<td style="text-align: right;">0.0087007</td>
<td style="text-align: right;">-0.0161134</td>
<td style="text-align: right;">0.0172711</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Collinsella</td>
<td style="text-align: right;">0.6001705</td>
<td style="text-align: right;">-0.0090597</td>
<td style="text-align: right;">-0.0054407</td>
<td style="text-align: right;">0.0328874</td>
<td style="text-align: right;">-0.0796780</td>
<td style="text-align: right;">0.0570527</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Coprobacillus catenaformis et rel.</td>
<td style="text-align: right;">-0.1911201</td>
<td style="text-align: right;">-0.0129783</td>
<td style="text-align: right;">0.0025070</td>
<td style="text-align: right;">0.0133740</td>
<td style="text-align: right;">-0.0237813</td>
<td style="text-align: right;">0.0339228</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Coprococcus eutactus et rel.</td>
<td style="text-align: right;">0.2154627</td>
<td style="text-align: right;">0.0017806</td>
<td style="text-align: right;">0.0004555</td>
<td style="text-align: right;">0.0132615</td>
<td style="text-align: right;">-0.0271240</td>
<td style="text-align: right;">0.0298796</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Corynebacterium</td>
<td style="text-align: right;">-0.0335297</td>
<td style="text-align: right;">0.0014754</td>
<td style="text-align: right;">-0.0001425</td>
<td style="text-align: right;">0.0083013</td>
<td style="text-align: right;">-0.0178405</td>
<td style="text-align: right;">0.0153994</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Desulfovibrio et rel.</td>
<td style="text-align: right;">-0.0032956</td>
<td style="text-align: right;">-0.0199853</td>
<td style="text-align: right;">0.0000699</td>
<td style="text-align: right;">0.0076787</td>
<td style="text-align: right;">-0.0160857</td>
<td style="text-align: right;">0.0171259</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Dialister</td>
<td style="text-align: right;">-0.3121025</td>
<td style="text-align: right;">-0.0012601</td>
<td style="text-align: right;">0.0004237</td>
<td style="text-align: right;">0.0136352</td>
<td style="text-align: right;">-0.0285200</td>
<td style="text-align: right;">0.0286115</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Dorea formicigenerans et rel.</td>
<td style="text-align: right;">0.1854114</td>
<td style="text-align: right;">-0.0235184</td>
<td style="text-align: right;">-0.0042398</td>
<td style="text-align: right;">0.0134072</td>
<td style="text-align: right;">-0.0379444</td>
<td style="text-align: right;">0.0182941</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Eggerthella lenta et rel.</td>
<td style="text-align: right;">0.1340149</td>
<td style="text-align: right;">-0.0065494</td>
<td style="text-align: right;">-0.0007804</td>
<td style="text-align: right;">0.0118443</td>
<td style="text-align: right;">-0.0253265</td>
<td style="text-align: right;">0.0215110</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Enterobacter aerogenes et rel.</td>
<td style="text-align: right;">0.4044719</td>
<td style="text-align: right;">-0.0079012</td>
<td style="text-align: right;">-0.0031717</td>
<td style="text-align: right;">0.0225261</td>
<td style="text-align: right;">-0.0520857</td>
<td style="text-align: right;">0.0405474</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Enterococcus</td>
<td style="text-align: right;">0.1572056</td>
<td style="text-align: right;">-0.0158517</td>
<td style="text-align: right;">-0.0024762</td>
<td style="text-align: right;">0.0114628</td>
<td style="text-align: right;">-0.0308401</td>
<td style="text-align: right;">0.0178184</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Escherichia coli et rel.</td>
<td style="text-align: right;">0.4203586</td>
<td style="text-align: right;">0.0038276</td>
<td style="text-align: right;">0.0016441</td>
<td style="text-align: right;">0.0196167</td>
<td style="text-align: right;">-0.0354580</td>
<td style="text-align: right;">0.0444888</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Eubacterium biforme et rel.</td>
<td style="text-align: right;">1.3462071</td>
<td style="text-align: right;">-0.0307209</td>
<td style="text-align: right;">-0.0413851</td>
<td style="text-align: right;">0.0690515</td>
<td style="text-align: right;">-0.1912759</td>
<td style="text-align: right;">0.0853494</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Eubacterium cylindroides et rel.</td>
<td style="text-align: right;">-0.1548839</td>
<td style="text-align: right;">-0.0138722</td>
<td style="text-align: right;">0.0023064</td>
<td style="text-align: right;">0.0127462</td>
<td style="text-align: right;">-0.0195743</td>
<td style="text-align: right;">0.0295970</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Eubacterium hallii et rel.</td>
<td style="text-align: right;">0.1130157</td>
<td style="text-align: right;">0.0003832</td>
<td style="text-align: right;">0.0000949</td>
<td style="text-align: right;">0.0090929</td>
<td style="text-align: right;">-0.0193297</td>
<td style="text-align: right;">0.0201696</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Eubacterium limosum et rel.</td>
<td style="text-align: right;">-0.0850814</td>
<td style="text-align: right;">-0.0004137</td>
<td style="text-align: right;">0.0000425</td>
<td style="text-align: right;">0.0091108</td>
<td style="text-align: right;">-0.0192802</td>
<td style="text-align: right;">0.0190708</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Eubacterium rectale et rel.</td>
<td style="text-align: right;">0.1573747</td>
<td style="text-align: right;">0.0290509</td>
<td style="text-align: right;">0.0046529</td>
<td style="text-align: right;">0.0120534</td>
<td style="text-align: right;">-0.0153754</td>
<td style="text-align: right;">0.0343830</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Eubacterium siraeum et rel.</td>
<td style="text-align: right;">-0.1692902</td>
<td style="text-align: right;">0.0114122</td>
<td style="text-align: right;">-0.0019732</td>
<td style="text-align: right;">0.0122994</td>
<td style="text-align: right;">-0.0298032</td>
<td style="text-align: right;">0.0212265</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Eubacterium ventriosum et rel.</td>
<td style="text-align: right;">-0.3664902</td>
<td style="text-align: right;">0.0238251</td>
<td style="text-align: right;">-0.0086357</td>
<td style="text-align: right;">0.0205087</td>
<td style="text-align: right;">-0.0543034</td>
<td style="text-align: right;">0.0292723</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Faecalibacterium prausnitzii et rel.</td>
<td style="text-align: right;">0.2746364</td>
<td style="text-align: right;">0.0688707</td>
<td style="text-align: right;">0.0189899</td>
<td style="text-align: right;">0.0211137</td>
<td style="text-align: right;">-0.0077978</td>
<td style="text-align: right;">0.0704884</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Fusobacteria</td>
<td style="text-align: right;">0.1970453</td>
<td style="text-align: right;">0.0395915</td>
<td style="text-align: right;">0.0077262</td>
<td style="text-align: right;">0.0168922</td>
<td style="text-align: right;">-0.0150911</td>
<td style="text-align: right;">0.0477986</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Gemella</td>
<td style="text-align: right;">-0.0240562</td>
<td style="text-align: right;">0.0056807</td>
<td style="text-align: right;">0.0000245</td>
<td style="text-align: right;">0.0075407</td>
<td style="text-align: right;">-0.0156232</td>
<td style="text-align: right;">0.0160408</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Granulicatella</td>
<td style="text-align: right;">-0.0394249</td>
<td style="text-align: right;">0.0029104</td>
<td style="text-align: right;">0.0000135</td>
<td style="text-align: right;">0.0079160</td>
<td style="text-align: right;">-0.0173830</td>
<td style="text-align: right;">0.0158808</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Haemophilus</td>
<td style="text-align: right;">0.2179848</td>
<td style="text-align: right;">0.0040922</td>
<td style="text-align: right;">0.0009814</td>
<td style="text-align: right;">0.0128486</td>
<td style="text-align: right;">-0.0261300</td>
<td style="text-align: right;">0.0281105</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Helicobacter</td>
<td style="text-align: right;">-0.0545538</td>
<td style="text-align: right;">0.0080151</td>
<td style="text-align: right;">-0.0004707</td>
<td style="text-align: right;">0.0089999</td>
<td style="text-align: right;">-0.0186403</td>
<td style="text-align: right;">0.0165323</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Klebisiella pneumoniae et rel.</td>
<td style="text-align: right;">0.5487348</td>
<td style="text-align: right;">-0.0092739</td>
<td style="text-align: right;">-0.0049139</td>
<td style="text-align: right;">0.0299892</td>
<td style="text-align: right;">-0.0704904</td>
<td style="text-align: right;">0.0574520</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Lachnobacillus bovis et rel.</td>
<td style="text-align: right;">0.1238226</td>
<td style="text-align: right;">-0.0044866</td>
<td style="text-align: right;">-0.0005925</td>
<td style="text-align: right;">0.0086661</td>
<td style="text-align: right;">-0.0206450</td>
<td style="text-align: right;">0.0165903</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Lachnospira pectinoschiza et rel.</td>
<td style="text-align: right;">-0.0844327</td>
<td style="text-align: right;">0.0088921</td>
<td style="text-align: right;">-0.0005593</td>
<td style="text-align: right;">0.0080094</td>
<td style="text-align: right;">-0.0181668</td>
<td style="text-align: right;">0.0160800</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Lactobacillus catenaformis et rel.</td>
<td style="text-align: right;">-0.0924885</td>
<td style="text-align: right;">-0.0306690</td>
<td style="text-align: right;">0.0030234</td>
<td style="text-align: right;">0.0099549</td>
<td style="text-align: right;">-0.0127935</td>
<td style="text-align: right;">0.0284185</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Lactobacillus gasseri et rel.</td>
<td style="text-align: right;">-0.1183432</td>
<td style="text-align: right;">-0.0041217</td>
<td style="text-align: right;">0.0004743</td>
<td style="text-align: right;">0.0108633</td>
<td style="text-align: right;">-0.0206147</td>
<td style="text-align: right;">0.0232117</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Lactobacillus plantarum et rel.</td>
<td style="text-align: right;">0.0296594</td>
<td style="text-align: right;">-0.0065527</td>
<td style="text-align: right;">-0.0002134</td>
<td style="text-align: right;">0.0082289</td>
<td style="text-align: right;">-0.0174067</td>
<td style="text-align: right;">0.0162651</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Lactobacillus salivarius et rel.</td>
<td style="text-align: right;">0.2171045</td>
<td style="text-align: right;">0.0345409</td>
<td style="text-align: right;">0.0075856</td>
<td style="text-align: right;">0.0148103</td>
<td style="text-align: right;">-0.0158723</td>
<td style="text-align: right;">0.0440080</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Lactococcus</td>
<td style="text-align: right;">0.0549825</td>
<td style="text-align: right;">-0.0021350</td>
<td style="text-align: right;">-0.0002006</td>
<td style="text-align: right;">0.0081130</td>
<td style="text-align: right;">-0.0189555</td>
<td style="text-align: right;">0.0169394</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Leminorella</td>
<td style="text-align: right;">-0.0418927</td>
<td style="text-align: right;">-0.0292347</td>
<td style="text-align: right;">0.0012824</td>
<td style="text-align: right;">0.0087448</td>
<td style="text-align: right;">-0.0151399</td>
<td style="text-align: right;">0.0219836</td>
<td style="text-align: right;">2e-04</td>
</tr>
<tr class="even">
<td style="text-align: left;">Megamonas hypermegale et rel.</td>
<td style="text-align: right;">-0.0677306</td>
<td style="text-align: right;">0.0038409</td>
<td style="text-align: right;">-0.0003766</td>
<td style="text-align: right;">0.0086568</td>
<td style="text-align: right;">-0.0185851</td>
<td style="text-align: right;">0.0167125</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Megasphaera elsdenii et rel.</td>
<td style="text-align: right;">1.0003573</td>
<td style="text-align: right;">-0.0092953</td>
<td style="text-align: right;">-0.0092938</td>
<td style="text-align: right;">0.0448133</td>
<td style="text-align: right;">-0.1008281</td>
<td style="text-align: right;">0.0779085</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Methylobacterium</td>
<td style="text-align: right;">-0.0336368</td>
<td style="text-align: right;">0.0039407</td>
<td style="text-align: right;">-0.0000330</td>
<td style="text-align: right;">0.0082967</td>
<td style="text-align: right;">-0.0172844</td>
<td style="text-align: right;">0.0179637</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Micrococcaceae</td>
<td style="text-align: right;">-0.0301822</td>
<td style="text-align: right;">0.0036038</td>
<td style="text-align: right;">-0.0000485</td>
<td style="text-align: right;">0.0076385</td>
<td style="text-align: right;">-0.0167338</td>
<td style="text-align: right;">0.0162873</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Mitsuokella multiacida et rel.</td>
<td style="text-align: right;">2.2893734</td>
<td style="text-align: right;">0.0098644</td>
<td style="text-align: right;">0.0226063</td>
<td style="text-align: right;">0.0889483</td>
<td style="text-align: right;">-0.1461785</td>
<td style="text-align: right;">0.2049157</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Moraxellaceae</td>
<td style="text-align: right;">0.2214110</td>
<td style="text-align: right;">-0.0368029</td>
<td style="text-align: right;">-0.0082513</td>
<td style="text-align: right;">0.0177238</td>
<td style="text-align: right;">-0.0517852</td>
<td style="text-align: right;">0.0161416</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Novosphingobium</td>
<td style="text-align: right;">-0.0195188</td>
<td style="text-align: right;">-0.0008200</td>
<td style="text-align: right;">-0.0000452</td>
<td style="text-align: right;">0.0077530</td>
<td style="text-align: right;">-0.0163329</td>
<td style="text-align: right;">0.0167480</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Oceanospirillum</td>
<td style="text-align: right;">-0.3134655</td>
<td style="text-align: right;">0.0031706</td>
<td style="text-align: right;">-0.0009159</td>
<td style="text-align: right;">0.0169925</td>
<td style="text-align: right;">-0.0362450</td>
<td style="text-align: right;">0.0349114</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Oscillospira guillermondii et rel.</td>
<td style="text-align: right;">0.9138457</td>
<td style="text-align: right;">0.0252810</td>
<td style="text-align: right;">0.0231063</td>
<td style="text-align: right;">0.0456910</td>
<td style="text-align: right;">-0.0597042</td>
<td style="text-align: right;">0.1247802</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Outgrouping clostridium cluster XIVa</td>
<td style="text-align: right;">0.3270423</td>
<td style="text-align: right;">0.0052313</td>
<td style="text-align: right;">0.0016481</td>
<td style="text-align: right;">0.0188497</td>
<td style="text-align: right;">-0.0378694</td>
<td style="text-align: right;">0.0423661</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Oxalobacter formigenes et rel.</td>
<td style="text-align: right;">-0.0626016</td>
<td style="text-align: right;">-0.0525842</td>
<td style="text-align: right;">0.0032554</td>
<td style="text-align: right;">0.0104369</td>
<td style="text-align: right;">-0.0154769</td>
<td style="text-align: right;">0.0296003</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Papillibacter cinnamivorans et rel.</td>
<td style="text-align: right;">0.3830457</td>
<td style="text-align: right;">0.0552865</td>
<td style="text-align: right;">0.0212981</td>
<td style="text-align: right;">0.0311141</td>
<td style="text-align: right;">-0.0186141</td>
<td style="text-align: right;">0.1011308</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Parabacteroides distasonis et rel.</td>
<td style="text-align: right;">-1.3967928</td>
<td style="text-align: right;">-0.0299605</td>
<td style="text-align: right;">0.0417644</td>
<td style="text-align: right;">0.0739321</td>
<td style="text-align: right;">-0.0904422</td>
<td style="text-align: right;">0.2021413</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Peptococcus niger et rel.</td>
<td style="text-align: right;">0.1025490</td>
<td style="text-align: right;">-0.0044213</td>
<td style="text-align: right;">-0.0004235</td>
<td style="text-align: right;">0.0092166</td>
<td style="text-align: right;">-0.0199071</td>
<td style="text-align: right;">0.0192405</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Peptostreptococcus anaerobius et rel.</td>
<td style="text-align: right;">-0.0339373</td>
<td style="text-align: right;">0.0004160</td>
<td style="text-align: right;">-0.0002586</td>
<td style="text-align: right;">0.0078424</td>
<td style="text-align: right;">-0.0174512</td>
<td style="text-align: right;">0.0157826</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Peptostreptococcus micros et rel.</td>
<td style="text-align: right;">-0.0419772</td>
<td style="text-align: right;">0.0205030</td>
<td style="text-align: right;">-0.0007327</td>
<td style="text-align: right;">0.0086963</td>
<td style="text-align: right;">-0.0194019</td>
<td style="text-align: right;">0.0154376</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Phascolarctobacterium faecium et rel.</td>
<td style="text-align: right;">0.0713639</td>
<td style="text-align: right;">-0.0201959</td>
<td style="text-align: right;">-0.0015023</td>
<td style="text-align: right;">0.0079725</td>
<td style="text-align: right;">-0.0211252</td>
<td style="text-align: right;">0.0127415</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Prevotella melaninogenica et rel.</td>
<td style="text-align: right;">1.8321808</td>
<td style="text-align: right;">0.0173086</td>
<td style="text-align: right;">0.0317789</td>
<td style="text-align: right;">0.0760096</td>
<td style="text-align: right;">-0.1130378</td>
<td style="text-align: right;">0.1959814</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Prevotella oralis et rel.</td>
<td style="text-align: right;">1.2723480</td>
<td style="text-align: right;">-0.0038279</td>
<td style="text-align: right;">-0.0048559</td>
<td style="text-align: right;">0.0608010</td>
<td style="text-align: right;">-0.1295469</td>
<td style="text-align: right;">0.1192153</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Prevotella ruminicola et rel.</td>
<td style="text-align: right;">0.3242024</td>
<td style="text-align: right;">0.0079605</td>
<td style="text-align: right;">0.0025622</td>
<td style="text-align: right;">0.0192387</td>
<td style="text-align: right;">-0.0370743</td>
<td style="text-align: right;">0.0438282</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Prevotella tannerae et rel.</td>
<td style="text-align: right;">-0.9231102</td>
<td style="text-align: right;">0.0054559</td>
<td style="text-align: right;">-0.0049108</td>
<td style="text-align: right;">0.0478144</td>
<td style="text-align: right;">-0.1039126</td>
<td style="text-align: right;">0.0864555</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Propionibacterium</td>
<td style="text-align: right;">-0.1465679</td>
<td style="text-align: right;">0.0011819</td>
<td style="text-align: right;">-0.0002517</td>
<td style="text-align: right;">0.0111528</td>
<td style="text-align: right;">-0.0240827</td>
<td style="text-align: right;">0.0240475</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Proteus et rel.</td>
<td style="text-align: right;">0.0127863</td>
<td style="text-align: right;">0.0057936</td>
<td style="text-align: right;">-0.0000805</td>
<td style="text-align: right;">0.0079460</td>
<td style="text-align: right;">-0.0170530</td>
<td style="text-align: right;">0.0161172</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Pseudomonas</td>
<td style="text-align: right;">-0.0615840</td>
<td style="text-align: right;">0.0648817</td>
<td style="text-align: right;">-0.0046813</td>
<td style="text-align: right;">0.0337717</td>
<td style="text-align: right;">-0.0593579</td>
<td style="text-align: right;">0.0193996</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Roseburia intestinalis et rel.</td>
<td style="text-align: right;">-0.0695782</td>
<td style="text-align: right;">0.0015959</td>
<td style="text-align: right;">-0.0001760</td>
<td style="text-align: right;">0.0076414</td>
<td style="text-align: right;">-0.0166707</td>
<td style="text-align: right;">0.0169508</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ruminococcus bromii et rel.</td>
<td style="text-align: right;">0.3812075</td>
<td style="text-align: right;">0.0008526</td>
<td style="text-align: right;">0.0002832</td>
<td style="text-align: right;">0.0185902</td>
<td style="text-align: right;">-0.0380212</td>
<td style="text-align: right;">0.0401233</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ruminococcus callidus et rel.</td>
<td style="text-align: right;">0.4415544</td>
<td style="text-align: right;">-0.0094853</td>
<td style="text-align: right;">-0.0042967</td>
<td style="text-align: right;">0.0234597</td>
<td style="text-align: right;">-0.0569593</td>
<td style="text-align: right;">0.0406762</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ruminococcus gnavus et rel.</td>
<td style="text-align: right;">-0.0951253</td>
<td style="text-align: right;">-0.0073943</td>
<td style="text-align: right;">0.0006909</td>
<td style="text-align: right;">0.0087131</td>
<td style="text-align: right;">-0.0170003</td>
<td style="text-align: right;">0.0203758</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Ruminococcus lactaris et rel.</td>
<td style="text-align: right;">0.4023908</td>
<td style="text-align: right;">-0.0507484</td>
<td style="text-align: right;">-0.0203447</td>
<td style="text-align: right;">0.0258097</td>
<td style="text-align: right;">-0.0839259</td>
<td style="text-align: right;">0.0191048</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Ruminococcus obeum et rel.</td>
<td style="text-align: right;">0.2702176</td>
<td style="text-align: right;">0.0129015</td>
<td style="text-align: right;">0.0035458</td>
<td style="text-align: right;">0.0173123</td>
<td style="text-align: right;">-0.0281278</td>
<td style="text-align: right;">0.0432072</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Serratia</td>
<td style="text-align: right;">0.5174583</td>
<td style="text-align: right;">0.0053252</td>
<td style="text-align: right;">0.0027160</td>
<td style="text-align: right;">0.0259184</td>
<td style="text-align: right;">-0.0481486</td>
<td style="text-align: right;">0.0563843</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Sporobacter termitidis et rel.</td>
<td style="text-align: right;">0.4822789</td>
<td style="text-align: right;">0.0370441</td>
<td style="text-align: right;">0.0177450</td>
<td style="text-align: right;">0.0274573</td>
<td style="text-align: right;">-0.0301732</td>
<td style="text-align: right;">0.0836117</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Staphylococcus</td>
<td style="text-align: right;">-0.1076488</td>
<td style="text-align: right;">0.0086398</td>
<td style="text-align: right;">-0.0011198</td>
<td style="text-align: right;">0.0099323</td>
<td style="text-align: right;">-0.0240845</td>
<td style="text-align: right;">0.0174317</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Streptococcus bovis et rel.</td>
<td style="text-align: right;">-0.3917821</td>
<td style="text-align: right;">-0.0233969</td>
<td style="text-align: right;">0.0091428</td>
<td style="text-align: right;">0.0211726</td>
<td style="text-align: right;">-0.0262839</td>
<td style="text-align: right;">0.0588384</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Streptococcus intermedius et rel.</td>
<td style="text-align: right;">-0.0425288</td>
<td style="text-align: right;">0.0104161</td>
<td style="text-align: right;">-0.0003618</td>
<td style="text-align: right;">0.0075260</td>
<td style="text-align: right;">-0.0165908</td>
<td style="text-align: right;">0.0150249</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Streptococcus mitis et rel.</td>
<td style="text-align: right;">0.0453537</td>
<td style="text-align: right;">0.0257317</td>
<td style="text-align: right;">0.0009929</td>
<td style="text-align: right;">0.0076281</td>
<td style="text-align: right;">-0.0137755</td>
<td style="text-align: right;">0.0183121</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Subdoligranulum variable at rel.</td>
<td style="text-align: right;">-0.3039238</td>
<td style="text-align: right;">0.0228576</td>
<td style="text-align: right;">-0.0069093</td>
<td style="text-align: right;">0.0169972</td>
<td style="text-align: right;">-0.0454611</td>
<td style="text-align: right;">0.0249571</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Sutterella wadsworthia et rel.</td>
<td style="text-align: right;">-0.3487286</td>
<td style="text-align: right;">0.0062186</td>
<td style="text-align: right;">-0.0021984</td>
<td style="text-align: right;">0.0179725</td>
<td style="text-align: right;">-0.0398900</td>
<td style="text-align: right;">0.0355873</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Tannerella et rel.</td>
<td style="text-align: right;">-0.6967831</td>
<td style="text-align: right;">-0.0025240</td>
<td style="text-align: right;">0.0018354</td>
<td style="text-align: right;">0.0395133</td>
<td style="text-align: right;">-0.0764881</td>
<td style="text-align: right;">0.0834779</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Uncultured Bacteroidetes</td>
<td style="text-align: right;">-0.2683498</td>
<td style="text-align: right;">-0.0178019</td>
<td style="text-align: right;">0.0046383</td>
<td style="text-align: right;">0.0152975</td>
<td style="text-align: right;">-0.0244418</td>
<td style="text-align: right;">0.0401138</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Uncultured Chroococcales</td>
<td style="text-align: right;">-0.0473783</td>
<td style="text-align: right;">0.0025112</td>
<td style="text-align: right;">-0.0002264</td>
<td style="text-align: right;">0.0084440</td>
<td style="text-align: right;">-0.0182313</td>
<td style="text-align: right;">0.0164414</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Uncultured Clostridiales I</td>
<td style="text-align: right;">0.5011098</td>
<td style="text-align: right;">-0.0166734</td>
<td style="text-align: right;">-0.0085170</td>
<td style="text-align: right;">0.0245970</td>
<td style="text-align: right;">-0.0595899</td>
<td style="text-align: right;">0.0398529</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Uncultured Clostridiales II</td>
<td style="text-align: right;">0.7104208</td>
<td style="text-align: right;">-0.0255257</td>
<td style="text-align: right;">-0.0181563</td>
<td style="text-align: right;">0.0384091</td>
<td style="text-align: right;">-0.1028451</td>
<td style="text-align: right;">0.0519384</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Uncultured Mollicutes</td>
<td style="text-align: right;">0.7179726</td>
<td style="text-align: right;">-0.0718507</td>
<td style="text-align: right;">-0.0516714</td>
<td style="text-align: right;">0.0443914</td>
<td style="text-align: right;">-0.1613910</td>
<td style="text-align: right;">0.0143618</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Uncultured Selenomonadaceae</td>
<td style="text-align: right;">-0.6308507</td>
<td style="text-align: right;">-0.0308528</td>
<td style="text-align: right;">0.0194505</td>
<td style="text-align: right;">0.0330697</td>
<td style="text-align: right;">-0.0402302</td>
<td style="text-align: right;">0.0947211</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Veillonella</td>
<td style="text-align: right;">0.5015398</td>
<td style="text-align: right;">0.0163959</td>
<td style="text-align: right;">0.0079972</td>
<td style="text-align: right;">0.0255301</td>
<td style="text-align: right;">-0.0433739</td>
<td style="text-align: right;">0.0609320</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Vibrio</td>
<td style="text-align: right;">0.1116184</td>
<td style="text-align: right;">-0.0121287</td>
<td style="text-align: right;">-0.0013696</td>
<td style="text-align: right;">0.0112115</td>
<td style="text-align: right;">-0.0254308</td>
<td style="text-align: right;">0.0189760</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Weissella et rel.</td>
<td style="text-align: right;">0.1592478</td>
<td style="text-align: right;">0.0163095</td>
<td style="text-align: right;">0.0028174</td>
<td style="text-align: right;">0.0114178</td>
<td style="text-align: right;">-0.0177390</td>
<td style="text-align: right;">0.0305335</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Wissella et rel.</td>
<td style="text-align: right;">0.0990022</td>
<td style="text-align: right;">0.0051398</td>
<td style="text-align: right;">0.0006193</td>
<td style="text-align: right;">0.0085410</td>
<td style="text-align: right;">-0.0175848</td>
<td style="text-align: right;">0.0190485</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="odd">
<td style="text-align: left;">Xanthomonadaceae</td>
<td style="text-align: right;">0.1414589</td>
<td style="text-align: right;">0.0062835</td>
<td style="text-align: right;">0.0009106</td>
<td style="text-align: right;">0.0104508</td>
<td style="text-align: right;">-0.0208838</td>
<td style="text-align: right;">0.0243091</td>
<td style="text-align: right;">0e+00</td>
</tr>
<tr class="even">
<td style="text-align: left;">Yersinia et rel.</td>
<td style="text-align: right;">0.0733683</td>
<td style="text-align: right;">-0.0043924</td>
<td style="text-align: right;">-0.0003841</td>
<td style="text-align: right;">0.0085765</td>
<td style="text-align: right;">-0.0202294</td>
<td style="text-align: right;">0.0171690</td>
<td style="text-align: right;">0e+00</td>
</tr>
</tbody>
</table>

``` r
hima_res$effects %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">effect</th>
<th style="text-align: right;">estimate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">indirect</td>
<td style="text-align: right;">-0.0778225</td>
</tr>
<tr class="even">
<td style="text-align: left;">direct</td>
<td style="text-align: right;">-0.2908627</td>
</tr>
<tr class="odd">
<td style="text-align: left;">total</td>
<td style="text-align: right;">-0.3686852</td>
</tr>
</tbody>
</table>

``` r
hdma_res$effects %>% knitr::kable()
```

<table>
<thead>
<tr class="header">
<th style="text-align: left;">effect</th>
<th style="text-align: right;">estimate</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">indirect</td>
<td style="text-align: right;">-0.1982313</td>
</tr>
<tr class="even">
<td style="text-align: left;">direct</td>
<td style="text-align: right;">-0.1704539</td>
</tr>
<tr class="odd">
<td style="text-align: left;">total</td>
<td style="text-align: right;">-0.3686852</td>
</tr>
</tbody>
</table>

``` r
bslmm_res$effects %>% knitr::kable()
```

<table>
<colgroup>
<col style="width: 10%" />
<col style="width: 22%" />
<col style="width: 21%" />
<col style="width: 22%" />
<col style="width: 23%" />
</colgroup>
<thead>
<tr class="header">
<th style="text-align: left;">effect</th>
<th style="text-align: left;">estimate</th>
<th style="text-align: left;">posterior_sd</th>
<th style="text-align: left;">cl_2.5%</th>
<th style="text-align: left;">cl_97.5%</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">indirect</td>
<td style="text-align: left;">0.110284136432547</td>
<td style="text-align: left;">0.22792386329395</td>
<td style="text-align: left;">-0.337724309229086</td>
<td style="text-align: left;">0.565863324448858</td>
</tr>
<tr class="even">
<td style="text-align: left;">direct</td>
<td style="text-align: left;">-0.466510264075557</td>
<td style="text-align: left;">0.24679877416986</td>
<td style="text-align: left;">-0.972806670532726</td>
<td style="text-align: left;">0.0121923422263902</td>
</tr>
<tr class="odd">
<td style="text-align: left;">total</td>
<td style="text-align: left;">-0.35622612764301</td>
<td style="text-align: left;">0.157285774998102</td>
<td style="text-align: left;">-0.668071601670694</td>
<td style="text-align: left;">-0.0470162145916991</td>
</tr>
</tbody>
</table>
