whoben
======

This repo contains reproduction materials for the paper "Who benefits? Welfare chauvinism and national stereotypes".

Contents:

1. `prel_s2_hjorth_20140505.sav` - the original data file (in SPSS format)
1. `dataprep.R` - R script that runs recodings of variables etc. and spits out `whobenefits_data.rds`, a data frame with all the relevant variables (in .rds format)
1. `whobenefits_data.rds` - the ready-to-analyze R data frame produced by `dataprep.r`
1. `analysis.R` - R script that runs all the analyses in the paper, saves tables in `tables` and figures in `figures`
1. `tables` - folder that holds tables (TeX formatted, in .txt format)
1. `figures` - folder that holds figures (in .pdf format)

Note that figure 1 in the paper uses data from the European Social Survey round 6. Due to its file size, the ESS data file `ESS6e02.dta` (in STATA format) is not included here. However, it can be downloaded from <http://www.europeansocialsurvey.org/data/download.html?r=6>.
