### BeBOD / generate internal data
### .. ICD-GBD mapping
### .. GBD cause list

## required packages
library(bd)

## ICD-GBD MAPPING

## import data
gbd <- readxl("ICD-GBD-mapping.xlsx")

## drop decimal symbol from 'icd_code'
gbd$icd_code <- gsub("\\.", "", gbd$icd_code)

## MASTER CAUSE LIST

causelist <- readxl("mastercauselist.xlsx")


###
### SAVE DATA
###

save(
  gbd,
  causelist,
  file = "../R/sysdata.rda")