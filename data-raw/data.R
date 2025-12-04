### BeBOD / generate internal data
### .. ICD-GBD mapping
### .. GBD cause list
### .. BEBOD colors

## required packages
library(ggsano)
library(readxl)

## ICD-GBD MAPPING

## import data
gbd <- BeBOD::readxl("data-raw/ICD-GBD-mapping.xlsx")

## drop decimal symbol from 'icd_code'
gbd$icd_code <- gsub("\\.", "", gbd$icd_code)

## MASTER CAUSE LIST

causelist <- BeBOD::readxl("data-raw/mastercauselist.xlsx")

## example of using the data: 'BeBOD:::causelist'
## package when defining colors: 
if (FALSE) {
  library(ggsano)
}

## read in the data
causelst <- causelist

## filter out specific codes
causelst <- subset(causelst, subset = Level1 != "Garbage code")

## remove level 4 and only keep unique entries
causelst <- causelst[paste0("Level",1:3)]
causelst <- unique(causelst)

## Create an order based on the cause list
L1 <- unique(causelst$Level1)
ord <- character()
lvls <- character()

for (i in seq_along(L1)) {
  ord <- c(ord, L1[i])
  lvls <- c(lvls, rep("Level1", length(L1[i])))
  L2 <- unique(causelst$Level2[causelst$Level1 == L1[i]])
  
  for (j in seq_along(L2)) {
    ord <- c(ord, L2[j])
    lvls <- c(lvls, rep("Level2", length(L2[j])))
    L3 <- unique(causelst$Level3[causelst$Level2 == L2[j]])
    
    for (k in seq_along(L3)) {
      ord <- c(ord, L3[k])
      lvls <- c(lvls, rep("Level3", length(L3[k])))
    }
  }
}

ord <- c("ALL CAUSES", ord)
lvls <- c("Level0", lvls)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
###                          CREATE COLORS ####
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## LEVEL1 colors
L1 <- unique(causelst$Level1)
L1.cols <- ggsano::pal_sciensano("default")(length(L1))
L1.lvls <- rep("Level1", length(L1))
scales::show_col(L1.cols)

## LEVEL2 colors
L2 <- unique(causelst$Level2)
L2.cols <-
  c("#3AAA35FF", "#006633FF", "#BCCF00FF", "#58595BFF", "#A5A5A5FF",
    "#FAD500FF", "#F29D00FF", "#C95117FF", "#E494C0FF", "#B93287FF",
    "#563B8CFF", "#84A5D7FF", "#60BCB8FF", "#228593FF", "#88D08FFF",
    "#00994CFF", "#E7E600FF", "#7A7B7DFF", "#FFD900FF", "#DB6F3FFF")
L2.lvls <- rep("Level2", length(L2))
scales::show_col(L2.cols)

## LEVEL 3 colors are the same as LEVEL 2
L3 <- character()
L3.cols <- character()

for (i in seq_along(L2)) {
  L3.i <- unique(causelst$Level3[causelst$Level2 == L2[i]])
  n.i <- length(L3.i) ## number of selected diseases
  # alpha <- seq(from = 1, to = 0.4, length.out = n.i)
  
  for (j in seq_along(L3.i)) {
    col <- rep(L2.cols[i], 1)
    L3 <- c(L3, L3.i[j])
    L3.cols <- c(L3.cols, col)
  }
}

L3.lvls <- rep("Level3", length(L3))
# scales::show_col(L3.cols)

## bind all rows
bebod_colors <-
  data.frame(
    "LEVEL" = c(L1.lvls, L2.lvls, L3.lvls),
    "CAUSE" = c(L1, L2, L3),
    "COL" = c(L1.cols, L2.cols, L3.cols))

###
### SAVE DATA
###

# usethis::use_data(
#   gbd, 
#   causelist, 
#   bebod_colors,
#   internal = TRUE, 
#   overwrite = TRUE
#   )

save(
  gbd,
  causelist,
  bebod_colors,
  file = "R/sysdata.rda")
