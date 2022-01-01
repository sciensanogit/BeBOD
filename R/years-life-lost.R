### BeBOD / Belgian National Burden of Disease Study
### .. Years of Life Lost

### RESIDUAL LIFE EXPECTANCY

rsle <-
function(x) {
  # GBD 2019 standard life expectancy table
  # assume 0 YLL for highest observed lifespan
  GBD <-
  data.frame(
    age = c(0, 1, 5 * 1:19, 122),
    LE = c(88.8718951, 88.00051053, 84.03008056, 79.04633476, 74.0665492,
           69.10756792, 64.14930031, 59.1962771, 54.25261364, 49.31739311,
           44.43332057, 39.63473787, 34.91488095, 30.25343822, 25.68089534,
           21.28820012, 17.10351469, 13.23872477, 9.990181244, 7.617724915, 
           5.922359078, 0))

  # return RLE value for age 'x'					
  approx(GBD$age, GBD$LE, x, rule = 1:2)$y
}


### REDISTRIBUTIONS

## expand ICD sequence into individual ICD codes
expand_icd <-
function(x) {
  if (grepl(":", x)) {
    b <- strsplit(x, ":")[[1]]
    X <- substr(b[1], 0, 1)
    s <- seq(from = as.numeric(gsub("[A-Z]", "", b[1])),
             to = as.numeric(gsub("[A-Z]", "", b[2])))
    fmt <- paste0("%0", nchar(substr(b[1], 2, nchar(b[1]))), "d")
    s <- sprintf(fmt, s)
    paste(paste0(X, s), collapse = "|")

  } else {
    x
  }
}

## expand ICD group into individual ICD codes
explode_icd <-
function(x) {
  y <- gsub(" ", "", unlist(strsplit(x, "\\|")))
  paste(sapply(y, expand_icd), collapse = "|")
}

## map level 4 GBD cause to other levels
map_gbd <-
function(x) {
  cause4 <- gbd$yll_cause_name[fmatch(x, gsub("\\.", "", gbd$icd_code))]
  id <- fmatch(cause4, causelist$Level4)
  cause3 <- causelist$Level3[id]
  cause2 <- causelist$Level2[id]
  cause1 <- causelist$Level1[id]
  cbind(cause4, cause3, cause2, cause1)
}
