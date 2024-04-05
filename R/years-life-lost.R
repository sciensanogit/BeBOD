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

rsle2021 <-
function(x) {
  # GBD 2021 standard life expectancy table
  # assume 0 YLL for highest observed lifespan
  GBD <-
  data.frame(
    age = c(0, 7/365, 1/12, 6/12, 1:110, 122),
    LE = c(89.9580397, 89.9914760, 89.9574569, 89.5655011, 89.0721580, 
           88.0908707, 87.0953494, 86.0990298, 85.1023919, 84.1058089, 
           83.1088953, 82.1115002, 81.1139531, 80.1161834, 79.1193000, 
           78.1223692, 77.1256640, 76.1293455, 75.1340069, 74.1408729, 
           73.1482455, 72.1563976, 71.1648732, 70.1735877, 69.1824886, 
           68.1912075, 67.1996830, 66.2082904, 65.2173888, 64.2262312, 
           63.2347744, 62.2434362, 61.2526150, 60.2626902, 59.2739148, 
           58.2853042, 57.2967160, 56.3081344, 55.3191685, 54.3297507, 
           53.3406958, 52.3531456, 51.3667019, 50.3803323, 49.3943345, 
           48.4098414, 47.4281271, 46.4503864, 45.4777365, 44.5105410, 
           43.5457622, 42.5839941, 41.6257863, 40.6708154, 39.7164939, 
           38.7641872, 37.8156317, 36.8717030, 35.9305719, 34.9918819, 
           34.0550998, 33.1205306, 32.1884443, 31.2576714, 30.3279454, 
           29.4006345, 28.4773321, 27.5594815, 26.6483802, 25.7019762, 
           24.8060039, 23.9172855, 23.0354761, 22.1602127, 21.2921735,
           20.4359834, 19.5968111, 18.7690913, 17.9521747, 17.1478391,
           16.3594354, 15.5903732, 14.8437514, 14.1228181, 13.4303470,
           12.7687747, 12.1406857, 11.5493584, 10.9991424, 10.4952212,
           10.0424249, 9.63466601, 9.27276948, 8.97351178, 8.75556961,
           8.62421991, 8.56828751, 8.52982465, 8.49469343, 8.35913019, 
           8.27608396, 8.16781129, 8.01200167, 7.77794559, 7.57542585,
           7.36983878, 7.16124979, 6.94945024, 6.73412054, 6.36130011,
           5.95447101, 5.51031821, 5.02500772, 4.49401111, 0))

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
