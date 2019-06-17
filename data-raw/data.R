### eq5d.be / generate internal data
### .. EQ-5D-3L value set
### .. EQ-5D-5L value set

## EQ-5D-3L value set
## .. based on Cleemput (2010)
## .. https://doi.org/10.1007/s10198-009-0167-0

## all possible 3L scores
scores3L <- expand.grid(MO = 1:3, SC = 1:3, UA = 1:3, PD = 1:3, AD = 1:3)

## define 'intercept': at least one score > 1
int <- rep(0, nrow(scores3L))                             # perfect health
int[apply(scores3L, 1, function(x) any(x > 1))] <- 0.152  # less than perfect

## define 'N3': at least one score == 3
N3 <- apply(scores3L, 1, function(x) any(x == 3)) * 0.256

## calculate 3L index
index3L <-
  with(scores3L,
       1 -                   # full health
         int -               # intercept
         0.074 * (MO - 1) -  # mobility
         0.083 * (SC - 1) -  # self-care
         0.031 * (UA - 1) -  # usual activities
         0.084 * (PD - 1) -  # pain/discomfort
         0.103 * (AD - 1) -  # anxiety/depression
         N3)                 # any dimension at level 3

## compile 3L value set
valueset3L <-
  data.frame(score = apply(scores3L, 1, paste, collapse = ""),
             index = index3L)


## EQ-5D-5L value set
## .. based on Cleemput (2010)
## .. https://doi.org/10.1007/s10198-009-0167-0
## .. based on van Hout et al. (2012)
## .. https://doi.org/10.1016/j.jval.2012.02.008

## all possible 5L scores
## .. note the reversed sequence in 'm'
scores5L <- expand.grid(AD = 1:5, PD = 1:5, UA = 1:5, SC = 1:5, MO = 1:5)
scores5L <- with(scores5L, cbind(MO, SC, UA, PD, AD))

## load 'Probability matrix' from 'EQ-5D-5L_Crosswalk_Value_Sets'
load("m.RData")

## product
m.prod <- t(t(m) * valueset3L$index)

## rowSums
m.sums <- rowSums(m.prod)

## compile 5L value set
valueset5L <-
  data.frame(score = apply(scores5L, 1, paste, collapse = ""),
             index = m.sums)


## Population norms :: 11/05/2018
load("popnormDIM.RData")
load("popnormIND.RData")
load("popnormVAS.RData")

popnormsMO  <- popnormMO
popnormsSC  <- popnormSC
popnormsUA  <- popnormUA
popnormsPD  <- popnormPD
popnormsAD  <- popnormAD
popnormsANY <- popnormANY
popnormsIND <- popnormIND
popnormsVAS <- popnormVAS

###
### SAVE DATA
###

save(
  valueset3L,
  valueset5L,
  popnormsMO,
  popnormsSC,
  popnormsUA,
  popnormsPD,
  popnormsAD,
  popnormsANY,
  popnormsIND,
  popnormsVAS,
  file = "../R/sysdata.rda")