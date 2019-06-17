### BeBOD / Belgian National Burden of Disease Study
### .. Years of Life Lost

### RESIDUAL LIFE EXPECTANCY

rle <-
function(x) {
  # GBD 2017 standard life expectancy table
  GBD <-
  data.frame(age = c(0, 1, 5 * 1:22),
             LE = c(86.6, 85.8, 81.8, 76.8, 71.9, 66.9, 62.0, 57.0,
                    52.1, 47.2, 42.4, 37.6, 32.9, 28.3, 23.8, 19.4,
                    15.3, 11.5,  8.2,  5.5,  3.7,  2.6,  1.6,  1.4))

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

## simulate target specific causes based on ICD codes
sim_target_icd <-
function(target, new) {
  tab <- with(target, xtabs(~ ucause3 + AGE + sex))
  p <- mapply(function(i, j) tab[, i, j], new$AGE, new$sex)
  if (is.null(dim(p))) {  # only one ICD code as target
    p <- matrix(p, nrow = 1)
    rownames(p) <- dimnames(tab)$ucause3
  }
  p[, colSums(p) == 0] <- 1  # random allocation if no cases
  apply(p, 2, function(x) sample(names(x), prob = x, size = 1))
}

## simulate target specific causes based on GBD causes
sim_target_gbd <-
function(target, new) {
  tab <- with(target, xtabs(~ cause4 + AGE + sex))
  p <- mapply(function(i, j) tab[, i, j], new$AGE, new$sex)
  p[, colSums(p) == 0] <- 1  # random allocation if no cases
  apply(p, 2, function(x) sample(names(x), prob = x, size = 1))
}

## map level 4 GBD cause to other levels
map_gbd <-
function(x) {
  cause4 <- gbd$yll_cause_name[match(x, gsub("\\.", "", gbd$icd_code))]
  cause3 <- causelist$Level3[match(cause4, causelist$Level4)]
  cause2 <- causelist$Level2[match(cause4, causelist$Level4)]
  cause1 <- causelist$Level1[match(cause4, causelist$Level4)]
  cbind(cause4, cause3, cause2, cause1)
}

## expand GBD causes
expand_gbd <-
function(x) {
  causelist$Code[grepl(paste0(x, "_"), causelist$Code)]
}

## redistribute based on GBD causes
redistribute_gbd <-
function(def, expand = FALSE) {
  if (expand) {
    def2 <- expand_gbd(def)

  } else {
    def2 <- def
  }

  def2 <- gsub(" ", "", unlist(strsplit(def2, "\\|")))
  def2 <- paste(def2, collapse = "|")

  # show status
  cat("\n", def, "\n")

  # extract ill-defined deaths
  iddi  <- subset(idd, Target == def)
  mrti <- subset(mrt, mrt$ucause3 %in% iddi[, "ICDcode"])
  new <- mrti[, c("year", "AGE", "age", "sex", "region", "YLL")]

  # show status
  cat(nrow(new), "ill-defined deaths > ")

  # extract target
  target <- subset(mrt_step1, grepl(tolower(def2), tolower(mrt_step1$code)))

  # show status
  cat(nrow(target), " target deaths (",
      length(unique(target$cause4)), " level 4 causes)\n",
      sep = "")

  # tabulate > simulate
  s <- sim_target_gbd(target, new)

  # export target
  write.csv2(
    with(target, xtabs(~ cause4 + AGE + sex)),
    file = paste0("TARGET/gbd-", strsplit(def, " \\| ")[[1]][1], ".csv"))

  # compile output
  out <- cbind(new, cause4 = s)

  # output
  out
}

## redistribute to all GBD causes
redistribute_gbd_all <-
function() {
  # show status
  cat("\nALL\n")

  # extract ill-defined deaths
  iddi  <- subset(idd, Target == "ALL")
  mrti <- subset(mrt, mrt$ucause3 %in% iddi[, "ICDcode"])
  new <- mrti[, c("year", "AGE", "age", "sex", "region", "YLL")]

  # show status
  cat(nrow(new), "ill-defined deaths > ")

  # extract target
  target <- mrt_step2

  # show status
  cat(nrow(target), " target deaths (",
      length(unique(target$cause4)), " level 4 causes)\n",
      sep = "")

  # tabulate > simulate
  s <- sim_target_gbd(target, new)

  # export target
  write.csv2(
    with(target, xtabs(~ cause4 + AGE + sex)),
    file = paste0("TARGET/gbd-all.csv"))

  # compile output
  out <- cbind(new, cause4 = s)

  # output
  out
}


### RESULTS

## plot original vs redistributed deaths
plot_idd <-
function(mrt, red, level) {
  tab_cln <-
    table(factor(mrt[[level]]))
  tab_idd <-
    table(factor(red[[level]],
                 levels = sort(unique(mrt[[level]]))))

  tab_tot <- tab_cln + tab_idd
  labs <- names(sort(tab_tot))

  df <-
  rbind(
    data.frame(x = row.names(tab_cln),
               y = c(tab_cln),
               grp = "Specific"),
    data.frame(x = row.names(tab_cln),
               y = c(tab_idd),
               grp = "Redistributed"))
  df$grp <- factor(df$grp, c("Redistributed", "Specific"))

  ggplot(df, aes(x = x, y = y, group = grp)) +
    geom_col(aes(fill = grp)) +
    coord_flip() +
    scale_x_discrete(NULL, limits = labs) +
    scale_y_continuous("Deaths", labels = scales::comma) +
    scale_fill_brewer(
      NULL,
      breaks = c("Specific", "Redistributed"),
      palette = "Paired") +
    theme_bw() +
    theme(legend.position = "top")
}

## plot original vs redistributed YLLs
plot_yll <-
function(mrt, red, level) {
  tab_cln <-
    xtabs(mrt$YLL ~ factor(mrt[[level]]))
  tab_idd <-
    xtabs(red$YLL ~ factor(red[[level]],
                      levels = sort(unique(mrt[[level]]))))

  tab_tot <- tab_cln + tab_idd
  labs <- names(sort(tab_tot))

  df <-
  rbind(
    data.frame(x = row.names(tab_cln),
               y = c(tab_cln),
               grp = "Specific"),
    data.frame(x = row.names(tab_cln),
               y = c(tab_idd),
               grp = "Redistributed"))
  df$grp <- factor(df$grp, c("Redistributed", "Specific"))

  ggplot(df, aes(x = x, y = y, group = grp)) +
    geom_col(aes(fill = grp)) +
    coord_flip() +
    scale_x_discrete(NULL, limits = labs) +
    scale_y_continuous("Years of Life Lost", labels = scales::comma) +
    scale_fill_brewer(
      NULL,
      breaks = c("Specific", "Redistributed"),
      palette = "Paired") +    theme_bw() +
    theme(legend.position = "top")
}

## tabulate results
tab <-
function(x) {
  y <- x[order(rowSums(x), decreasing = TRUE), ]
  kable(addmargins(y))
}
