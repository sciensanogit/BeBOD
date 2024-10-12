###=========================================================================#
### UTILS ==================================================================#
###=========================================================================#
###-- collapse .................... collapse elements without separator
###-- openwd ...................... open working directory
###-- write_cb .................... write to Windows clipboard
###-- read_cb ..................... read from Windows clipboard
###-- today ....................... return today's date in yyymmdd format
###-- now ......................... return current time
###-- extract_pubmed .............. extract data from PubMed file
###-- logit ....................... log(p / (1 - p))
###-- expit ....................... exp(x) / (1 + exp(x))
###-- convert ..................... read and save as
###-- multiplot ................... plot multiple ggplot2 objects
###-- prop_table .................. return proportional table
###-- dropbox ..................... path to dropbox folder
###-- github ...................... path to github folder
###-- sanitize_specials ........... special characters to HTML/LaTeX
###-- readxl ...................... read excel file as data.frame
###-- quiet_source ................ source file without printing/plotting

##--------------------------------------------------------------------------#
## Collapse elements without separator -------------------------------------#
collapse <-
function(...) {
  paste(..., sep = "", collapse = "")
}


##--------------------------------------------------------------------------#
## Open working directory --------------------------------------------------#
openwd <-
function() {
  shell.exec(getwd())
}


##--------------------------------------------------------------------------#
## Write to Windows clipboard ----------------------------------------------#
write_cb <-
function(x, limit = 32, quote = FALSE, dec = ",", sep = "\t",
         row.names = FALSE, col.names = FALSE, ...) {
  clipboard_string <- paste0("clipboard-", limit)
  write.table(x, file = clipboard_string,
              quote = quote, dec = dec, sep = sep,
              row.names = row.names, col.names = col.names, ...)
}

##--------------------------------------------------------------------------#
## Read from Windows clipboard ---------------------------------------------#
read_cb <-
function(dec = ",", sep = "\t", ...) {
  read.table(file = "clipboard",
             dec = dec, sep = sep, ...)
}


##--------------------------------------------------------------------------#
## Return today's date in yyyymmdd format ----------------------------------#
today <-
function() {
  return(format(Sys.time(), "%Y%m%d"))
}


##--------------------------------------------------------------------------#
## Return current time -----------------------------------------------------#
now <-
function() {
  return(Sys.time())
}


##--------------------------------------------------------------------------#
## Extract data from PubMed file -------------------------------------------#
extract_pubmed <-
function(file, what = c("TI", "AUTH", "SRC", "YEAR", "AB")) {
  ## read file as character vector
  x <- readLines(file)

  ## check 'what'
  what <- match.arg(what, several.ok = TRUE)

  ## initialize 'out'
  out <- NULL

  ## ABSTRACT
  if ("AB" %in% what) {
    y <- character()
    t <- FALSE

    for (i in seq_along(x)) {
      if (t && identical(substr(x[i], 0, 6), "PMID- ")) {
        y <- c(y, "NA")
        t <- FALSE
      }
      if (identical(substr(x[i], 0, 6), "PMID- "))
        t <- TRUE
      if (t && identical(substr(x[i], 0, 4), "AB  ")) {
        ab <- substr(x[i], 7, nchar(x[i]))
        j <- i + 1
        while(identical(substr(x[j], 0, 4), "    ")) {
          ab <- paste(ab, substr(x[j], 7, nchar(x[j])))
          j <- j + 1
        }
        y <- c(y, ab)
        t <- FALSE
      }
    }

    out <- cbind(y, out)
  }

  ## YEAR
  if ("YEAR" %in% what) {
    y <- character()
    t <- FALSE

    for (i in seq_along(x)) {
      if (identical(substr(x[i], 0, 3), "DP ")) {
        year <- substr(x[i], 4, nchar(x[i]))
        year <- strsplit(year, " ")[[1]][3]
        y <- c(y, year)
      }
    }

    out <- cbind(y, out)
  }

  ## SOURCE
  if ("SRC" %in% what) {
    y <- character()
    t <- FALSE

    for (i in seq_along(x)) {
      if (t && identical(substr(x[i], 0, 6), "PMID- ")) {
        y <- c(y, "NA")
        t <- FALSE
      }
      if (identical(substr(x[i], 0, 6), "PMID- "))
        t <- TRUE
      if (t && identical(substr(x[i], 0, 4), "JT  ")) {
        auth <- substr(x[i], 7, nchar(x[i]))
        y <- c(y, auth)
        t <- FALSE
      }
    }

    out <- cbind(y, out)
  }

  ## AUTHOR
  if ("AUTH" %in% what) {
    y <- character()
    t <- FALSE

    for (i in seq_along(x)) {
      if (t && identical(substr(x[i], 0, 6), "PMID- ")) {
        y <- c(y, "NA")
        t <- FALSE
      }
      if (identical(substr(x[i], 0, 6), "PMID- "))
        t <- TRUE
      if (t && identical(substr(x[i], 0, 4), "FAU ")) {
        auth <- substr(x[i], 7, nchar(x[i]))
        auth <- strsplit(auth, ",")[[1]][1]
        y <- c(y, auth)
        t <- FALSE
      }
    }

    out <- cbind(y, out)
  }

  ## TITLE
  if ("TI" %in% what) {
    y <- character()
    t <- FALSE

    for (i in seq_along(x)) {
      if (t && identical(substr(x[i], 0, 3), "   ")) {
        y[length(y)] <- paste(y[length(y)], substr(x[i], 7, nchar(x[i])))
      } else {
        t <- FALSE
      }
      if (identical(substr(x[i], 0, 3), "TI ")) {
        y <- c(y, substr(x[i], 7, nchar(x[i])))
        t <- TRUE
      }
    }

    out <- cbind(y, out)
  }

  order <- order(match(what, c("TI", "AUTH", "SRC", "YEAR", "AB")))
  return(out[, order])
}


##--------------------------------------------------------------------------#
## Logit, Expit ------------------------------------------------------------#
logit <-
function(x) {
  log(x / (1 - x))
}

expit <-
function(x) {
  exp(x) / (1 + exp(x))
}


##--------------------------------------------------------------------------#
## Read and save as --------------------------------------------------------#
convert <-
function(file,
         from = c("csv2", "csv", "delim2", "delim", "dta"),
         to = c("csv", "csv2", "delim2", "delim", "dta"), ...) {
  from <- match.arg(from)
  to <- match.arg(to)
  if (from == to) stop("'from' should be different than 'to'")

  ## read file
  data <-
    switch(from,
           csv2 = read.csv2(file, ...),
           csv = read.csv(file, ...),
           delim2 = read.delim2(file, ...),
           delim = read.delim(file, ...),
           dta = read.dta(file, ...))

  ## save as
  file_to <- paste0("copy_", file)
  switch(to,
         csv2 = write.csv2(data, file_to),
         csv = write.csv(data, file_to),
         delim2 = write.table(data, file_to, sep = "\t", dec = ","),
         delim = write.table(data, file_to, sep = "\t", dec = "."),
         dta = write.dta(data, file_to))
}


##--------------------------------------------------------------------------#
## Plot multiple ggplot2 objects -------------------------------------------#

## SOURCE
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

multiplot <-
function(..., cols = 1, layout = NULL) {
  ## make a list from the ... arguments
  plots <- list(...)
  n_plots <- length(plots)

  ## if layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # make the panel
    # ncol: number of columns of plots
    # nrow: number of rows needed, calculated from # of cols
    layout <-
      matrix(seq(1, cols * ceiling(n_plots/cols)),
             ncol = cols, nrow = ceiling(n_plots/cols))
  }

  if (n_plots == 1) {
    print(plots[[1]])

  } else {
    ## set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    ## make each plot, in the correct location
    for (i in seq(n_plots)) {
      ## get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


##--------------------------------------------------------------------------#
## Return proportional table -----------------------------------------------#

prop_table <-
function(x, ...) {
  c(table(x, ...) / length(x))
}


##--------------------------------------------------------------------------#
## Path to Dropbox folder --------------------------------------------------#

dropbox <-
function(dir) {
  paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")),
         "/Dropbox/", dir)
}


##--------------------------------------------------------------------------#
## Path to GitHub folder --------------------------------------------------#

github <-
function(dir) {
  paste0(gsub("\\\\", "/", Sys.getenv("USERPROFILE")),
         "/Documents/GitHub/", dir)
}


##--------------------------------------------------------------------------#
## Translate special characters to HTML or LaTeX ---------------------------#

sanitize_specials <-
function(char, type = c("html", "latex")) {
  type <- match.arg(type)

  table <-
    matrix(c("\uE1", "&aacute", "\\\\'{a}",
             "\uE9", "&eacute", "\\\\'{e}",
             "\uED", "&iacute", "\\\\'{i}",
             "\uF3", "&oacute", "\\\\'{o}",
             "\uFA", "&uacute", "\\\\'{u}",

             "\uE0", "&agrave", "\\\\`{a}",
             "\uE8", "&egrave", "\\\\`{e}",
             "\uEC", "&igrave", "\\\\`{i}",
             "\uF2", "&ograve", "\\\\`{o}",
             "\uF9", "&ugrave", "\\\\`{u}",

             "\uE4", "&auml", "\\\\\"{a}",
             "\uEB", "&euml", "\\\\\"{e}",
             "\uEF", "&iuml", "\\\\\"{i}",
             "\uF6", "&ouml", "\\\\\"{o}",
             "\uFC", "&uuml", "\\\\\"{u}",

             "\uC1", "&Aacute", "\\\\'{A}",
             "\uC9", "&Eacute", "\\\\'{E}",
             "\uCD", "&Iacute", "\\\\'{I}",
             "\uD3", "&Oacute", "\\\\'{O}",
             "\uDA", "&Uacute", "\\\\'{U}",

             "\uC0", "&Agrave", "\\\\`{A}",
             "\uC8", "&Egrave", "\\\\`{E}",
             "\uCC", "&Igrave", "\\\\`{I}",
             "\uD2", "&Ograve", "\\\\`{O}",
             "\uD9", "&Ugrave", "\\\\`{U}",

             "\uC4", "&Auml", "\\\\\"{A}",
             "\uCB", "&Euml", "\\\\\"{E}",
             "\uCF", "&Iuml", "\\\\\"{I}",
             "\uD6", "&Ouml", "\\\\\"{O}",
             "\uDC", "&Uuml", "\\\\\"{U}",

             "\uF8", "&oslash", "\\\\o",
             
             "\uF1", "&ntilde", "\\\\~{n}",
             "\uD1", "&Ntilde", "\\\\~{N}",

             "&", "&amp;", "\\\\&"),
           ncol = 3, byrow = T)

  id <- ifelse(type == "html", 2, 3)

  for (i in seq(nrow(table))) {
    char <- gsub(table[i, 1], table[i, id], char)
  }

  return(char)
}


##--------------------------------------------------------------------------#
## Read Excel file as data.frame -------------------------------------------#

readxl <-
function(...) {
  xl <- read_excel(...)
  class(xl) <- "data.frame"
  colnames(xl) <- make.names(colnames(xl))
  return(xl)
}


##--------------------------------------------------------------------------#
## Source file without printing/plotting -----------------------------------#

quiet_source <-
function(file) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(source(file))) 
}
