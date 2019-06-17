\name{BeBOD-package}
\alias{BeBOD-package}
\alias{BeBOD}

\docType{package}

\title{Helper Functions for the Belgian National Burden of Disease Study}

\description{
The \pkg{BeBOD} package provides helper functions for the Belgian National Burden of Disease Study.
}

\details{
  \tabular{ll}{
    Package:    \tab  BeBOD\cr
    Type:       \tab  Package\cr
    Version:    \tab  0.0.1\cr
    Date:       \tab  2019-06-17\cr
    Authors:    \tab  Brecht Devleesschauwer\cr
    Maintainer: \tab  Brecht Devleesschauwer <\email{brechtdv@gmail.com}>\cr
    Depends:    \tab  R (>= 3.5.0)\cr
    License:    \tab  GPL (>= 2)\cr
}

Available functions in the \pkg{BeBOD} package:
  \tabular{ll}{
    \strong{Years of Life Lost}\cr
    \code{\link{rle}}	\tab Calculate residual life expectancy.\cr
    \code{\link{explode_icd}}	\tab Expand ICD group into individual ICD codes.\cr
    \code{\link{sim_target_icd}}	\tab Simulate target specific causes based on ICD codes.\cr
    \code{\link{sim_target_gbd}}	\tab Simulate target specific causes based on GBD causes.\cr
    \code{\link{map_gbd}}	\tab Map level 4 GBD cause to other levels.\cr
    \code{\link{redistribute_gbd}}	\tab Redistribute based on GBD causes.\cr
    \code{\link{redistribute_gbd_all}}	\tab Redistribute to all GBD causes.\cr
    \code{\link{plot_idd}}	\tab Plot original vs redistributed deaths.\cr
    \code{\link{plot_yll}}	\tab Plot original vs redistributed YLLs.\cr
    \code{\link{tab}}	\tab Tabulate results.\cr\cr
  }
}

\keyword{package}
