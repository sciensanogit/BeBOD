\name{BeBOD-package}
\alias{BeBOD-package}
\alias{BeBOD}

\docType{package}

\title{Helper Functions for the Belgian National Burden of Disease Study}

\description{
The \pkg{BeBOD} package provides helper functions for the calculation of Years of Life Lost as part of the Belgian National Burden of Disease Study.
}

\details{
  \tabular{ll}{
    Package:    \tab  BeBOD\cr
    Type:       \tab  Package\cr
    Version:    \tab  0.0.7\cr
    Date:       \tab  2022-08-19\cr
    Authors:    \tab  Brecht Devleesschauwer\cr
    Maintainer: \tab  Brecht Devleesschauwer <\email{brechtdv@gmail.com}>\cr
    Depends:    \tab  R (>= 4.1.0)\cr
    License:    \tab  GPL (>= 2)\cr
}

Available functions in the \pkg{BeBOD} package:
  \tabular{ll}{
    \code{\link{rsle}}	\tab Calculate residual life expectancy.\cr
    \code{\link{explode_icd}}	\tab Expand ICD group into individual ICD codes.\cr
    \code{\link{map_gbd}}	\tab Map level 4 GBD cause to other levels.\cr
  }
}

Available datasets in the \pkg{BeBOD} package:
  \tabular{ll}{
    \code{\link{gbd}}	\tab ICD10 to GBD cause list map.\cr
    \code{\link{causelist}}	\tab Master cause list used in the BeBOD study.\cr
  }
}

\keyword{package}
