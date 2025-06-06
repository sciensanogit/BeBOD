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
    Version:    \tab  0.0.11\cr
    Date:       \tab  2024-10-06\cr
    Authors:    \tab  Brecht Devleesschauwer\cr
    Maintainer: \tab  Brecht Devleesschauwer <\email{brechtdv@gmail.com}>\cr
    Depends:    \tab  R (>= 4.2.0)\cr
    License:    \tab  GPL (>= 2)\cr
}

Available functions in the \pkg{BeBOD} package:
  \tabular{ll}{
    \code{\link{rsle}}	\tab Calculate residual life expectancy, GBD 2019 standard life table.\cr
    \code{\link{rsle2021}}	\tab Calculate residual life expectancy, GBD 2021 standard life table.\cr
    \code{\link{explode_icd}}	\tab Expand ICD group into individual ICD codes.\cr
    \code{\link{map_gbd}}	\tab Map level 4 GBD cause to other levels.\cr
    \code{\link{select_best_inla_model}}	\tab Select best INLA model.\cr
    \code{\link{predict_with_best_inla_model}}	\tab Predict with best INLA model.\cr
  }
}

Available datasets in the \pkg{BeBOD} package:
  \tabular{ll}{
    \code{\link{gbd}}	\tab ICD10 to GBD cause list map.\cr
    \code{\link{causelist}}	\tab Master cause list used in the BeBOD study.\cr
  }
}

\keyword{package}
