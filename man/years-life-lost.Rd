\name{YLL-functions}
\alias{YLL-functions}
\alias{rsle}
\alias{explode_icd}
\alias{sim_target_icd}
\alias{sim_target_gbd}
\alias{map_gbd}
\alias{redistribute_gbd}
\alias{redistribute_gbd_all}
\alias{plot_idd}
\alias{plot_yll}
\alias{tab}

\title{Helper functions for Years of Life Lost calculations.}

\description{
Helper functions for Years of Life Lost calculations.
}

\usage{
rsle(x)
explode_icd(x)
sim_target_icd(target, new)
sim_target_gbd(target, new)
map_gbd(x)
redistribute_gbd(def, expand = FALSE)
redistribute_gbd_all()
plot_idd(mrt, red, level)
plot_yll(mrt, red, level)
tab(x)
}

\arguments{
  \item{x}{generic argument name.}
  \item{target}{target distribution.}
  \item{new}{new dataset.}
  \item{def}{definition.}
  \item{expand}{should codes be expanded?}
  \item{mrt}{original dataset.}
  \item{red}{redistributed dataset.}
  \item{level}{cause level.}
}

\author{
Brecht Devleesschauwer <\email{brechtdv@gmail.com}>
}
