\name{compare_models}
\alias{compare_models}
\title{Summarize a list of scoreFactory objects by r-squared and AUC on the validation set.}
\usage{
  compare_models(objects, targets = "all", show_rsq = TRUE,
    extraction = NULL, ...)
}
\arguments{
  \item{objects}{a scoreFactory object}

  \item{targets}{string of target names, defaults to all.}

  \item{show_rsq}{should r-squared be computed?}

  \item{extraction}{argument for only computing statistics
  on a subset of the validation population.}
}
\description{
  Example situation: compare R-squared, and AUC across a
  list of scoreFactory objects.
}

