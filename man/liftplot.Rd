\name{liftplot}
\alias{liftplot}
\title{liftplot is a subclass of scoreFactory. Used to summarize scores by quantiles. Includes methods for plotting liftcharts. Defaults largely based on how the model was created (e.g. part_by argument, targets, etc.), but can be overridden for higher level summaries.}
\usage{
  liftplot(object, targets = "all",
    data_split = "Validation", agg_over_grps = TRUE,
    n = 10, ...)
}
\arguments{
  \item{object}{A scoreFactory object}

  \item{grp_by}{}

  \item{targets}{}

  \item{data_split}{}

  \item{agg_over_grps}{}

  \item{n}{}
}
\description{
  Example situation:
}

