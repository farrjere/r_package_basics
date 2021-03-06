% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/univariate.R
\name{plot_univariates}
\alias{plot_univariates}
\title{Helper function for rendering univariate plots}
\usage{
plot_univariates(plot_list)
}
\arguments{
\item{plot_list}{The plots list that univariate_analysis generated}
}
\description{
\code{plot_univariates} Renders univariate plots into 3 plots, one per plot type
}
\details{
This function is intended to be used with @seealso \code{\link{univariate_analysis}}
}
\examples{
\dontrun{results = univariate_analysis(mtcars)}
\dontrun{plot_univariates(results$plots)}
}
