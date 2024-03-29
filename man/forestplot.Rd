% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forestplot.R
\name{forestplot}
\alias{forestplot}
\title{Draw a Forestplot of Measures of Effects}
\usage{
forestplot(
  df,
  name = author,
  r_legend = NULL,
  l_legend = NULL,
  estimate = VE,
  lower = LB,
  upper = UB,
  colour = NULL,
  shape = NULL,
  xrange = c(0, 100),
  xbreaks = c(0, 25, 50, 75, 100),
  base_size = 13,
  legend_size = 3,
  ...
)
}
\arguments{
\item{df}{A data frame with the data to plot. It must contain at least three
variables, a character column with the names to be displayed on the y-axis
(see parameter \code{name}), a numeric column with the value (or the log of the
value) to display (see parameter \code{estimate}) .}

\item{name}{the variable in \code{df} that contains the y-axis
names. This argument is automatically \link[rlang:quotation]{quoted} and
\link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
See Note.}

\item{r_legend}{the variable in \code{df} that contains right legend information}

\item{l_legend}{the variable in \code{df} that contains left legend information
groups of points. This argument is automatically \link[rlang:quotation]{quoted} and
\link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
See Note.}

\item{estimate}{the variable in \code{df} that contains the values to be displayed.
This argument is automatically
\link[rlang:quotation]{quoted} and \link[rlang:eval_tidy]{evaluated} in the
context of the \code{df} data frame.
See Note.}

\item{lower}{the variable in \code{df} that contains the values to be displayed}

\item{upper}{the variable in \code{df} that contains the values to be displayed.}

\item{colour}{the variable in \code{df} by which to colour the different}

\item{shape}{the variable in \code{df} by which to shape the different}

\item{xrange}{default range of x range. Alternate striped background will use this value.
The actual xlim parameter from ggplot will be expanded by 10\% each side.
Values outside this range will not be diplayed}

\item{xbreaks}{breaks on x axis}

\item{base_size}{base font size, given in pts.}

\item{legend_size}{Size of left and right legend (not sure of unit but definitively not the same as base_size)
\code{ylab}, \code{xlab}, \code{xtickbreaks} etc. to be passed along.}

\item{...}{\code{ggplot2} graphical parameters such as \code{title},}
}
\value{
A \code{ggplot} object.
}
\description{
This version is a simplified version of \code{ggforest::forestplot} that removes confidence interval
computation and allows for a  lower bound and upper bound values
}
\author{
Maria Kalimeri, Ilari Scheinin, Vilma Jagerroos
}
