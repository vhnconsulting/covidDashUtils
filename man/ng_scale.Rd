% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_forestplot.R
\name{scale_colour_ng_d}
\alias{scale_colour_ng_d}
\alias{scale_fill_ng_d}
\title{Colour scale constructor for Nightingale colours}
\usage{
scale_colour_ng_d(..., palette = "all", reverse = FALSE, aesthetics = "colour")

scale_fill_ng_d(..., palette = "all", reverse = FALSE, aesthetics = "fill")
}
\arguments{
\item{...}{Additional arguments passed to
\code{\link[ggplot2:discrete_scale]{discrete_scale()}} or
\code{\link[ggplot2:continuous_scale]{continuous_scale()}} to control
name, limits, breaks, labels and so forth.}

\item{palette}{Character name of the Nightingale (or viridis) colour palette.}

\item{reverse}{Boolean indicating whether the palette should be reversed.}

\item{aesthetics}{Character string or vector of character strings listing the
name(s) of the aesthetic(s) that this scale works with. This can be
useful, for example, to apply colour settings to the \code{colour} and
\code{fill} aesthetics at the same time, via
\code{aesthetics = c("colour", "fill")}.}
}
\description{
Colour scale constructor for Nightingale colours
}
\author{
Ilari Scheinin
}
