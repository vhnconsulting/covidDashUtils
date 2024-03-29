% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_forestplot.R
\name{ng_palette_d}
\alias{ng_palette_d}
\title{Nightingale's colour palettes}
\usage{
ng_palette_d(name = "all", reverse = FALSE)
}
\arguments{
\item{name}{Character name of the Nightingale (or viridis) colour palette.}

\item{reverse}{Boolean indicating whether the palette should be reversed.}
}
\description{
\code{ng_palette_d()} and \code{ng_palette_c()} (respectively for discrete
and continuous palettes) return functions that take an integer argument (the
required number of colours) and return a character vector of colours' hex
codes.
In addition, the functions also recognize the
\code{\link[viridisLite:viridis]{viridis}} palettes: "magma" (or "A"),
"inferno" ("B"), "plasma" ("C"), "viridis" ("D"), or "cividis" ("D").
}
\author{
Ilari Scheinin
}
