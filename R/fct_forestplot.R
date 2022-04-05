#' Draw a Forestplot of Measures of Effects
#'
#' 
#' This version is a simplified version of `ggforest::forestplot` that removes confidence interval 
#' computation and allows for a  lower bound and upper bound values
#'
#' @param df A data frame with the data to plot. It must contain at least three
#' variables, a character column with the names to be displayed on the y-axis
#' (see parameter \code{name}), a numeric column with the value (or the log of the
#' value) to display (see parameter \code{estimate}) .
#' @param name the variable in \code{df} that contains the y-axis
#' names. This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @param estimate the variable in \code{df} that contains the values to be displayed. 
#' This argument is automatically
#' \link[rlang:quotation]{quoted} and \link[rlang:eval_tidy]{evaluated} in the
#' context of the \code{df} data frame.
#' See Note.
#' @param lower the variable in \code{df} that contains the values to be displayed
#' @param upper the variable in \code{df} that contains the values to be displayed.
#' @param colour the variable in \code{df} by which to colour the different
#' @param shape the variable in \code{df} by which to shape the different
#' @param r_legend the variable in \code{df} that contains right legend information
#' @param l_legend the variable in \code{df} that contains left legend information
#' groups of points. This argument is automatically \link[rlang:quotation]{quoted} and
#' \link[rlang:eval_tidy]{evaluated} in the context of the \code{df} data frame.
#' See Note.
#' @param xrange default range of x range. Alternate striped background will use this value. 
#' The actual xlim parameter from ggplot will be expanded by 10% each side.
#' Values outside this range will not be diplayed
#' @param ... \code{ggplot2} graphical parameters such as \code{title},
#' @param xbreaks breaks on x axis
#' @param base_size base font size, given in pts.
#' @param legend_size Size of left and right legend (not sure of unit but definitively not the same as base_size)
#' \code{ylab}, \code{xlab}, \code{xtickbreaks} etc. to be passed along.
#' @return A \code{ggplot} object.
#' @author Maria Kalimeri, Ilari Scheinin, Vilma Jagerroos
#' @importFrom magrittr %>%
#' @importFrom rlang := !! 
#' @export

forestplot <- function(df,
                       name = author, ## To be modified later
                       r_legend = NULL,
                       l_legend = NULL,
                       estimate = VE,
                       lower = LB,
                       upper = UB,
                       colour = NULL,
                       shape = NULL,
                       xrange = c(0,100),
                       xbreaks = c(0,25,50,75,100),
                       base_size = 13,
                       legend_size = 3,
                       ...) {
  
  
  # Input checks
  stopifnot(is.data.frame(df))
  
  # TODO: Add some warnings when name, estimate etc are missing from df and user
  # is not defining the name, estimate etc explicitly.
  
  # Quote input
  name <- rlang::enquo(name)
  r_legend <- rlang::enquo(r_legend)
  l_legend <- rlang::enquo(l_legend)
  estimate <- rlang::enquo(estimate)
  upper <- rlang::enquo(upper)
  lower <- rlang::enquo(lower)
  colour <- rlang::enquo(colour)
  shape <- rlang::enquo(shape)
  
  args <- list(...)
  
  
  # Adjust xrange variable ------------------------------------------------------------------------
  
  xspan <- xrange[2]- xrange[1]  
  xlim <- xrange + xspan * c(-0.1, 0.1)
  
  
  # Adjust data frame variables-------------
  
  df <-
    df %>% 
    ## Remove values under 0 and above 100 in lower and upper
    dplyr::mutate(
      !!lower := pmax(!!lower, xrange[1]),
      !!upper := pmin(!!upper, xrange[2])
    ) %>%
    # Convert to a factor to preserve order. TODO: modify this behaviour
    dplyr::mutate(
      !!name := factor(!!name)
    ) 
  # Plot---------------
  g <-
    ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = !!estimate,
        y = !!name
      )
    )
  
  g <-
    g +
    # Add custom theme
    theme_forest(base_size = base_size) +
    # Add Nightingale colour palette
    scale_colour_ng_d() +
    scale_fill_ng_d() +
    # Add striped background
    geom_stripes(
      xmin = xrange[2],
      xmax = xrange[1]
    ) 
  
  g <-
    g +
    # And point+errorbars
    #geom_effect(
    ggplot2::geom_pointrange(
      ggplot2::aes(
        xmin = !!lower,
        xmax = !!upper,
        color = !!colour,
        shape = !!shape,
        group = !!name ## necessary to avoid grouping by color
      ),
      position = ggstance::position_dodge2v(0.9, reverse = FALSE),
      size = 1,
      fatten = 4
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = !!r_legend),
      size = legend_size,
      x = xrange[2] + 0.02 * xspan,
      color = "black", hjust = 0,
      position = ggstance::position_dodge2v(1, reverse = FALSE)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = !!l_legend),
      size = legend_size,
      x = xrange[1] - 0.02 * xspan,
      color = "black", hjust = 1,
      position = ggstance::position_dodge2v(1, reverse = FALSE)
    ) +
    # Define the shapes to be used manually
    ggplot2::guides(
      colour = ggplot2::guide_legend(reverse = TRUE),
      shape = ggplot2::guide_legend()
    ) 
  
  if(!is.null(xbreaks)){
    g <- g +
      ggplot2::scale_x_continuous(
        breaks = xbreaks
      ) 
  }
  
  
  # scale_y_continuous(
  #   breaks = seq_along(levels(!!name)),
  #   labels = levels(!!name),
  # ) 
  # Pass through graphical parameters and define defaults values for some.
  if ("title" %in% names(args)) {
    g <- g + ggplot2::labs(title = args$title)
  }
  if ("subtitle" %in% names(args)) {
    g <- g + ggplot2::labs(subtitle = args$subtitle)
  }
  if ("caption" %in% names(args)) {
    g <- g + ggplot2::labs(caption = args$caption)
  }
  if ("xlab" %in% names(args)) {
    g <- g + ggplot2::labs(x = args$xlab)
  }
  if (!"ylab" %in% names(args)) {
    args$ylab <- ""
  }
  g <- g + ggplot2::labs(y = args$ylab)
  if (!is.null(xlim)) {
    g <- g + ggplot2::coord_cartesian(
      xlim = xlim,
      clip = 'off'
    )
  }
  if ("ylim" %in% names(args)) {
    g <- g + ggplot2::ylim(args$ylim)
  }
  if ("xtickbreaks" %in% names(args)) {
    g <- g + ggplot2::scale_x_continuous(breaks = args$xtickbreaks)
  }
  g
}


