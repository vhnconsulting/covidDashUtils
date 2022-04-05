## Utils function from ggpforestpackage

#' Forestplot Theme
#'
#' A custom theme used in \code{\link{forestplot}} that builts upon
#' \link[ggplot2]{theme_minimal}.
#'
#' @inheritParams ggplot2::theme_minimal
#' @importFrom ggplot2 %+replace%
#' @author Maria Kalimeri


theme_forest <- function(base_size = 13,
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  ggplot2::theme_minimal(
    base_size = base_size,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(
      rect = ggplot2::element_blank(),
      text = ggplot2::element_text(
        colour = "black"
      )
    ) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0
      ),
      axis.text.y = ggplot2::element_text(
        colour = "black",
        size = 12,
        hjust = 0
      ),
      axis.text.y.right = ggplot2::element_text(
        hjust = 1
      ),
      axis.text.x = ggplot2::element_text(
        colour = "black"
      ),
      panel.border = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        hjust = 0
      ),
      panel.background = ggplot2::element_rect(
        colour = NA,
        fill = NA
      ),
      panel.grid.major.x = ggplot2::element_line(
        colour = "gray50",
        size = 0.25,
        linetype = 2
      ),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "right"
    )
}

ng_colour_list <-
  c(
    `night` = "#323D5A",
    `warm red` = "#FF4A4F",
    `electro` = "#00C0C0",
    `violet` = "#8F87FF",
    `pesto` = "#CCCF40",
    
    `mid night` = "#508CC8",
    `dark red` = "#A0192D",
    `dark electro` = "#2D506E",
    `dark violet` = "#5028C8",
    `dark pesto` = "#006E00",
    
    `light night` = "#C8DCF0",
    `light red` = "#FFC9CA",
    `light electro` = "#BEF0F0",
    `light violet` = "#DCDCFF",
    `light pesto` = "#78D700",
    
    `yellow` = "#FFFF00",
    
    `black` = "#000000",
    `graphite` = "#4D4D4D",
    `fog` = "#E6E6E6",
    `white` = "#FFFFFF"
  )

#' Nightingale's colours
#'
#' \code{ng_colour()} returns Nightingale's colours' hex codes.
#' \code{display_ng_colours()} displays the avaliable colours, with their names
#' and hex codes, in a vertical layout.
#'
#' @param ... Character names of Nightingale's colours.
#' @author Ilari Scheinin

ng_colour <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(ng_colour_list)
  }
  
  unname(ng_colour_list[cols])
}


ng_palette_list <-
  list(
    primary = ng_colour("night", "warm red", "electro", "violet", "pesto"),
    
    dark = ng_colour(
      "mid night", "dark red", "dark electro",
      "dark violet", "dark pesto"
    ),
    light = ng_colour(
      "light night", "light red", "light electro",
      "light violet", "light pesto"
    ),
    accents = ng_colour(
      "mid night", "light night", "dark red", "light red",
      "dark electro", "light electro", "dark violet", "light violet",
      "dark pesto", "light pesto"
    ),
    
    supportive = ng_colour("yellow"),
    
    blacks = ng_colour("black", "graphite", "fog", "white"),
    
    night = ng_colour("night", "mid night", "light night"),
    red = ng_colour("dark red", "warm red", "light red"),
    electro = ng_colour("dark electro", "electro", "light electro"),
    violet = ng_colour("dark violet", "violet", "light violet"),
    pesto = ng_colour("dark pesto", "pesto", "light pesto"),
    
    nwr = ng_colour("night", "electro", "white", "warm red", "dark red"),
    pwr = ng_colour(
      "dark pesto", "light pesto", "white", "warm red",
      "dark red"
    ),
    nwp = ng_colour("night", "electro", "white", "light pesto", "dark pesto"),
    pyr = ng_colour("dark pesto", "yellow", "warm red"),
    py = ng_colour("dark pesto", "yellow"),
    ne = ng_colour("night", "electro", "light electro"),
    new = ng_colour("night", "electro", "white"),
    rw = ng_colour("dark red", "warm red", "white"),
    pw = ng_colour("dark pesto", "light pesto", "white"),
    
    all = ng_colour(names(ng_colour_list))
  )

#' Nightingale's colour palettes
#'
#' \code{ng_palette_d()} and \code{ng_palette_c()} (respectively for discrete
#' and continuous palettes) return functions that take an integer argument (the
#' required number of colours) and return a character vector of colours' hex
#' codes.
#' In addition, the functions also recognize the
#' \code{\link[viridisLite:viridis]{viridis}} palettes: "magma" (or "A"),
#' "inferno" ("B"), "plasma" ("C"), "viridis" ("D"), or "cividis" ("D").
#'
#' @param name Character name of the Nightingale (or viridis) colour palette.
#' @param reverse Boolean indicating whether the palette should be reversed.
#'
#' @rdname ng_palette
#' @author Ilari Scheinin

ng_palette_d <- function(name = "all", reverse = FALSE) {
  if (name %in% names(ng_palette_list)) {
    function(n) {
      pal <- ng_palette_list[[name]]
      if (n > length(pal)) {
        warning(
          'n too large, allowed maximum for palette "',
          name, '" is ', length(pal),
          call. = FALSE
        )
        pal <- rep(pal, length.out = n)
      } else {
        pal <- pal[seq_len(n)]
      }
      if (reverse) pal <- rev(pal)
      pal
    }
  } else if (name %in% c(
    "A", "magma",
    "B", "inferno",
    "C", "plasma",
    "D", "viridis",
    "E", "cividis"
  )) {
    scales::viridis_pal(
      option = name,
      direction = dplyr::if_else(reverse, true = -1L, false = 1L)
    )
  } else {
    stop("Unknown palette: ", name, call. = FALSE)
  }
}


#' Colour scale constructor for Nightingale colours
#'
#' @inheritParams ng_palette_d
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams scales::gradient_n_pal
#' @param ... Additional arguments passed to
#'        \code{\link[ggplot2:discrete_scale]{discrete_scale()}} or
#'        \code{\link[ggplot2:continuous_scale]{continuous_scale()}} to control
#'        name, limits, breaks, labels and so forth.
#' @param palette Character name of the Nightingale (or viridis) colour palette.
#' @param aesthetics Character string or vector of character strings listing the
#'        name(s) of the aesthetic(s) that this scale works with. This can be
#'        useful, for example, to apply colour settings to the `colour` and
#'        `fill` aesthetics at the same time, via
#'        `aesthetics = c("colour", "fill")`.
#'
#' @rdname ng_scale
#' @author Ilari Scheinin
scale_colour_ng_d <- function(..., palette = "all", reverse = FALSE,
                              aesthetics = "colour") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_d_", palette),
    palette = ng_palette_d(name = palette, reverse = reverse),
    ...
  )
}

#' @rdname ng_scale
#'
scale_fill_ng_d <- function(..., palette = "all", reverse = FALSE,
                            aesthetics = "fill") {
  ggplot2::discrete_scale(
    aesthetics = aesthetics,
    scale_name = paste0("ng_d_", palette),
    palette = ng_palette_d(name = palette, reverse = reverse),
    ...
  )
}


#' Alternating Background Colour
#'
#' Add alternating background color along the y-axis. The geom takes default
#' aesthetics \code{odd} and \code{even} that receive color codes. The codes
#' would preferably be in the 8-hex ARGB format to allow for transparency if
#' the geom is meant to be used as visual background.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_rect
#' @author Ilari Scheinin


geom_stripes <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}

GeomStripes <- ggplot2::ggproto(
  "GeomStripes", 
  ggplot2::Geom,
  required_aes = c("y"),
  
  default_aes = ggplot2::aes(
    xmin = 0, xmax = 100,
    odd = "#22222222", even = "#00000000",
    # Change 'size' below from 0 to NA.
    # When not NA then when *printing in pdf device* borders are there despite
    # requested 0th size. Seems to be some ggplot2 bug caused by grid overriding
    # an lwd parameter somewhere, unless the size is set to NA. Found solution here
    # https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA, colour = "black", linetype = "solid", size = NA
  ),
  
  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,
  
  draw_panel = function(data, panel_params, coord) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(.data$y),
          ymin = .data$y - 0.5,
          ymax = .data$y + 0.5
        ) %>%
        dplyr::select(
          .data$xmin, .data$xmax,
          .data$ymin, .data$ymax,
          .data$odd, .data$even,
          .data$alpha, .data$colour, .data$linetype, .data$size
        ) %>%
        unique() %>%
        dplyr::arrange(.data$ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .data$.n %% 2L == 1L,
            true = .data$odd,
            false = .data$even
          )
        ) %>%
        dplyr::select(-.data$.n, -.data$odd, -.data$even),
      panel_params,
      coord
    )
  }
)
