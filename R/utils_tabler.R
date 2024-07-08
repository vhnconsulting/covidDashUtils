## Utils function for tablerDash template (based on tabler.io)

#' Build a tabler dashboard Dropdown menu to be placed inside tablerNavMenu()
#'
#' This items can replace a tablerNavMenuItem
#'
#' @param ... slot for tablerNavMenuDropItem elements
#' @param text Name displayed in navbar
#' @param icon Item icon
#'
#' @export
#'
tablerNavMenuDrop <- function(..., text = NULL, icon = NULL) {
  shiny::tags$li(
    class = "nav-item dropdown",
    shiny::tags$a(
      class = "nav-link dropdown-toggle",
      href = "#navbar-base",
      `data-toggle` = "dropdown",
      `data-auto-close` = "outside",
      role = "button",
      `aria-expanded` = "false",
      tablerDash::tablerIcon(name = icon, lib = "feather"),
      shiny::tags$span(
        class = "nav-link-title",
        text
      )
    ),
    shiny::tags$div(
      class = "dropdown-menu",
      ...
    )
  )
}

#' Build a tabler dashboard Dropdown item to be placed inside tablerNavMenuDrop()
#'
#' @param ... Item name displayed in the UI
#' @param tabName Should correspond exactly to the tabName given in tablerTabItem
#'
#' @export

tablerNavMenuDropItem <- function(..., tabName = NULL){
  shiny::tags$a(
    # class = "nav-link",
    class = "dropdown-item",
    id = paste0("tab-", tabName),
    href = paste0("#shiny-tab-", tabName),
    `data-toggle` = "tab",
    `data-value` = tabName,
    # `data-target` = paste0("#", tabName),
    ...
  )
}

#' A condensed version of tablerDashNav.
#'
#' One header only and not a navbar below the header
#' @param id Navbar id
#' @param ... Navbar content (will be placed on the right side of header)
#' @param src logo image source (displayed on the left side)
#' @param navMenu Slot for tablerNavMenu
#'
#' @importFrom shiny tags
#'
#' @export

tablerDashNavCondensed <- function(id, ..., src = NULL, navMenu = NULL) {

  shiny::tags$div(
    class = "header navbar navbar-expand-md navbar-light d-print-none",
    shiny::tags$div(
      class = "container-xl",
      tags$button(
        class = "navbar-toggler",
        type = "button",
        `data-bs-toggle` = "collapse",
        `data-bs-target` = "#navbar-menu",
        tags$span(class = "navbar-toggler-icon")
      ),
      tags$div(
        class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        tags$a(
          href = ".",
          shiny::img(
            src = src,
            class = "navbar-brand-image"
          )

        )
      ),
      tags$div(
        class  = "navbar-nav flex-row order-md-last",
        lapply(list(...), shiny::tagAppendAttributes, class = "mx-2")
      ),
      tags$div(
        class = "collapse navbar-collapse",
        id = "navbar-menu",
        tags$div(
          class = "d-flex flex-column flex-md-row flex-fill align-items-stretch align-items-md-center",
          navMenu
        )
      )
    )
  )
}



#' Variation of tablerCard with different default options and a fix a bug in zoomable cards
#'
#' @inheritParams tablerDash::tablerCard
#'
#' @export
#'
tablerCardZoomable <- function(..., title = NULL, options = NULL, footer = NULL,
                               status = NULL, statusSide = c("top", "left"),
                               collapsible = TRUE, collapsed = FALSE, closable = FALSE,
                               zoomable = FALSE, width = NULL, overflow = FALSE) {

  statusSide <- match.arg(statusSide)

  statusCl <- if (!is.null(status)) paste0("card-status bg-", status)
  if (!is.null(status)) {
    if (statusSide == "left") {
      statusCl <- paste0(statusCl , " card-status-left")
    }
  }

  cardCl <- "card"
  if (collapsed) cardCl <- paste0(cardCl, " card-collapsed")

  cardTag <- shiny::tags$div(
    class = cardCl,
    style =  if (overflow) "max-height: 500px; overflow-y: auto;" else NULL,
    if (!is.null(status)) shiny::tags$div(class = statusCl),
    # header
    if (!is.null(title)) {
      shiny::tags$div(
        class = "card-header",
        shiny::tags$h3(class = "card-title", title),

        # card toolbox and other elements such as buttons, ...
        shiny::tags$div(
          class = "card-options",
          if (!is.null(options)) {
            lapply(options, shiny::tagAppendAttributes, class = "mx-1")
          },
          if (collapsible) {
            shiny::tags$a(
              href = "#",
              class = "card-options-collapse",
              `data-toggle` = "card-collapse",
              tablerDash::tablerIcon(name = "chevron-up", lib = "feather")
            )
          },
          if (zoomable) {
            shiny::tags$a(
              href = "#",
              class = "card-options-fullscreen",
              `data-toggle` = "card-fullscreen",
              tablerDash::tablerIcon(name = "maximize", lib = "feather")
            )
          },
          if (closable) {
            shiny::tags$a(
              href = "#",
              class = "card-options-remove",
              `data-toggle` = "card-remove",
              tablerDash::tablerIcon(name = "x", lib = "feather")
            )
          }
        )
      )
    },
    # body
    shiny::tags$div(class = "card-body", ...),
    # footer
    if (!is.null(footer)) shiny::tags$div(class = "card-footer", footer)
  )

  if (!is.null(width)) shiny::column(width = width, cardTag) else cardTag

}

#' overwrite default tablerDashBody
#'
#' @param ... element to put inside Dash body
#'
#' @export
#'
tablerDashBody <- function(...){
  shiny::tags$div(
    class = "page-body",
    shiny::tags$div(
      class = "container-fluid",
      ...
    )
  )
}


#' Create a Boostrap 4 dashboard page
#' Overwrite default function from {tablerDash} package that crash when there is no internet connection.
#'
#' Build a tabler dashboard page
#'
#' @param navbar Slot for \link{tablerDashNav}.
#' @param body Slot for \link{tablerDashBody}.
#' @param footer Slot for \link{tablerDashFooter}.
#' @param title App title.
#' @param enable_preloader Whether to enable a page loader. FALSE by default.
#' @param loading_duration Loader duration in seconds. 2s by default.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(tablerDash)
#'
#'  shiny::shinyApp(
#'    ui = tablerDashPage(
#'     navbar = tablerDashNav(),
#'     footer = tablerDashFooter(),
#'     title = "test",
#'     body = tablerDashBody()
#'    ),
#'    server = function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
tablerDashPage <- function(navbar = NULL, body = NULL,
                        footer = NULL, title = NULL,
                        enable_preloader = FALSE,
                        loading_duration = 2){


  # hide body and footer
  bodyContent <- shiny::tags$div(
    class = "page-main",
    style = if (enable_preloader) "visibility: hidden;" else NULL,
    navbar,
    body
  )

  footer <- shiny::tagAppendAttributes(footer, class = "footer")

  footer <- if (enable_preloader) {
    shiny::tagAppendAttributes(footer, style = "visibility: hidden;")
  } else {
    footer
  }

  # preloader
  preloader <- shiny::tags$div(
    class = "dimmer active",
    shiny::tags$div(
      class = "loader",
      style = "
        position: fixed;
        z-index: 999;
        height: 2em;
        width: 2em;
        overflow: visible;
        margin: auto;
        top: 0;
        left: 0;
        bottom: 0;
        right: 0;",
      shiny::tags$div(
        class = "dimmer-content"
      )
    )
  )

  shiny::tags$html(
    # Head
    shiny::tags$head(
        shiny::tags$meta(charset="UTF-8"),
        shiny::tags$meta(
          name = "viewport",
          content = "
          width=device-width,
          user-scalable=no,
          initial-scale=1.0,
          maximum-scale=1.0,
          minimum-scale=1.0"
        ),
        shiny::tags$meta(`http-equiv` = "X-UA-Compatible", content = "ie=edge"),
        shiny::tags$meta(`http-equiv` = "Content-Language", content = "en"),
        shiny::tags$meta(name = "msapplication-TileColor", content = "#2d89ef"),
        shiny::tags$meta(name = "theme-color", content = "#4188c9"),
        shiny::tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "black-translucent"),
        shiny::tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
        shiny::tags$meta(name = "mobile-web-app-capable", content = "yes"),
        shiny::tags$meta(name = "HandheldFriendly", content = "True"),
        shiny::tags$meta(name = "MobileOptimized", content = "320"),
        #<link rel="icon" href="./favicon.ico" type="image/x-icon">
        #<link rel="shortcut icon" type="image/x-icon" href="./favicon.ico">
      shiny::tags$title(title)
    ),
    # Body
    tablerDash:::addDeps(
      shiny::tags$body(
        class = NA,
        # set up a time-out for the preloader
        # The body content disappears first,
        # then 2s after, the preloader disappears and
        # the body content is shown
        onload = if (enable_preloader) {
          duration <- loading_duration * 1000
          paste0(
            "$(document).ready(function() {
              setTimeout(function(){
                $('.dimmer.active').remove();
                $('.page-main').css('visibility', 'visible');
                $('.footer').css('visibility', 'visible');
                }, ", duration, ");
            });
            "
          )
        },
        shiny::tags$div(
          class = "page",
          if (enable_preloader) preloader,
          bodyContent,
          footer
        )
      )
    )
  )
}