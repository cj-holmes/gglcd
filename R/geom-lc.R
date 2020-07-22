#' @usage NULL
#' @format NULL
#' @importFrom ggplot2 .pt
#' @export
GeomLc <-
  ggplot2::ggproto(
    "GeomLc",
    ggplot2::Geom,
    required_aes = c("x", "y"),
    default_aes = ggplot2::aes(
      colour = 1,
      fill = "grey70",
      size = 0.2,
      linetype = 1,
      alpha = 1
    ),
    draw_key = ggplot2::draw_key_polygon,
    draw_panel = function(data, panel_params, coord) {

      coords <- coord$transform(data, panel_params)

      # Create a datafrane of the first row per molecule
      # Where the my_id is what was created in stat_lc() with row_number()
      firsts <- coords %>% dplyr::group_by(my_id) %>% dplyr::summarize_all(dplyr::first)
      #print(firsts)

      grid::polygonGrob(
        x = coords$x,
        y = coords$y,
        default.units = "native",
        id = coords$my_id,
        gp = grid::gpar(
          col = firsts$colour,
          fill = scales::alpha(firsts$fill, firsts$alpha),
          lwd = firsts$size * .pt,
          lty = firsts$linetype
        )
      )
    }
  )


#' @usage NULL
#' @format NULL
#' @export
StatLc <-
  ggplot2::ggproto(
    `_class` = "StatLc",
    `_inherit` = ggplot2::Stat,
    required_aes = c("x", "y", "angle", "width", "length"),
    compute_panel = function(data, scales, lc_shape = "rectangle", ellipse_res = 35) {
      if (lc_shape == "rectangle") {

        # Add a unique ID per row with row_number() and then map across x, y, angle, length, and width running rects()
        # This creates a list column of the long format new x and y coordinates (4 rows per 1 row)
        # Remove the original x and y columns and unnest the new x and y columns (this makes the
        # dataframe 4 times as long repeating the unique ID and anlge, length and width every 4 rows)
        out <-
          data %>%
          dplyr::mutate(my_id = dplyr::row_number(),
                        new = purrr::pmap(list(x, y, angle, length, width), rects)) %>%
          dplyr::select(-x, -y) %>%
          tidyr::unnest(new)
      }

      if (lc_shape == "ellipse") {
        # This does the same as above but for the ellipses() function. The dataframe is made ellipse_res times longer
        # Rather than just 4 times longer for the rectangles case
        out <-
          data %>%
          dplyr::mutate(my_id = dplyr::row_number(),
                        new = purrr::pmap(list(x, y, angle, length, width), ellipses, res = ellipse_res)) %>%
          dplyr::select(-x, -y) %>%
          tidyr::unnest(new)
      }
      out
    }
  )




#' Geom Liquid Crystal (LC)
#'
#' @inheritParams ggplot2::geom_polygon
#' @param lc_shape "rectangle" (default) or "Ellipse"
#' @param ellipse_res Resolution of ellipse polygons (Default: 35)
#'
#' @details Each row in the input dataframe is converted into the x-y coordinates
#'   that make the perimeter of the molecule rectangle (4 coordinates) or ellipse (ellipse_res coordinates)
#'
#' @section Aesthetics:
#'
#'   \code{geom_lc()} understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   \itemize{
#'     \item \strong{x}
#'     \item \strong{y}
#'     \item \strong{angle}
#'     \item \strong{length}
#'     \item \strong{width}
#'     \item fill
#'     \item alpha
#'     \item colour
#'     \item linetype
#'   }
#'
#' @export
geom_lc <-
  function(mapping = NULL,
           data = NULL,
           stat = "lc",
           position = "identity",
           show.legend = NA,
           inherit.aes = TRUE,
           na.rm = TRUE,
           lc_shape = "rectangle",
           ellipse_res = 35,
           ...) {
    ggplot2::layer(
      geom = GeomLc,
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, lc_shape = lc_shape, ellipse_res = ellipse_res, ...)
    )

  }

#' Stat Liquid Crystal (LC)
#'
#' @inheritParams ggplot2::geom_polygon
#' @param lc_shape "rectangle" (default) or "Ellipse"
#' @param ellipse_res Resolution of ellipse polygons (Default: 35)
#'
#' @details Each row in the input dataframe is converted into the x-y coordinates
#'   that make the perimeter of the molecule rectangle (4 coordinates) or ellipse (ellipse_res coordinates)
#'
#' @section Aesthetics:
#'
#'   \code{geom_lc()} understands the following aesthetics (required
#'   aesthetics are in bold):
#'
#'   \itemize{
#'     \item \strong{x}
#'     \item \strong{y}
#'     \item \strong{angle}
#'     \item \strong{length}
#'     \item \strong{width}
#'     \item fill
#'     \item alpha
#'     \item colour
#'   }
#'
#' @export
stat_lc <-
  function(mapping = NULL,
           data = NULL,
           geom = "lc",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           lc_shape = "rectangle",
           ellipse_res = 35,
           ...) {
    ggplot2::layer(
      stat = StatLc,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, lc_shape = lc_shape, ellipse_res = ellipse_res, ...)
    )
  }
