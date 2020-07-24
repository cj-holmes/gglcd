#' Liquid Crystal Diagram (LCD) plot
#'
#' LCD is a wrapper for gglcd::geom_lc() gglcd::and stat_lc(). It produces simple LC alignmnet diagrams using ggplot2
#'
#' @param angle_b Angle of molecules at bottom of plot (degrees, measured anti-clockwise from +ve x)
#' @param angle_t Angle of molecules at top of plot (degrees, measured anti-clockwise from +ve x)
#' @param angle_function A function that takes a value of the image height (from 0 to 1) and returns the angle of
#'   a molecule at that height
#' @param angle_n Noise to be added to angles (a random angle from -angle_n:angle_n is added to each molecule)
#' @param n_mol_x Number of molecules in x (default is 30)
#' @param n_mol_y Number of molecules in y (default is 30)
#' @param x_jitter Jitter to apply to molecule centres in x. Value is applied as a proportion of lc_length (default = 0.4)
#' @param y_jitter Jitter to apply to molecule centres in y. Value is applied as a proportion of lc_length (default = 0.4)
#' @param show_function Logical. Print functional form of angle_function over diagram
#' @param lc_length Length of LC molecule (vector of length 1 or n_mol)
#' @param lc_width Width of LC molecule (vector of length 1 or n_mol)
#' @param lc_shape One of "rectangle" (default) or "ellipse"
#' @param diagram_aspect Aspect ratio of diagram (length / width)
#' @param seed Random seed for reproducability
#' @param surface_b Height of bottom surface layer
#' @param surface_t Height of top surface layer
#' @param return_df Should just the dataframe be returned (Default: FALSE)
#' @param lc_fill Fill colour for LC molecules
#' @param lc_col Border colour for LC molecules
#' @param lc_lwd Linewidth of LC molecules
#' @param bg_fill Fill colour for diagram background
#' @param bg_col Line colour for diagram background
#' @param bg_lwd Linewidth of plot border
#' @param surface_b_fill Fill colour of bottom surface
#' @param surface_b_col Line colour of bottom surface
#' @param surface_b_lwd Linewidth of bottom surface
#' @param surface_t_fill Fill colour of top surface
#' @param surface_t_col Line colour of top surface
#' @param surface_t_lwd Linewidth of top surface
#' @param ellipse_res Resolution of ellipse polygons
#' @param themeing Should themeing be applied (Default: TRUE)

#'
#' @return
#' @export
#'
#' @examples
#' lcd(0, 360)
#' lcd(0, 360, return_df = TRUE)
lcd <- function(angle_b = 0,
                angle_t = 90,
                angle_function = NULL,
                angle_n = 0,

                n_mol_x = 30,
                n_mol_y = 30,
                x_jitter = 0.4,
                y_jitter = 0.4,

                lc_length = 0.05,
                lc_width = 0.016,
                diagram_aspect = 1,

                seed = NULL,
                surface_b = NULL,
                surface_t = NULL,

                lc_shape = "rectangle",
                ellipse_res = 35,

                # Only return data
                return_df = FALSE,

                # Apply themeing
                themeing = TRUE,

                show_function = FALSE,

                # Line sizes
                lc_lwd = 0.2,
                surface_b_lwd = 0.2,
                surface_t_lwd = 0.2,
                bg_lwd = 0.2,

                # Colours
                lc_fill = "grey80",
                lc_col = "black",
                bg_fill = "white",
                bg_col = "black",
                surface_b_fill = "black",
                surface_b_col = NA,
                surface_t_fill = "black",
                surface_t_col = NA){


  # Set seed if present
  if(!is.null(seed)) set.seed(seed)

  # Define plot dimensions
  # Plot height is always 1. Plot width changes
  h <- 1
  w <- h*diagram_aspect

  # If no angle function is supplied, create a linear one here
  if(is.null(angle_function)){
    angle_function <-
      approxfun(x = seq(0, 1, l=(n_mol_x*n_mol_y)),
                y = seq(angle_b, angle_t, l=(n_mol_x*n_mol_y)),
                rule = 2)
  }

  t <-
    tidyr::crossing(x = seq(0, w, l=n_mol_x),
                    y = seq(0, h, l=n_mol_y)) %>%
      dplyr::mutate(x = x + runif(n_mol_x*n_mol_y, min = -lc_length*x_jitter, max = lc_length*x_jitter),
                    y = y + runif(n_mol_x*n_mol_y, min = -lc_length*y_jitter, max = lc_length*y_jitter)) %>%
      dplyr::arrange(y) %>%
      dplyr::mutate(width = lc_width,
                    length = lc_length) %>%
      dplyr::mutate(angle = angle_function(y) + runif(n_mol_x * n_mol_y, angle_n*-1, angle_n))

  if(return_df) return(t)

  p <-
    ggplot2::ggplot(t)+
    geom_lc(ggplot2::aes(x, y,
                         angle = angle,
                         length = length,
                         width = width),
            fill = lc_fill,
            col = lc_col,
            lwd = lc_lwd,
            lc_shape = lc_shape,
            ellipse_res = ellipse_res)

  # Apply surfaces if supplied
  if(!is.null(surface_b) | !is.null(surface_t)){
    p <-
      p +
      ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = Inf,
                        ymin = 0-surface_b, ymax = 0,
                        fill=surface_b_fill, col=surface_b_col, size = surface_b_lwd)+
      ggplot2::annotate(geom = "rect", xmin = -Inf, xmax = Inf,
                        ymin = 1, ymax = 1+surface_t,
                        fill=surface_t_fill, col=surface_t_col, size = surface_t_lwd)
  }


  if(show_function){
    normalised_angle_values <-
      tibble::tibble(x = seq(0, h, l=1000),
                     y = angle_function(x),
                     y_norm = ((y - min(y))/max(y - min(y)))*w)

    # print(normalised_angle_values)

    p <-
      p +
      geom_line(data = normalised_angle_values,
                aes(x=y_norm, y=x), orientation = "y", col=2, size=1)

  }

  if(themeing){
    p <-
      p +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult=0, add=0))+
      ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult=0, add=0))+
      ggplot2::coord_fixed(xlim = c(0,w),
                           ylim = c(0 - ifelse(is.null(surface_b), 0, surface_b),
                                  h + ifelse(is.null(surface_t), 0, surface_t)))+
      ggplot2::theme_bw()+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill=bg_fill, colour = bg_col, size = bg_lwd),
                     panel.border = ggplot2::element_rect(colour = bg_col, fill = NA, size = bg_lwd),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())

    return(p)
  }

  p + ggplot2::coord_fixed()
  }
