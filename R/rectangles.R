#' Rectangles
#'
#' Compute the 4 corner coordinates of a rectangle parameterised by its centre position (x, y),
#' its length, width and angle of rotation
#'
#' @param x Centre of rectangle in x
#' @param y Centre of rectangle in y
#' @param angle Angle (degrees) of rotation (measured from positive x to the rectangle long axis)
#' @param length Length of rectangle
#' @param width Width of rectangle
#'
#' @return A dataframe
#' @export
rects <- function(x, y, angle, length, width){

  # Convert angle to radians for computations
  angle_r <- angle * (pi/180)

  # Compute x_start and x_end of each rectangle (assuming zero width)
  xs <- x - cos(angle_r) * length/2
  xe <- x + cos(angle_r) * length/2
  ys <- y - sin(angle_r) * length/2
  ye <- y + sin(angle_r) * length/2

  # Compute the x and y coordinates of the 4 corners of the rectangle when the
  # width is considered.
  p1x <- xe - ((width/2) * cos((pi/2) - angle_r))
  p1y <- ye + ((width/2) * sin((pi/2) - angle_r))
  p2x <- xe + ((width/2) * cos((pi/2) - angle_r))
  p2y <- ye - ((width/2) * sin((pi/2) - angle_r))
  p3x <- xs + ((width/2) * cos((pi/2) - angle_r))
  p3y <- ys - ((width/2) * sin((pi/2) - angle_r))
  p4x <- xs - ((width/2) * cos((pi/2) - angle_r))
  p4y <- ys + ((width/2) * sin((pi/2) - angle_r))

  tibble::tibble(x = c(p1x, p2x, p3x, p4x),
         y = c(p1y, p2y, p3y, p4y))
  }
