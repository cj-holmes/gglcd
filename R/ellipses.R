#' Create general elipse perimeter coordinates
#'
#' Parameterised elipse equations. Provide an origin (x and y), minor and major radii,
#' an angle of rotation (measured from positive x to the elipse major axis) and a resolution.
#' The funtion returns a list of the x and y coordinates of the perimeter of the elipse.
#'
#' @param x Elipse centre x coordinate
#' @param y Elipse centre y coordinate
#' @param angle Angle (degrees) of rotation (measured from positive x to the elipse major axis)
#' @param res Number of points in elipse perimeter
#' @param length Length of major axis
#' @param width Length of minor axis
#'
#' @return A dataframe
#' @export
ellipses <- function(x, y, angle, length, width, res=50){

  # Generate all angles from 0 to 2*pi and resolution res
  t <- seq(0, 2*pi, l=res)

  # Convert angle to radians for computations
  angle_r <- angle * (pi/180)

  # Compute x and y coordinates of elipse perimeter
  # The equations use major and minor RADII (but I provide molecule TOTAL length and width so need to divide by 2)
  # Major RADIUS = length/2
  # Minor RADIUS = width/2

  xo <- x + ((length/2)*cos(t)*cos(angle_r)) - ((width/2) * sin(t) * sin(angle_r))
  yo <- y + ((length/2)*cos(t)*sin(angle_r)) + ((width/2) * sin(t) * cos(angle_r))

  tibble(x=xo, y=yo)

}
