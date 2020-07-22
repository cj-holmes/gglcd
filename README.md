
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gglcd <img src="data-raw/hex-logo/gglcd-hex.png" align="right" height="139"/>

**`gglcd` is a work in progress - use at your own risk\!**

The goal of `gglcd` is to programmatically create schematic liquid
crystal (LC) alignment diagrams.

`gglcd` has two features:

  - A ggplot2 geom and stat (`geom_lc()` and `stat_lc()`) for fine
    control
  - A convenient wrapper function `lcd()` for simple plots with linear
    changes in molecule angle

## Example use

``` r
library(gglcd)
library(tidyverse)
```

For quick LC alignment diagrams you can use `lcd()` and specify the
bottom and top theta anlgle arguments

``` r
lcd(45, -45, 1000)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

`lcd` returns a ggplot2 object, so further layers cab be added as normal

``` r
lcd(0, 90) + labs(title = "Title", subtitle = "Subtitle")
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

There are a whole load of other arguments to `lcd()` to control the
format of the output

``` r
args(lcd)
#> function (theta_b = 0, theta_t = 90, n_mol = 1000, theta_n = 0, 
#>     lc_length = 0.05, lc_width = 0.05/3.5, diagram_aspect = 1, 
#>     seed = NULL, surface_b = NULL, surface_t = NULL, lc_shape = "rectangle", 
#>     ellipse_res = 35, return_df = FALSE, themeing = TRUE, lc_lwd = 0.2, 
#>     surface_b_lwd = 0.2, surface_t_lwd = 0.2, bg_lwd = 0.2, lc_fill = "grey80", 
#>     lc_col = "black", bg_fill = "white", bg_col = "black", surface_b_fill = "black", 
#>     surface_b_col = NA, surface_t_fill = "black", surface_t_col = NA) 
#> NULL
```

For example, the aspect of the diagram can be changed, bounding surfaces
applied and colour elements edited.

``` r
lcd(0, 360, diagram_aspect = 2, surface_b = 0.05, surface_t = 0.05, bg_fill = "aquamarine", lc_fill="white")
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

## Geoms and stats

The `lcd()` function described above is a wrapper aronud `geom_lc()` and
`stat_lc()`. These functions can be used as normal with a call to
`ggplot()` for full control over the diagram creation, including the
ability to make additional aesthetic mappings.

A particularly useful argument to `lcd()` is `return_df`. If set to
`TRUE` (default is `FALSE`), a dataframe of the required aesthetic
mappings (x, y, angle, length, width) is returned, rather than a plot.
This is a convenient way to produce a dataframe that can be passed to
`ggplot()` (example below).

This example also makes use of the ability to map non-required
aesthetics. The fill is mapped to angle.

``` r
lcd(-180, 180, n_mol = 1000, return_df = T, seed=1) %>%
  print() %>% # view dataframe
  ggplot(aes(x, y, angle=angle, length=length, width=width))+
  geom_lc(aes(fill=angle))+
  coord_fixed()+
  scale_fill_gradient2()
#> # A tibble: 1,000 x 5
#>        x        y  width length angle
#>    <dbl>    <dbl>  <dbl>  <dbl> <dbl>
#>  1 0.612 0.000605 0.0143   0.05 -180 
#>  2 0.442 0.00103  0.0143   0.05 -180.
#>  3 0.912 0.00136  0.0143   0.05 -179.
#>  4 0.141 0.00164  0.0143   0.05 -179.
#>  5 0.839 0.00202  0.0143   0.05 -179.
#>  6 0.956 0.00330  0.0143   0.05 -178.
#>  7 0.480 0.00471  0.0143   0.05 -178.
#>  8 0.935 0.00573  0.0143   0.05 -177.
#>  9 0.894 0.00596  0.0143   0.05 -177.
#> 10 0.925 0.00730  0.0143   0.05 -177.
#> # ... with 990 more rows
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

As length is a required aesthetic for `geom_lc()`, it can be varied per
LC molecule. Here I have varied the length for the ‘twist’ category,
approximating the projection of the rotated molecule onto the x-y plane.

``` r
# Define inputs
n <- 1000
lc_length <- 0.05
lc_width <- lc_length/3

# Approximate the length projection on to the x-y plane
twist_proj <- cos(seq(0, pi/2, l=n))*lc_length
twist_proj[twist_proj < lc_width] <- lc_width # Do not molecule lengths to become smaller than widths

bind_rows(
  lcd(50, -50, n, lc_length=lc_length, lc_width=lc_width, return_df = TRUE) %>% mutate(label = "Splay"),
  lcd(50, 130, n, lc_length=lc_length, lc_width=lc_width, return_df = TRUE) %>% mutate(label = "Bend"),
  lcd(0, 0, n, lc_length = twist_proj, lc_width=lc_width, return_df = TRUE) %>% mutate(label = "Twist")) %>% 
  ggplot(aes(x, y, angle=angle, length=length, width=width))+
  geom_lc()+
  facet_wrap(~label)+
  coord_fixed()+
  theme(legend.position = "none")
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->

### Using ellipse polygons

If using the `lc_shape = 'ellipse'` option, a further argument
`ellipse_res` can be used to control the number of equally spaced points
(in angle) that make up each individual ellipse. The default is
`ellipse_res = 35`

``` r
tibble(x=0.5, y=0.5, angle=45, length=1, width=1/4) %>% 
  ggplot(aes(x, y, angle=angle, length=length, width=width))+
  geom_polygon(stat="lc", aes(col="rectangle"), fill=NA, size=1)+
  geom_polygon(stat="lc", lc_shape='ellipse', ellipse_res = 60, aes(col="ellipse_res = 60"), fill=NA, size=1)+
  geom_polygon(stat="lc", lc_shape='ellipse', ellipse_res = 35, aes(col="ellipse_res = 35"), fill=NA, size=1)+
  geom_polygon(stat="lc", lc_shape='ellipse', ellipse_res = 10, aes(col="ellipse_res = 10"), fill=NA, size=1)+
  geom_polygon(stat="lc", lc_shape='ellipse', ellipse_res = 5, aes(col="ellipse_res = 05"), fill=NA, size=1)+
  coord_fixed()
```

![](man/figures/README-unnamed-chunk-9-1.png)<!-- -->

## Animations

With the super useful `gganimate` package it is also possible to create
custom animations using `geom_lc()`.
![](man/figures/README-unnamed-chunk-10-1.gif)<!-- -->
