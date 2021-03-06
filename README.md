<!-- README.md is generated from README.Rmd. Please edit that file -->

# dubois <img align="right" src="man/figures/dubois.png" alt="logo" width="200">

<!-- badges: start -->

[![Lifecycle:
experimental](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The R package, `dubois`, aids users to render Du Bois styled plots from
custom datasets.

W.E.B. Du Bois (February 23, 1868 – August 27, 1963) was an American
sociologist, socialist, historian and Pan-Africanist civil rights
activist. Born in Great Barrington, Massachusetts, Du Bois grew up in a
relatively tolerant and integrated community, and after completing
graduate work at the University of Berlin and Harvard, where he was the
first African American to earn a doctorate, he became a professor of
history, sociology and economics at Atlanta University. Du Bois was one
of the founders of the National Association for the Advancement of
Colored People (NAACP) in 1909 (*Source: Wikpedia*).

In the 1900 Paris Exposition his team made a historical display. They
presented in many charts the situation of Black people in Georgia and
the US in general. The timelessness of these posters provoked the
dataviz community and they created challenges to reproduce them using
modern tools. The
[#DuBoisChallenge2022](https://github.com/ajstarks/dubois-data-portraits/tree/master/challenge/2022)
was the starting point to the `dubois` package.

The package is under development and has these functions:

-   db_area: creates a vertical area chart with two categories that
    together amount to 100%.

## Installation and loading

Install dubois from [GitHub](https://github.com/IcaroBernardes/dubois)
as follows:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("IcaroBernardes/dubois")
```

## Examples

``` r
library(dubois)

data <- dubois::managers %>%
  dplyr::select(race, year, pct_bosses_total) %>%
  tidyr::pivot_wider(names_from = "race",
                     values_from = "pct_bosses_total")

title <- "PARTICIPATION IN MANAGERIAL POSITIONS BY RACE IN BRAZIL."
subtitle <- "INSPIRED BY: W.E.B. DU BOIS | DATA FROM: IBGE | GRAPHIC BY: ICARO BERNARDES"
message <- "IN THE SERIES, USUALLY WHITES OCCUPY SLIGHTLY LESS GENERAL WORK POSITIONS. HOWEVER WHITES OCCUPY WAY MORE MANAGERIAL POSITIONS THAN BLACKS"

dubois::db_area(data = data, order = "year", cat1 = "black", cat2 = "white",
                limits = c(-3,4), path = "man/figures", filename = "managers.png",
                title = title,
                subtitle = subtitle,
                message = message)
```

<p align="center">
<img src="man/figures/managers.png" alt="logo" height="500">
</p>

## Challenge Posts

-   Icaro BSC
    [Twitter](https://twitter.com/IcaroBSC/status/1501323176088780800)

## Similar packages

-   <a href='https://github.com/vladmedenica/themedubois' target='_blank'>`themedubois`</a>:
    theme_dubois() appplies the Du Bois style to ggplot graphics. It
    customizes font, colors, grids, legend and so on. While
    scale_xxxx_duboisx() applies the Du Bois palette to the fill/color
    of the plot;
-   <a href='https://github.com/18kimn/ggdubois' target='_blank'>`ggdubois`</a>:
    provides theme, pallete and scale in the Du Bois style. Also adds
    some geom_xxxx based on some of his posters. Examples are
    geom_pathspiral and geom_wovenbar.
