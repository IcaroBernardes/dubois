<!-- README.md is generated from README.Rmd. Please edit that file -->

# dubois <img align="right" src="https://github.com/IcaroBernardes/dubois/blob/master/man/figures/dubois.png" alt="logo" width="140">

<div>

The R package, `dubois`, aids users to render Du Bois styled plots from
custom datasets.

</div>

<div>

W.E.B Du Bois (February 23, 1868 – August 27, 1963) was an American
sociologist, socialist, historian and Pan-Africanist civil rights
activist. Born in Great Barrington, Massachusetts, Du Bois grew up in a
relatively tolerant and integrated community, and after completing
graduate work at the University of Berlin and Harvard, where he was the
first African American to earn a doctorate, he became a professor of
history, sociology and economics at Atlanta University. Du Bois was one
of the founders of the National Association for the Advancement of
Colored People (NAACP) in 1909 (*Source: Wikpedia*).

The development inspiration came from the #DuBoisChallenge2022 and the
dissemination of SER (International Seminar on Statistics with R).

</div>

## Installation and loading

-   Install dubois from
    [GitHub](https://github.com/IcaroBernardes/dubois) as follows:

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

## Blog posts

-   Icaro BSC
    [Twitter](https://twitter.com/IcaroBSC/status/1501323176088780800)
