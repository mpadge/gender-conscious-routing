<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R build
status](https://github.com/mpadge/gender-conscious-routing/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/gender-conscious-routing/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/mpadge/gender-conscious-routing/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/gender-conscious-routing)
[![Project Status:
Concept](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
<!-- badges: end -->

# gender conscious routing

Routing along streets named after women rather than men.

*Disclaimer*: This package presumes that gender is binary. Which it is
not. The package and its developers neither endorse nor encourage
viewing gender in binary terms. This binary distinction is nevertheless
hard-coded here because the package relies on a library coded elsewhere
to associate names with genders, and which provides only binary
definitions of gender. The library nevertheless allows gender
associations in a very wide variety of languages, far more than any
other equivalent library, and so allows the functionality of this
package to be applied to far greater regions of the world that would
other, non-binary alternatives.

## Gender catogorizer

This **R** package includes an internally bundled C library for
categorising the gender of first names, thanks to Michael Jörg
(available [here](https://www.heise.de/ct/ftp/07/17/182/)). The library
is extremely fast and flexible; covers all European languages and a host
of others, and categorises gender very accurately. Here’s a test run on
a very large data set of names from the English-speaking world:

``` r
u <- "https://github.com/hadley/data-baby-names/raw/master/baby-names.csv"
if (!file.exists ("baby-names.csv"))
    chk <- download.file (u, "baby-names.csv")
n <- read.csv ("baby-names.csv", stringsAsFactors = FALSE)
format (nrow (n), big.mark = ",")
#> [1] "258,000"
st <- system.time (x <- get_gender (n$name))
st
#>    user  system elapsed 
#>   1.389   1.839   3.234
knitr::kable (table (x$gender))
```

| Var1             |   Freq |
|:-----------------|-------:|
| IS_FEMALE        | 103059 |
| IS_MALE          |  95751 |
| IS_MOSTLY_FEMALE |  15919 |
| IS_MOSTLY_MALE   |  17290 |
| IS_UNISEX_NAME   |  11296 |
| NAME_NOT_FOUND   |  14685 |

``` r
knitr::kable (table (n$sex))
```

| Var1 |   Freq |
|:-----|-------:|
| boy  | 129000 |
| girl | 129000 |

Categorising 258,000 names took only 3.234 seconds, or around 100,000
names per second. The following code compares the accuracy, noting that
many names are of course unisex, whereas the “baby-names” data are
direct records of individual names and sex.

``` r
x$gender [x$gender == "IS_MALE"] <- "boy"
x$gender [x$gender == "IS_MOSTLY_MALE"] <- "boy"
x$gender [x$gender == "IS_FEMALE"] <- "girl"
x$gender [x$gender == "IS_MOSTLY_FEMALE"] <- "girl"

index_right <- which (x$gender == n$sex)
message (format (length (index_right), big.mark = ","), " / ",
         format (nrow (x), big.mark = ","),
         " of names correctly classified = ",
         formatC (100 * length (index_right) / nrow (x),
                  format = "f", digits = 1), "%")
#> 217,630 / 258,000 of names correctly classified = 84.4%
```

Noting that the baby name records are structured over time, and include
many repeats of the same names, we can try to create “mostly girl/boy”
categories based on relative proportions.

``` r
library (dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

categorise_sex <-  function (sex, size) {
    # define relative proportions:
    # if > rel_props [2], then category is singular
    # else if > rel_props [1], then category is "mostly" singular,
    # else category is unisex
    rel_props <- c (4, 1000)

    if (length (size) == 1)
      return (sex)

    bi <- which (sex == "boy")
    gi <- which (sex == "girl")

    if (size [bi] > (size [gi] * rel_props [2]))
        return ("boy")
    else if (size [gi] > (size [bi] * rel_props [2]))
        return ("girl")
    else if (size [bi] > (size [gi] * rel_props [1]))
        return ("mostly boy")
    else if (size [gi] > (size [bi] * rel_props [1]))
        return ("mostly girl")
    else
        return ("unisex")
    }
n2 <- n |>
    group_by (name, sex) |>
    summarise (size = n ()) |>
    group_by (name) |>
    summarise (category = categorise_sex (sex, size))
#> `summarise()` has grouped output by 'name'. You can override using the `.groups` argument.
knitr::kable (table (n2$category))
```

| Var1        | Freq |
|:------------|-----:|
| boy         | 2764 |
| girl        | 3345 |
| mostly boy  |  147 |
| mostly girl |  224 |
| unisex      |  302 |

The above values for relative proportions were selected to give good
agreement with the observed overall distribution of categories as
determined by the internal library. These two more refined data sets can
then be compared:

``` r
n2$gender <- get_gender (n2$name)$gender
n2$gender [n2$gender == "IS_FEMALE"] <- "girl"
n2$gender [n2$gender == "IS_MALE"] <- "boy"
n2$gender [n2$gender == "IS_MOSTLY_FEMALE"] <- "mostly girl"
n2$gender [n2$gender == "IS_MOSTLY_MALE"] <- "mostly boy"
n2$gender [n2$gender == "IS_UNISEX_NAME"] <- "unisex"
```

Some names are simply not found, so we’ll remove those from the
comparison before calculating final statistics.

``` r
n2 <- n2 [which (!n2$gender == "NAME_NOT_FOUND"), ]
knitr::kable (with (n2, table (category, gender)))
```

|             |  boy | girl | mostly boy | mostly girl | unisex |
|:------------|-----:|-----:|-----------:|------------:|-------:|
| boy         | 1643 |   19 |         92 |          15 |     61 |
| girl        |   19 | 2221 |         15 |          89 |     66 |
| mostly boy  |   90 |    3 |         36 |           4 |      8 |
| mostly girl |    0 |  173 |          3 |          30 |      7 |
| unisex      |   27 |   44 |         65 |          79 |     66 |

The accuracy in that case is

``` r
ct <- with (n2, table (category, gender))
sum (diag (ct)) / sum (ct)
#> [1] 0.8196923
```
