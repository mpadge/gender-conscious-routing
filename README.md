<!-- README.md is generated from README.Rmd. Please edit that file -->

# gender conscious routing

Routing along streets named after women rather than men.

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mpadge/gender-conscious-routing.svg?branch=master)](https://travis-ci.org/mpadge/gender-conscious-routing)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mpadge/gender-conscious-routing?branch=master&svg=true)](https://ci.appveyor.com/project/mpadge/gender-conscious-routing)
[![codecov](https://codecov.io/gh/mpadge/gender-conscious-routing/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/gender-conscious-routing)
[![Project Status:
Concept](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
<!-- badges: end -->

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
#>   1.155   1.463   2.619
knitr::kable (table (x$category))
```

| Var1               |   Freq |
| :----------------- | -----: |
| IS\_FEMALE         | 103059 |
| IS\_MALE           |  95751 |
| IS\_MOSTLY\_FEMALE |  15919 |
| IS\_MOSTLY\_MALE   |  17290 |
| IS\_UNISEX\_NAME   |  11296 |
| NAME\_NOT\_FOUND   |  14685 |

``` r
knitr::kable (table (n$sex))
```

| Var1 |   Freq |
| :--- | -----: |
| boy  | 129000 |
| girl | 129000 |

Categorising 258,000 names took only 2.619 seconds, or around 100,000
names per second. The following code compares the accuracy, noting that
many names are of course unisex, whereas the “baby-names” data are
direct records of individual names and sex.

``` r
x$category [x$category == "IS_MALE"] <- "boy"
x$category [x$category == "IS_MOSTLY_MALE"] <- "boy"
x$category [x$category == "IS_FEMALE"] <- "girl"
x$category [x$category == "IS_MOSTLY_FEMALE"] <- "girl"

index_right <- which (x$category == n$sex)
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
n2 <- n %>%
    group_by (name, sex) %>%
    summarise (size = n ()) %>%
    group_by (name) %>%
    summarise (category = categorise_sex (sex, size))
knitr::kable (table (n2$category))
```

| Var1        | Freq |
| :---------- | ---: |
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
n2$gender <- get_gender (n2$name)$category
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

|                |    boy |    girl | mostly boy | mostly girl | unisex |
| -------------- | -----: | ------: | ---------: | ----------: | -----: |
| boy            |   1643 |      19 |         92 |          15 |     61 |
| girl           |     19 |    2221 |         15 |          89 |     66 |
| mostly boy     |     90 |       3 |         36 |           4 |      8 |
| mostly girl    |      0 |     173 |          3 |          30 |      7 |
| unisex         |     27 |      44 |         65 |          79 |     66 |
| The accuracy i | n that | case is |            |             |        |

``` r
ct <- with (n2, table (category, gender))
sum (diag (ct)) / sum (ct)
#> [1] 0.8196923
```
