# gcr_city

Analyse entire properties of one city

## Usage

``` r
gcr_city(net, wt_profile = "foot", n = NULL, quiet = FALSE)
```

## Arguments

- net:

  Street network in `osmdata_sc` format.

- wt_profile:

  Type of weighting profile for street network routing,.

- n:

  Number of pints used to calculate pair-wise routes along which to
  aggregate statistics. If no value specified, routes are calculated
  between all pairs for the entire network. If value exceeds total
  number of network nodes, that number is used instead.

- quiet:

  if `FALSE`, display progress information on screen.
