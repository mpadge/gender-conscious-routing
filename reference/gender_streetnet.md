# gender_streetnet

Weight a street network via dodgr function `weight_streetnet`, adding
additional information on gender of street names

## Usage

``` r
gender_streetnet(net, wt_profile = "foot", country = "de")
```

## Arguments

- net:

  Street network in sf or sc format

- wt_profile:

  Type of weighting to be used; see
  [`dodgr::weight_streetnet`](https://UrbanAnalyst.github.io/dodgr/reference/weight_streetnet.html)
  for details.

- country:

  Country for which gender is to be determined, either as ISO3166
  two-letter abbreviation, or full text. See
  [list_countries](https://mpadge.github.io/gender-conscious-routing/reference/list_countries.md)
  for list of recognized countries.

## Value

A weighted street network in dodgr format.
