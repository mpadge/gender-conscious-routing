# get_gender

Get the gender of a character vector

## Usage

``` r
get_gender(text, country = NULL)
```

## Arguments

- text:

  The character vector to encode

- country:

  Country for which gender is to be determined, either as ISO3166
  two-letter abbreviation, or full text. See
  [list_countries](https://mpadge.github.io/gender-conscious-routing/reference/list_countries.md)
  for list of recognized countries.

## Value

`data.frame` of text with gender attached
