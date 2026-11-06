# conscious_route

Route between nominated start and end points along greatest proportional
length of streets named after women rather than men.

## Usage

``` r
conscious_route(net, start = NULL, stop = NULL, gender_weight = 10)
```

## Arguments

- net:

  Street network with gender column, returned by
  [gender_streetnet](https://mpadge.github.io/gender-conscious-routing/reference/gender_streetnet.md)
  function.

- start:

  Start point of route.

- stop:

  End point of route.

- gender_weight:

  A numeric factor \>= 1 by which to preferentially route along streets
  named after women. Larger values will search larger distances relative
  to the default (shortest) route to find routes with greatest
  proportional distances along streets named after women. Arbitrarily
  large values may be specified.
