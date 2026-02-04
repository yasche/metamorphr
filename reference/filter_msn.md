# Filter Features based on occurrence of fragment ions

Filters Features based on the presence of MSn fragments. This can help,
for example with the identification of potential homologous molecules.

## Usage

``` r
filter_msn(
  data,
  fragments,
  min_found,
  tolerance = 5,
  tolerance_type = "ppm",
  show_progress = TRUE
)
```

## Arguments

- data:

  A data frame containing MSn spectra.

- fragments:

  A numeric. Exact mass of the fragment(s) to filter by.

- min_found:

  How many of the `fragments` must be found in order to keep the row? If
  `min_found = length(fragments)`, all fragments must be found.

- tolerance:

  A numeric. The tolerance to apply to the fragments. Either an absolute
  value in Da (if `tolerance_type = "absolute"`) or in ppm (if
  `tolerance_type = "ppm"`).

- tolerance_type:

  Either `"absolute"` or `"ppm"`. Should the tolerance be an absolute
  value or in ppm?

- show_progress:

  A `logical` indicating whether the progress of the filtering should be
  printed to the console. Only important for large tibbles.

## Value

A filtered tibble.

## Examples

``` r
# all of the given fragments (3) must be found
# returns the first row of toy_mgf
toy_mgf %>%
  filter_msn(fragments = c(12.3456, 23.4567, 34.5678), min_found = 3)
#> # A tibble: 1 × 5
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn             
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>          
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]>

# all of the given fragments (3) must be found
# returns an empty tibble because the third fragment
# of row 1 (34.5678)
# is outside of the tolerance (5 ppm):
# Lower bound:
# 34.5688 - 34.5688 * 5 / 1000000 = 34.5686
# Upper bound:
# 34.5688 + 34.5688 * 5 / 1000000 = 34.5690
toy_mgf %>%
  filter_msn(fragments = c(12.3456, 23.4567, 34.5688), min_found = 3)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: VARIABLEONE <dbl>, VARIABLETWO <dbl>, VARIABLETHREE <dbl>,
#> #   PEPMASS <dbl>, MSn <list>

# only 2 of the 3 fragments must be found
# returns the first row of toy_mgf
toy_mgf %>%
  filter_msn(fragments = c(12.3456, 23.4567, 34.5688), min_found = 2)
#> # A tibble: 1 × 5
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn             
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>          
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]>
```
