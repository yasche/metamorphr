# Filter Features based on occurrence of neutral losses

The occurrence of characteristic neutral losses can help with the
putative annotation of molecules. See the Reference section for an
example.

## Usage

``` r
filter_neutral_loss(
  data,
  losses,
  min_found,
  tolerance = 10,
  tolerance_type = "ppm",
  show_progress = TRUE
)
```

## Arguments

- data:

  A data frame containing MSn spectra.

- losses:

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

## References

- A. Brink, F. Fontaine, M. Marschmann, B. Steinhuber, E. N. Cece, I.
  Zamora, A. Pähler, *Rapid Commun. Mass Spectrom.* **2014**, *28*,
  2695–2703, DOI 10.1002/rcm.7062.

## Examples

``` r
# neutral losses must be calculated first
toy_mgf_nl <- toy_mgf %>%
  calc_neutral_loss(m_z_col = PEPMASS)

# all of the given losses (3) must be found
# returns the first row of toy_mgf
toy_mgf_nl %>%
  filter_neutral_loss(losses = c(11.1111, 22.2222, 33.3333), min_found = 3)
#> # A tibble: 1 × 6
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn              Neutral_Loss
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>           <list>      
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]> <tibble>    

# all of the given fragments (3) must be found
# returns an empty tibble because the third loss
# of row 1 (33.3333)
# is outside of the tolerance (10 ppm):
# Lower bound:
# 33.4333 - 33.4333 * 5 / 1000000 = 33.4333
# Upper bound:
# 33.4333 + 33.4333 * 5 / 1000000 = 33.4336
toy_mgf_nl %>%
  filter_neutral_loss(losses = c(11.1111, 22.2222, 33.4333), min_found = 3)
#> # A tibble: 0 × 6
#> # ℹ 6 variables: VARIABLEONE <dbl>, VARIABLETWO <dbl>, VARIABLETHREE <dbl>,
#> #   PEPMASS <dbl>, MSn <list>, Neutral_Loss <list>

# only 2 of the 3 fragments must be found
# returns the first row of toy_mgf
toy_mgf_nl %>%
  filter_neutral_loss(losses = c(11.1111, 22.2222, 33.4333), min_found = 2)
#> # A tibble: 1 × 6
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn              Neutral_Loss
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>           <list>      
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]> <tibble>    
```
