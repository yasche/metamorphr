# Calculate neutral losses from precursor ion mass and fragment ion masses

Calculate neutral loss spectra for all ions with available MSn spectra
in `data`. To calculate neutral losses, MSn spectra are required. See
[`read_mgf`](https://yasche.github.io/metamorphr/reference/read_mgf.md).
This step is required for subsequent filtering based on neutral losses
([`filter_neutral_loss`](https://yasche.github.io/metamorphr/reference/filter_neutral_loss.md)).
Resulting neutral loss spectra are stored in tibbles in a new list
column named `Neutral_Loss`.

## Usage

``` r
msn_calc_nl(data, m_z_col)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- m_z_col:

  Which column holds the precursor m/z? Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

## Value

A tibble with added neutral loss spectra. A new list column is created
named `Neutral_Loss`.

## Examples

``` r
toy_mgf %>%
  msn_calc_nl(m_z_col = PEPMASS)
#> # A tibble: 3 × 6
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn              Neutral_Loss
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>           <list>      
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]> <tibble>    
#> 2         2.1         2.2           2.3   679.  <tibble [5 × 2]> <tibble>    
#> 3         3.1         3.2           3.3  5890.  <tibble [6 × 2]> <tibble>    
```
