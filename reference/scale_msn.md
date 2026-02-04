# Scale intensities in MSn spectra to the highest value within each spectrum

Scale the intensity of each peak in an MSn spectrum to that of the
highest peak. MSn spectra are required to use this function. See
[`read_mgf`](https://yasche.github.io/metamorphr/reference/read_mgf.md).

**Important Note**

Please note that existing MSn spectra in `data` will be overwritten.

## Usage

``` r
scale_msn(data, scale_to = 100)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- scale_to:

  A `numeric` that specifies to which number the highest signal in each
  spectrum will be scaled.

## Value

A tibble with scaled MSn spectra.

## Examples

``` r
toy_mgf %>%
  scale_msn()
#> # A tibble: 3 × 5
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn             
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>          
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]>
#> 2         2.1         2.2           2.3   679.  <tibble [5 × 2]>
#> 3         3.1         3.2           3.3  5890.  <tibble [6 × 2]>
```
