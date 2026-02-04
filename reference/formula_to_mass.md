# Calculate the monoisotopic mass from a given formula

Calculates the monoisotopic mass from a given formula. If only the
element symbols are provided, the calculated mass corresponds to that of
a molecule made up from the most abundant isotopes. Other isotopes can
also be provided (e.g., ¹³C, instead of the naturally most abundant
¹²C). See the samples for details.

## Usage

``` r
formula_to_mass(formula)
```

## Arguments

- formula:

  A formula as a string.

## Value

The monoisotopic mass of the formula.

## Examples

``` r
# The monoisotopic mass is calculated with the most abundant isotopes
# if only the element symbols are provided:
formula_to_mass("CH4")
#> [1] 16.0313
formula_to_mass("NH3")
#> [1] 17.02655
formula_to_mass("C10H17N3O6S")
#> [1] 307.0838

# Other isotopes can be provided as follows:
formula_to_mass("[13C]H4")
#> [1] 17.03465
formula_to_mass("[15N]H3")
#> [1] 18.02358

# Every isotope, including the most abundant ones, can be named explicitly.
# Compare:
formula_to_mass("[14N][1H]3")
#> [1] 17.02655
formula_to_mass("NH3")
#> [1] 17.02655

# The function also supports brackets and nested brackets:
formula_to_mass("(CH3)2")
#> [1] 30.04695
formula_to_mass("(((CH3)2N)3C)2")
#> [1] 288.3001
formula_to_mass("((([13C]H3)2N)3C)2")
#> [1] 300.3404
```
