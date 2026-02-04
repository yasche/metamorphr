# Calculate the Kendrick mass

Calculate the Kendrick mass for a given mass (or m/z) and repeating
unit. The Kendrick mass is a rescaled mass, that usually sets CH2 = 14
but other repeating units can also be used. It is usefull for the visual
identification of potential homologues. See the References section for
more information. The Kendrick mass is not to be confused with the
Kendrick mass defect (KMD,
[`calc_kmd`](https://yasche.github.io/metamorphr/reference/calc_kmd.md))
and the nominal Kendrick mass
([`calc_nominal_km`](https://yasche.github.io/metamorphr/reference/calc_nominal_km.md)).

## Usage

``` r
calc_km(mass, repeating_unit = "CH2")
```

## Arguments

- mass:

  A molecular mass (or m/z).

- repeating_unit:

  The formula of the repeating unit, given as a string.

## Value

The Kendrick mass.

## References

- [Kendrick mass on
  Wikipedia](https://en.wikipedia.org/wiki/Kendrick_mass)

- Edward Kendrick, *Anal. Chem.* **1963**, *35*, 2146–2154.

- C. A. Hughey, C. L. Hendrickson, R. P. Rodgers, A. G. Marshall, K.
  Qian, *Anal. Chem.* **2001**, *73*, 4676–4681.

## Examples

``` r
# Calculate the Kendrick masses for two measured masses with
# CH2 as the repeating unit.
# See Hughey et al. in the References section above

calc_km(c(351.3269, 365.3425))
#> [1] 350.9346 364.9346

# Construct a KMD plot from m/z values.
# RT is mapped to color and the feature-wise maximum intensity to size.
# Note that in the publication by Hughey et al., the nominal Kendrick mass
# is used on the x-axis instead of the exact Kendrick mass.
# See ?calc_nominal_km.

toy_metaboscape %>%
  dplyr::group_by(UID, `m/z`, RT) %>%
  dplyr::summarise(max_int = max(Intensity, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(KMD = calc_kmd(`m/z`),
                KM = calc_km(`m/z`)) %>%
  ggplot2::ggplot(ggplot2::aes(x = KM,
                               y = KMD,
                               size = max_int,
                               color = RT)) +
    ggplot2::geom_point()
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by UID, m/z, and RT.
#> ℹ Output is grouped by UID and m/z.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(UID, m/z, RT))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
```
