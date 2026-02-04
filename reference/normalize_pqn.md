# Normalize intensities across samples using a Probabilistic Quotient Normalization (PQN)

This method was originally developed for H-NMR spectra of complex
biofluids but has been adapted for other 'omics data. It aims to
eliminate dilution effects by calculating the most probable dilution
factor for each sample, relative to one or more reference samples. See
references for more details.

## Usage

``` r
normalize_pqn(
  data,
  fn = "median",
  normalize_sum = TRUE,
  reference_samples = NULL,
  ref_as_group = FALSE,
  group_column = NULL
)
```

## Arguments

- data:

  A tidy tibble created by
  [`read_featuretable`](https://yasche.github.io/metamorphr/reference/read_featuretable.md).

- fn:

  Which function should be used to calculate the reference spectrum from
  the reference samples? Can be either "mean" or "median".

- normalize_sum:

  A logical indicating whether a sum normalization (aka total area
  normalization) should be performed prior to PQN. It is
  [recommended](https://rdrr.io/github/ricoderks/Rcpm/man/pqn.html) to
  do so and other packages (e.g.,
  [KODAMA](https://CRAN.R-project.org/package=KODAMA)) also perform a
  sum normalization prior to PQN.

- reference_samples:

  Either `NULL` or a character or character vector containing the
  sample(s) to calculate the reference spectrum from. In the original
  publication, it is advised to calculate the median of control samples.
  If `NULL`, all samples will be used to calculate the reference
  spectrum.

- ref_as_group:

  A logical indicating if `reference_samples` are the names of samples
  or group(s).

- group_column:

  Only relevant if `ref_as_group = TRUE`. Which column should be used
  for grouping reference and non-reference samples? Usually
  `group_column = Group`. Uses
  [`args_data_masking`](https://rlang.r-lib.org/reference/args_data_masking.html).

## Value

A tibble with intensities normalized across samples.

## References

- F. Dieterle, A. Ross, G. Schlotterbeck, H. Senn, *Anal. Chem.*
  **2006**, *78*, 4281–4290, DOI 10.1021/ac051632c.

## Examples

``` r
# specify the reference samples with their sample names
toy_metaboscape %>%
  impute_lod() %>%
  normalize_pqn(reference_samples = c("QC1", "QC2", "QC3"))
#> # A tibble: 110 × 8
#>      UID Feature                Sample  Intensity    RT `m/z` Name       Formula
#>    <int> <chr>                  <chr>       <dbl> <dbl> <dbl> <chr>      <chr>  
#>  1     1 161.10519 Da 26.98 s   Sample1   0.115    0.45  162. NA         C7H15N…
#>  2     2 276.13647 Da 27.28 s   Sample1   0.0862   0.45  277. Octyl hyd… C16H22…
#>  3     3 304.24023 Da 32.86 s   Sample1   0.00575  0.55  305. Arachidon… C20H32…
#>  4     4 417.23236 Da 60.08 s   Sample1   0.144    1     418. NA         NA     
#>  5     5 104.10753 Da 170.31 s  Sample1   0.144    2.84  105. NA         C5H14NO
#>  6     6 105.04259 Da 199.80 s  Sample1   0.144    3.33  106. NA         C3H8NO3
#>  7     7 237.09204 Da 313.24 s  Sample1   0.0402   5.22  238. Ketamine   C13H16…
#>  8     8 745.09111 Da 382.23 s  Sample1   0.0862   6.37  746. NADPH      C21H30…
#>  9     9 427.02942 Da 424.84 s  Sample1   0.115    7.08  428. ADP        C10H15…
#> 10    10 1284.34904 Da 498.94 s Sample1   0.0115   8.32 1285. NA         NA     
#> # ℹ 100 more rows

# specify the reference samples with their group names
toy_metaboscape %>%
  join_metadata(toy_metaboscape_metadata) %>%
  impute_lod() %>%
  normalize_pqn(reference_samples = c("QC"), ref_as_group = TRUE, group_column = Group)
#> # A tibble: 110 × 12
#>      UID Feature      Sample Intensity    RT `m/z` Name  Formula Group Replicate
#>    <int> <chr>        <chr>      <dbl> <dbl> <dbl> <chr> <chr>   <chr>     <int>
#>  1     1 161.10519 D… Sampl…   0.115    0.45  162. NA    C7H15N… cont…         1
#>  2     2 276.13647 D… Sampl…   0.0862   0.45  277. Octy… C16H22… cont…         1
#>  3     3 304.24023 D… Sampl…   0.00575  0.55  305. Arac… C20H32… cont…         1
#>  4     4 417.23236 D… Sampl…   0.144    1     418. NA    NA      cont…         1
#>  5     5 104.10753 D… Sampl…   0.144    2.84  105. NA    C5H14NO cont…         1
#>  6     6 105.04259 D… Sampl…   0.144    3.33  106. NA    C3H8NO3 cont…         1
#>  7     7 237.09204 D… Sampl…   0.0402   5.22  238. Keta… C13H16… cont…         1
#>  8     8 745.09111 D… Sampl…   0.0862   6.37  746. NADPH C21H30… cont…         1
#>  9     9 427.02942 D… Sampl…   0.115    7.08  428. ADP   C10H15… cont…         1
#> 10    10 1284.34904 … Sampl…   0.0115   8.32 1285. NA    NA      cont…         1
#> # ℹ 100 more rows
#> # ℹ 2 more variables: Batch <int>, Factor <dbl>
```
