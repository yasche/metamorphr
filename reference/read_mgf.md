# Read a MGF file into a tidy tibble

[MGF files](https://www.matrixscience.com/help/data_file_help.html)
allow the storage of MS/MS spectra. This function reads them into a tidy
tibble. Each variable is stored in a column and each ion (observation)
is stored in a separate row. MS/MS spectra are stored in a list column
named MSn.  
Please note that [MGF files are
software-specific](https://fiehnlab.ucdavis.edu/projects/lipidblast/mgf-files)
so the variables and their names may vary. This function was developed
with the GNPS file format exported from
[mzmine](https://mzio.io/mzmine-news/) in mind.  
  
If you encounter any bugs please report them:
<https://github.com/yasche/metamorphr/issues>

## Usage

``` r
read_mgf(file, show_progress = TRUE)
```

## Arguments

- file:

  The path to the MGF file.

- show_progress:

  A `logical` indicating whether the progress of the import should be
  printed to the console. Only important for large MGF files.

## Value

A tidy tibble holding MS/MS spectra.

## Examples

``` r
mgf_path <- system.file("extdata", "toy_mgf.mgf", package = "metamorphr")
read_mgf(mgf_path)
#> # A tibble: 3 × 5
#>   VARIABLEONE VARIABLETWO VARIABLETHREE PEPMASS MSn             
#>         <dbl>       <dbl>         <dbl>   <dbl> <list>          
#> 1         1.1         1.2           1.3    45.7 <tibble [3 × 2]>
#> 2         2.1         2.2           2.3   679.  <tibble [5 × 2]>
#> 3         3.1         3.2           3.3  5890.  <tibble [6 × 2]>
```
