# A tibble containing the NIST standard atomic weights

The data set contains the atomic weights of the elements and their
isotopes. It is used to calculate the exact mass in
[`formula_to_mass`](https://yasche.github.io/metamorphr/reference/formula_to_mass.md)
but can also be used as a reference.

description

## Usage

``` r
atoms
```

## Format

### `atoms`

A data frame with 442 rows and 7 columns:

- Number:

  The atomic number of the element in the periodic table.

- Element:

  The element.

- Isotope:

  The mass number of the specific isotope.

- Symbol:

  The atomic symbol. Either only the letter (for standard isotopes) or
  the mass number followed by the symbol (for special isotopes).

- Weight:

  The monoisotopic mass of the isotope.

- Composition:

  The fraction of the isotope in the naturally occuring element.

- Standard_Weight:

  The standard atomic weight of the element. It is the sum of the
  product of the `Weight` and `Composition` column for each element.
  Where no composition is available, the weight of the IUPAC "ATOMIC
  WEIGHTS OF THE ELEMENTS 2023" table was used. See the Source section
  for more information.

## Source

The table was retrieved from the National Institute of Standards and
Technology (NIST) at
https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl, accesed in
October 2025, and enriched with data from the IUPAC "ATOMIC WEIGHTS OF
THE ELEMENTS 2023" table at https://iupac.qmul.ac.uk/AtWt/, accesed in
October 2025
