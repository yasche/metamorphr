## code to prepare `atoms` dataset goes here
library(rvest)
library(tidyverse)

atoms <- read_html("https://physics.nist.gov/cgi-bin/Compositions/stand_alone.pl") %>%
  html_table()

atoms <- atoms[[1]]

colnames(atoms) <- c("Number",
                     "Element",
                     "Isotope",
                     "x",
                     "Weight",
                     "Y",
                     "Composition",
                     "Standard_Weight",
                     "Notes")

atoms <- atoms %>%
  select(-c(4, 6)) %>%
  filter(!is.na(Number)) %>%
  # rbind(mutate(.[2:3,], Element = "H")) %>%
  arrange(Number, Isotope, Element) %>%
  mutate(Composition = str_remove_all(Composition, "\\([0-9]{0,}\\)")) %>%
  mutate(Composition = str_remove_all(Composition, "[^ -~]+")) %>%
  mutate(Composition = case_when(Composition == "" ~ "0",
                                 .default = Composition)) %>%
  mutate(Composition = as.numeric(Composition)) %>%
  mutate(Weight = str_remove_all(Weight, "\\(.{0,}\\)")) %>%
  mutate(Weight = str_remove_all(Weight, "[^ -~]+")) %>%
  mutate(Weight = as.numeric(Weight)) %>%
  # calculate Standard_weight using Composition and Weight
  group_by(Number) %>%
  mutate(Standard_Weight = sum(Weight * Composition)) %>%
  ungroup() %>%
  rbind(mutate(.[2:3,], Element = "H"))

# Some elements do not have a composition assigned to them (from NIST), so their Standard_Weight is 0.
# They are added below.
# Weights are from https://iupac.qmul.ac.uk/AtWt/

missing_weights_elements <- atoms %>%
  filter(Standard_Weight == 0) %>%
  pull(Element) %>%
  unique() #%>%
  #tibble(Element = .)

missing_weights <- read_html("https://iupac.qmul.ac.uk/AtWt/") %>%
  html_table()

missing_weights_tbl <- missing_weights[[2]] %>%
  filter(X2 %in% missing_weights_elements) %>%
  select(X2, X4)

colnames(missing_weights_tbl) <- c("Element", "Standard_Weight2")

missing_weights_tbl <- missing_weights_tbl %>%
  mutate(Standard_Weight2 = as.numeric(str_remove_all(Standard_Weight2, "\\[|\\]")))


atoms <- left_join(atoms, missing_weights_tbl) %>%
  mutate(Standard_Weight = case_when(Standard_Weight == 0 ~ Standard_Weight2,
                                     .default = Standard_Weight)) %>%
  select(-Standard_Weight2)


explicit_atoms <- atoms %>%
  mutate(Symbol = paste0(Isotope, Element)) %>%
  rbind(filter(., Symbol == "2D" | Symbol == "3T") %>%
          mutate(Symbol = str_remove_all(Symbol, "[[:digit:]]"))) %>%
  arrange(Number, Isotope, Element)


standard_atoms <- atoms %>%
  group_by(Number) %>%
  filter(Composition == max(Composition)) %>%
  ungroup() %>%
  filter(Composition != 0) %>%
  mutate(Symbol = Element)

atoms <- rbind(standard_atoms, explicit_atoms)%>%
  select(-Notes) %>%
  relocate(Symbol, .after = Isotope)


usethis::use_data(atoms, overwrite = TRUE)
