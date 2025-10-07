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
  rbind(mutate(.[2:3,], Element = "H")) %>%
  arrange(Number, Isotope, Element) %>%
  mutate(Composition = str_remove_all(Composition, "\\([0-9]{0,}\\)")) %>%
  mutate(Composition = str_remove_all(Composition, "[^ -~]+")) %>%
  mutate(Composition = case_when(Composition == "" ~ "0",
                                 .default = Composition)) %>%
  mutate(Composition = as.numeric(Composition)) %>%
  mutate(Weight = str_remove_all(Weight, "\\(.{0,}\\)")) %>%
  mutate(Weight = str_remove_all(Weight, "[^ -~]+")) %>%
  mutate(Weight = as.numeric(Weight))

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
