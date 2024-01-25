library(tidyverse)
library(readxl)
library(HHSKwkl)

doelen <-
  readxl::read_excel("data/waterlichamen_ekrs_en_doelen_2022.xlsx") %>% 
  select(type, naam = OWMNAAM, doelen, groep)


ekrs <- 
  read_excel("data/overzicht ekr nieuwe toetsing 2023 v24-7-2023.xlsx") %>% 
  pivot_longer(cols = starts_with("20"), names_to = "jaar", values_to = "ekr", values_drop_na = TRUE) %>% 
  rename_all(str_to_lower) %>%
  mutate(jaar = as.numeric(jaar)) %>% 
  arrange(type, nr, naam, jaar) %>% 
  group_by(type, nr, naam) %>% 
  mutate(ekr3 = slider::slide_dbl(ekr, ~mean(.x), .before = 2)) %>% 
  ungroup() %>% 
  left_join(doelen) %>% 
  filter(jaar == max(jaar), .by = c(naam, type)) %>% 
  select(type, nr, naam, watertype, jaar, ekr = ekr3, doelen, groep)

ekrs %>% 
  openxlsx::write.xlsx("data/waterlichamen_ekrs_en_doelen_2023.xlsx")
