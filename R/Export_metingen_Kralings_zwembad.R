library(HHSKwkl)
library(tidyverse)
library(glue)

fys_chem <- readRDS("P:/Dawaco/FME/fys_chem.rds")
meetpunten <- import_meetpunten("P:/Dawaco/FME/meetpunten.csv")
parameters <- import_parameters("P:/Dawaco/FME/parameters.csv")
pfas_rpf <- readxl::read_excel("rpf_pfas.xlsx")
theme_set(hhskthema())  

pfas <- 
  fys_chem %>% 
  add_jaar() %>% 
  filter(jaar == 2023) %>% 
  inner_join(filter(parameters, cluster == "PFAS")) %>% 
  mutate(par = str_replace(par, "FRD-903", "GenX"))


pfas %>% 
  filter(mp == "S_1124") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  arrange(mp, datum, par) %>% 
  group_by(mp, datum) %>% 
  summarise(factor_risicogrens = sum(fac, na.rm = TRUE), pfoa_eq =sum(pfoa_equivalent, na.rm = TRUE))
  

pfas %>% 
  filter(str_detect(mp, "ADHOC_23_KE|S_1124|S_1133|S_1149")) %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  arrange(mp, datum, par) %>% 
  select(meetpunt = mp,
         datum,
         parametercode = aquo_parcode,
         parameternaam = parnaamlang,
         detectiegrens,
         meetwaarde = waarde,
         eenheid,
         `relatieve potentiefactor tov pfoa (rpf)` = rpf,
         `pfoa_equivalent_in_ng/l` = pfoa_equivalent,
         `percentage van toelaatbare dagelijkse inname (% TDI - scenario_kind_worst_case_realistisch)` = fac
         
         ) %>% 
  
  openxlsx::write.xlsx(glue("data_export/{Sys.Date()}_pfas_metingen_kralings_zwembad_2023.xlsx"))
  


pfas %>% 
  filter(mp == "S_1124") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  filter(!is.na(pfoa_equivalent)) %>% 
  filter(is.na(detectiegrens)) %>% 
  mutate(parnaamlang = fct_reorder(parnaamlang, pfoa_equivalent)) %>% 
  ggplot(aes(fac, parnaamlang)) + 
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(), position = "top",
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       x = "Percentage van de tolereerbare dagelijkse inname",
       y = "") +
  facet_wrap(~datum)
 



pfas %>% 
  filter(mp == "S_1124") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  filter(!is.na(pfoa_equivalent)) %>% 
  filter(is.na(detectiegrens)) %>% 
  mutate(parnaamlang = fct_reorder(parnaamlang, pfoa_equivalent)) %>% 
  ggplot(aes(as.character(datum), fac)) + 
  geom_col(aes(fill = parnaamlang), colour = "grey") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(),
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       y = "Percentage van de tolereerbare dagelijkse inname",
       x = "") +
  scale_fill_viridis_d()



pfas %>% 
  filter(mp == "S_1124") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  mutate(parnaamlang = fct_reorder(parnaamlang, pfoa_equivalent)) %>% 
  # arrange(mp, datum, par) %>% 
  # group_by(mp, datum) %>% 
  # summarise(factor_risicogrens = sum(fac, na.rm = TRUE), pfoa_eq =sum(pfoa_equivalent, na.rm = TRUE)) %>% 
  ggplot(aes(as.character(datum), fac)) + 
  geom_col(aes(fill = parnaamlang)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(),
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       y = "Percentage van de tolereerbare dagelijkse inname",
       x = "")


pfas %>% 
  filter(mp == "S_1120") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  filter(!is.na(pfoa_equivalent)) %>% 
  filter(is.na(detectiegrens)) %>% 
  mutate(parnaamlang = fct_reorder(parnaamlang, pfoa_equivalent)) %>% 
  ggplot(aes(fac, parnaamlang)) + 
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(), position = "top",
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       x = "Percentage van de tolereerbare dagelijkse inname",
       y = "") +
  facet_wrap(~datum)


pfas %>% 
  filter(mp %in% c("S_1120", "S_0030", "S_0601")) %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  filter(!is.na(pfoa_equivalent)) %>% 
  filter(is.na(detectiegrens)) %>% 
  mutate(parnaamlang = fct_reorder(parnaamlang, pfoa_equivalent)) %>% 
  mutate(mp = fct_reorder(mp, waarde)) %>% 
  ggplot(aes(pfoa_equivalent, parnaamlang)) + 
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
  labs(title = "Risico van PFAS voor zwemmen",
       x = "",
       y = "") +
  facet_wrap(~mp, ncol = 3)


pfas %>% 
  filter(mp == "S_1120") %>% 
  left_join(pfas_rpf) %>% 
  filter(is.na(detectiegrens)) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  summarise(pfoa_tot = sum(pfoa_equivalent, na.rm = TRUE), pfoa_tot / 849)


pfas %>% 
  filter(mp == "S_1120") %>% 
  left_join(pfas_rpf) %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  ungroup() %>% 
  mutate(fac = pfoa_equivalent / 849) %>% 
  arrange(mp, datum, par) %>% 
  select(meetpunt = mp,
         datum,
         parametercode = aquo_parcode,
         parameternaam = parnaamlang,
         detectiegrens,
         meetwaarde = waarde,
         eenheid,
         `relatieve potentiefactor tov pfoa (rpf)` = rpf,
         `pfoa_equivalent_in_ng/l` = pfoa_equivalent,
         `percentage van toelaatbare dagelijkse inname (% TDI - scenario_kind_worst_case_realistisch)` = fac
         
  ) %>% 
  
  openxlsx::write.xlsx(glue("data_export/{Sys.Date()}_pfas_metingen_zwarte_plasje_2023.xlsx"))
