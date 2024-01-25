library(tidyverse)
# library(lubridate)
# devtools::install_github("RedTent/HHSKwkl") # Zie ook https://github.com/RedTent/HHSKwkl
library(HHSKwkl)
# library(plotly)
# library(leaflet)
# library(sf)
library(glue)
# library(ggtext)
# library(colorspace)
library(twn)
library(readxl)
# library(patchwork)
# library(scales)

## ---- themes ----



## ---- load_data ----

copy_data(c("meetpunten.csv", "planten_info.xlsx", "biologie.csv", "biologie_kenmerken.csv"))

meetpunten <- HHSKwkl::import_meetpunten()
bio <- read_csv2("data/biologie.csv") %>% HHSKwkl::add_jaar()
bio_km <- read_csv2("data/biologie_kenmerken.csv") %>% HHSKwkl::add_jaar()
planten_info <- readxl::read_excel("data/planten_info.xlsx", sheet = "planten_info")

# ws_grens <-
#   st_read("data/ws_grens.gpkg", quiet = TRUE, crs = 28992) %>%
#   st_transform(crs = 4326)
# 
# 
# bio_km %>% 
#   filter(taxatype == "MACFT") %>% 
#   filter(methode != "VEGVLK") %>% 
#   select(-kenmerknaam) %>% 
#   pivot_wider(names_from = kenmerkcode, values_from = kenmerk_waarde) %>% 
#   mutate(zicht = ZICHT,
#          diepte = coalesce(WATDTE, `WATDTEWK+250`, `WATDTEWK+200`, `WATDTEWK+150`, `WATDTEWK+100`, `WATDTEWK+075`), 
#          .after = jaar) %>% 
#   # filter(jaar == 2022) %>% 
#   select(1:8) %>% 
#   type_convert(locale = locale(decimal_mark = ",")) %>% 
#   mutate(helder = zicht > 1 | (zicht >= 0.5 & zicht / diepte > 0.6) | zicht / diepte > 0.85) %>% 
#   # filter(!helder) %>% View()
#   ggplot(aes(helder)) + geom_bar() + facet_wrap(~jaar)
  
f_mp_type <- maak_opzoeker(meetpunten, mp, meetpunttypering)


planten <-
  bio %>%
  filter(methode %in% c("VEGSTO", "VEG%"),
         f_mp_type(mp) %in% c(1, 2, 3)) %>%
  select(-contains("stadium")) %>%
  distinct()

# planten <- HHSKwkl::import_biologie() %>%
#   filter(methode %in% c("VEGSTO", "VEG%"),
#          f_mp_type(mp) %in% c(1, 2, 3))


stowa_fun <- function(x){
  
  stowa_codes <- tibble::tribble(
    ~stowa, ~bedekking,
    0,          0,
    1,        0.5,
    2,          1,
    3,          2,
    4,          5,
    5,          8,
    6,         19,
    7,         38,
    8,         63,
    9,         88
  ) %>% tibble::deframe()
  
  unname(stowa_codes[as.character(x)])
  
}

planten_basis <-
  planten %>%
  select(mp, datum, methode, naam, waarde = waarde_totaal, eenheid) %>%
  mutate(waarde = ifelse(methode == "VEGSTO", stowa_fun(waarde), waarde)) %>%
  select(-methode, -eenheid) %>%
  # filter(naam %in% planten_info$naam) %>%
  left_join(select(planten_info, naam, bedekkingslaag, aquatisch, bijzonder), by = "naam") %>%
  # filter(aquatisch == "aquatisch") %>%
  mutate(jaar = year(datum)) 

helderheid_flab <- 
  bio_km %>%
  filter(taxatype == "MACFT") %>%
  filter(methode != "VEGVLK") %>%
  select(-kenmerknaam) %>%
  pivot_wider(names_from = kenmerkcode, values_from = kenmerk_waarde) %>%
  mutate(zicht = ZICHT,
         diepte = coalesce(WATDTE, `WATDTEWK+250`, `WATDTEWK+200`, `WATDTEWK+150`, `WATDTEWK+100`, `WATDTEWK+075`),
         .after = jaar) %>%
  # filter(jaar == 2022) %>%
  select(1:8, BDKFLAB) %>%
  type_convert(locale = locale(decimal_mark = ",")) %>%
  mutate(helder = zicht > 1 | (zicht >= 0.5 & zicht / diepte > 0.6) | zicht / diepte > 0.85) %>% 
  select(mp, datum, zicht, diepte, helder, flab_perc = BDKFLAB)

  # filter(!helder) %>% View()
 


woekerende_soorten <- c("Callitriche",
                          "Callitriche obtusangula",
                          "Callitriche platycarpa",
                          "Ceratophyllum demersum",
                          "Egeria densa",
                          "Elodea",
                          "Elodea nuttallii",
                          "Potamogeton pusillus",
                          "Stuckenia pectinata",
                          "Potamogeton")
 
weinig_relevant <- c("Lemna trisulca", "Riccia fluitans")
 

planten_basis_est <- 
  planten_basis %>% 
  group_by(mp, datum) %>% 
  summarise(kroos_perc     = sum((bedekkingslaag == "kroos") * waarde, na.rm = TRUE),
            drijfblad_perc = sum((bedekkingslaag == "drijfblad") * waarde, na.rm = TRUE),
            helo_perc      = sum((aquatisch == "facultatief_aquatisch") * waarde, na.rm = TRUE),
            aantal_submers = sum((bedekkingslaag == "submers"), na.rm = TRUE),
            aantal_submers_niet_woekerend = sum((bedekkingslaag == "submers" & !(naam %in% c(woekerende_soorten, weinig_relevant)) ), na.rm = TRUE),
            submers_perc   = sum((bedekkingslaag == "submers") * waarde, na.rm = TRUE),
            krabbescheer_perc = sum((naam == "Stratiotes aloides") * waarde, na.rm = TRUE),
            woekerend_perc = sum((naam %in% woekerende_soorten) * waarde, na.rm = TRUE),
            flab_perc_s    = sum((naam == "FLAB") * waarde, na.rm = TRUE), # Niet af te leiden uit alleen soortdata - is niet overal opgenomen
            ) %>% 
  ungroup() %>% 
  left_join(helderheid_flab) %>% 
  mutate(flab_perc = coalesce(flab_perc, flab_perc_s)) %>% 
  select(-flab_perc_s, -zicht, -diepte) 


planten_basis_est %>% 
  ggplot(aes(kroos_perc)) + geom_histogram()

planten_basis_est %>% 
  ggplot(aes(drijfblad_perc)) + geom_histogram()

planten_basis_est %>% 
  ggplot(aes(helo_perc)) + geom_histogram()

planten_basis_est %>% 
  ggplot(aes(aantal_submers_niet_woekerend)) + geom_bar()

planten_basis_est %>% 
  ggplot(aes(woekerend_perc)) + geom_histogram()

planten_basis_est %>% 
  ggplot(aes(flab_perc)) + geom_histogram()

planten_basis_est %>% 
  ggplot(aes(krabbescheer_perc)) + geom_histogram()


ests <- 
  planten_basis_est %>% 
  filter(year(datum) > 2010) %>%
  mutate(est = case_when(
    krabbescheer_perc > 10 ~ "Krabbescheersloot", 
    woekerend_perc > 50 & kroos_perc > 50  ~ "Kroos en woekerende waterplanten", # 11
    woekerend_perc > 50 ~ "Woekerende waterplanten", # 7
    drijfblad_perc > 10 & submers_perc > 10 ~ "Drijfblad met waterplanten", # 10
    submers_perc > 5 & (aantal_submers_niet_woekerend >= 1 & aantal_submers >= 3) | aantal_submers >= 5 ~ "Water met gevarieerde submerse planten", # 8
    drijfblad_perc > 10 ~ "Drijfblad", # 5
    kroos_perc   > 50 ~ "Kroos", # 4
    flab_perc   > 50 ~ "Flab",
    submers_perc > 5 ~ "Water met (monotone) submerse planten",
    helo_perc > 20 ~ "Veel helofyten",
    !helder ~ "Troebel zonder planten", # 123
    helder ~ "Helder zonder planten",
    
    
    TRUE ~ "Onbekend / Niet in te delen"
    
  )) 

est_omschrijvingen <- 
c(
  kroos = "Veel kroos", 
  troebel = "Troebel zonder waterplanten", 
  helder = "Helder zonder waterplanten", 
  submers_eenzijdig = "Veel of dezelfde onderwaterplanten", 
  gevarieerd = "Gevarieerde onderwaterplanten",
  helofyten = "Veel planten boven water", 
  onbekend = "Onbekend / Niet in te delen", 
  woekerend = "Woekerende waterplanten", 
  kroos_woekerend = "Veel kroos en veel onderwaterplanten", 
  flab = "Flab", 
  drijfblad = "Drijfbladplanten", 
  drijfblad_submers = "Drijfblad- en onderwaterplanten", 
  krabbescheer = "Krabbescheer")

ests <- 
  planten_basis_est %>% 
  filter(year(datum) > 2010) %>%
  mutate(est = case_when(
    krabbescheer_perc > 10 ~ "krabbescheer", 
    woekerend_perc > 50 & kroos_perc > 50  ~ "kroos_woekerend", # 11
    woekerend_perc > 50 ~ "submers_eenzijdig", # 7
    drijfblad_perc > 10 & submers_perc > 10 ~ "drijfblad_submers", # 10
    submers_perc > 5 & (aantal_submers_niet_woekerend >= 1 & aantal_submers >= 3) | aantal_submers >= 5 ~ "gevarieerd", # 8
    drijfblad_perc > 10 ~ "drijfblad", # 5
    kroos_perc   > 50 ~ "kroos", # 4
    flab_perc   > 50 ~ "flab",
    submers_perc > 5 ~ "submers_eenzijdig",
    helo_perc > 20 ~ "helofyten",
    !helder ~ "troebel", # 123
    helder ~ "helder",
    
    
    TRUE ~ "onbekend"),
    est_omsch = est_omschrijvingen[est]
    ) 

ests %>%   
  ggplot(aes(y = est)) + geom_bar()

ests %>% 
  filter(year(datum) %in% c(2020:2022)) %>% 
  select(mp, est, est_omsch) %>% 
  left_join(meetpunten) %>% 
  write_csv2("data/test_ests.csv")

labels <- c(
  "kroos" = "<img src='images/est/kroos.jpg' width='300' />",
  "troebel" = "<img src='images/est/troebel.jpg' width='300' />",
  "helder" = "<img src='images/est/helder_leeg.jpg' width='300' />",
  "submers_monotoon" = "<img src='images/est/woekerende_planten.jpg' width='300' />",
  "gevarieerd" = "<img src='images/est/gevarieerde_planten.jpg' width='300' />",
  "helofyten" = "<img src='images/est/helofyten.jpg' width='300' />",
  "onbekend" = "<img src='images/logo_website.png' width='300' />",
  "woekerend" = "<img src='images/est/woekerende_planten.jpg' width='300' />",
  "kroes_woekerend" = "<img src='images/est/kroos_woekerend.jpg' width='300' />",
  "flab" = "<img src='images/logo_website.png' width='300' />",
  "drijfblad" = "<img src='images/est/drijfblad.jpg' width='300' />",
  "drijfblad_submers"= "<img src='images/est/gevarieerd_en_drijfblad.jpg' width='300' />",
  "krabbescheer"= "<img src='images/logo_website.png' width='300' />"
)  

library(ggtext)
 
ests %>% 
  ggplot(aes(y = est)) + 
  geom_bar() +
  scale_y_discrete(
      name = NULL,
      labels = labels
    ) +
    theme(
      axis.text.y = element_markdown(color = "black", size = 11, hjust = 0)
    )
