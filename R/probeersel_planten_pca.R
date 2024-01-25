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

copy_data(c("meetpunten.csv",   "planten_info.xlsx", "biologie.csv", "biologie_kenmerken.csv"))

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
  mutate(jaar = year(datum)) %>%
  mutate(naam_species = ifelse(twn_taxonlevel(naam) < "Genus",
                               yes = twn::increase_taxonlevel(naam, taxonlevel = "Species"), no = NA))  
  

pca_data <- 
  planten_basis %>% 
  filter(jaar > 2018) %>% 
  filter(aquatisch == "aquatisch", !is.na(naam_species)) %>% 
  group_by(mp, datum, naam_species) %>% 
  summarise(waarde = sum(waarde)) %>% 
  ungroup() %>%  
  mutate(waarde_log = log1p(waarde)) %>% 
  mutate(monster = paste(mp, datum, sep = "_")) %>% 
  select(monster, naam_species, waarde_log) %>%
  pivot_wider(names_from = naam_species, values_from = waarde_log, values_fill = 0) %>% 
  column_to_rownames("monster")

library(factoextra)
res <- prcomp(pca_data)

fviz_eig(res)

# fviz_pca_ind(res,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )

fviz_pca_var(res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

pca_data %>% dist() %>% hclust() %>% plot()
