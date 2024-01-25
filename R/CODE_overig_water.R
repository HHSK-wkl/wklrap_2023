# source("R/CODE_index_setup.R")

library(HHSKwkl)
library(tidyverse)
library(readxl)
library(glue)
library(sf)
library(leaflet)

## ---- parameters ----

rap_jaar <- 2023
ws_grens <- st_read("data/ws_grens.gpkg", quiet = TRUE) %>% st_transform(crs = 4326)


## ---- prep-overig ----

kleuren_ov <- c(Akkerbouwgebied       = RColorBrewer::brewer.pal(9, "Set1")[6], # akkerbouw
                Glastuinbouwgebied    = RColorBrewer::brewer.pal(9, "Set1")[2],
                Weidegebied            = RColorBrewer::brewer.pal(9, "Set3")[7],
                "Stedelijk gebied"       = RColorBrewer::brewer.pal(9, "Set1")[1],
                "Eendragtspolder plas-dras" = RColorBrewer::brewer.pal(9, "Set1")[5],
                "Zwemplas Krimpenerhout" = RColorBrewer::brewer.pal(9, "Set1")[4],
                "Natuurgebied" = RColorBrewer::brewer.pal(9, "Set1")[3],
                "Waterparel Zuidplaspolder" = RColorBrewer::brewer.pal(9, "Set1")[8])

f_kleur_ov <- colorFactor(palette = kleuren_ov, levels = names(kleuren_ov))

nieuwste_bestand <- function(pattern, pad = "data"){
  tibble(files =  list.files(path = pad, pattern = pattern, full.names = TRUE)) %>%
    mutate(tijd = file.info(files)$mtime) %>%
    filter(tijd == max(tijd)) %>%
    .[[1,1]]
}


kaart_overig <-
  st_read("data/overig_water_kaart.gpkg") %>%
  st_transform(crs = 4326) %>%
  basiskaart() %>%
  addPolylines(data = ws_grens, opacity = 1, color = "grey", weight = 2, label = "waterschapsgrens") %>%
  addPolygons(color = ~f_kleur_ov(naam_ovw), fillOpacity = 0.7, stroke = 0, label = ~naam_ovw, popup = ~naam_ovw) %>%
  addLegend(position = "topright", pal = f_kleur_ov, values = ~naam_ovw, opacity = 0.7, title = "Overig water")

ekr_overig <- read_excel(nieuwste_bestand("EKRs overig water.xlsx")) %>%
  filter(!str_detect(ovw_naam, "Eendragtspolder|Krimpenerhout"))

ekr_edp <- read_excel(nieuwste_bestand("EKR's EDP Plas Dras.xlsx")) %>%
  mutate(ovw_naam = "Eendragtspolder plas-dras", ovw_code = "NL39_DOW_6") %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr))


ekr_kriho <- read_excel(nieuwste_bestand("EKR's Krimpenerhout.xlsx")) %>%
  mutate(ovw_naam = "Zwemplas Krimpenerhout", ovw_code = "NL39_DOW_8") %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr))

# cuts <- 2021 - c(0, 3, 6, 9 , 12, 15, 18, 21, 50) + 0.5

doelen_overig <- tibble::tribble(
  ~ovw_naam,                      ~ovw_code, ~doel_ekr_veg,
  "Stedelijk gebied",          "NL39_DOW_1",           0.3,
  "Weidegebied",               "NL39_DOW_2",           0.4,
  "Glastuinbouwgebied",        "NL39_DOW_3",           0.3,
  "Akkerbouwgebied",           "NL39_DOW_4",          0.35,
  "Natuurgebied",              "NL39_DOW_5",          0.45,
  "Eendragtspolder plas-dras", "NL39_DOW_6",           0.6,
  "Waterparel Zuidplaspolder", "NL39_DOW_7",          0.45,
  "Zwemplas Krimpenerhout",    "NL39_DOW_8",           0.5
)


overig_doelen <-
  doelen_overig %>%
  mutate(ovw_naam = fct_reorder(ovw_naam, ovw_code, .fun = last)) %>%
  mutate(doel = pmin(doel_ekr_veg / 0.6, 1),
         doelaanpassing = 1 - doel) %>%
  pivot_longer(cols = c(doel, doelaanpassing)) %>%
  ggplot(aes(value, fct_rev(ovw_naam), fill = fct_rev(name))) +
  geom_col() +
  scale_fill_manual(values = c(doel = blauw_m, doelaanpassing = grijs_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "Hoogte doel t.o.v. standaard",
       y ="",
       title = "Doelen overig water - waterplanten") +
  hhskthema() +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "panel",
        plot.margin = margin(5.5, 15, 5.5, 5.5)) +
  guides(fill = guide_legend(title = "", reverse = TRUE))


overig_opgave <-
  ekr_overig %>%
  add_jaar() %>%
  filter(jaar >= rap_jaar - 2) %>%
  group_by(ovw_code, ovw_naam) %>%
  summarise(ekr = mean(ekr)) %>%
  ungroup() %>%
  bind_rows(ekr_edp, ekr_kriho) %>%
  left_join(doelen_overig) %>%
  mutate(`huidige toestand` = pmin(ekr / doel_ekr_veg, 1),
         doelgat = 1 - `huidige toestand`) %>%
  pivot_longer(c(`huidige toestand`, doelgat)) %>%
  mutate(ovw_naam = fct_reorder(ovw_naam, ovw_code, .fun = last)) %>%
  ggplot(aes(value, fct_rev(ovw_naam), fill = name)) +
  geom_col() +
  scale_fill_manual(values = c("huidige toestand" = blauw_m, doelgat = oranje_m)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), labels = function(x) scales::percent(x, accuracy = 1)) +
  labs(x = "Toestand ten opzichte van het doel",
       y ="",
       title = "Opgave overig water - waterplanten",
       caption = "Gemiddelde van de laatste 3 jaar") +
  hhskthema() +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "panel",
        plot.margin = margin(5.5, 15, 5.5, 2)) +
  guides(fill = guide_legend(title = "", reverse = TRUE))


# doel_en_opgave_overig <- (
#   overig_doelen | (overig_opgave + theme(axis.text.y = element_blank()))
#                 ) +
#   plot_annotation(title = "Overig water - waterplanten")




  # scale_fill_manual(values = c(oranje_m, blauw_m)) +
  # thema_hor_bar

