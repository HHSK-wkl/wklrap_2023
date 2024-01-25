options(OutDec = ",")
rap_jaar <- 2023

## ---- libs ----

library(tidyverse)
library(lubridate)
# devtools::install_github("RedTent/HHSKwkl") # Zie ook https://github.com/RedTent/HHSKwkl
library(HHSKwkl)
library(plotly)
library(leaflet)
library(sf)
library(glue)
library(ggtext)
library(colorspace)
library(twn)
library(readxl)
library(patchwork)
library(scales)

## ---- themes ----



## ---- load_data ----

copy_data(c("meetpunten.csv", "parameters.csv", "fys_chem.csv",
            "normen.txt", "fys_chem.rds", "gbm_toxiciteit.xlsx",
            "planten_info.xlsx", "biologie.csv"))

fys_chem <- readRDS("data/fys_chem.rds") %>% HHSKwkl::add_jaar()
meetpunten <- HHSKwkl::import_meetpunten()
parameters <- HHSKwkl::import_parameters()
toxiciteit <- readxl::read_excel("data/gbm_toxiciteit.xlsx", sheet = "SSDinfo")
bio <- read_csv2("data/biologie.csv") %>% HHSKwkl::add_jaar()
planten_info <- readxl::read_excel("data/planten_info.xlsx", sheet = "planten_info")

ws_grens <-
  st_read("data/ws_grens.gpkg", quiet = TRUE, crs = 28992) %>%
  st_transform(crs = 4326)

## ---- functions ----

# Niet meer in gebruik
# layout_ggplotly_nut <- function(gg, x = -0.05, y = -0.05){
#   # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
#   #gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
#   gg[['x']][['layout']][['annotations']][[1]][['x']] <- y
#   gg
# }

blauwe_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#0079c2{grootte}'>{tekst}</span>")
}

oranje_tekst <- function(tekst, grootte = NA){
  grootte <- if (is.na(grootte)) "" else glue::glue(";font-size:{grootte}px")
  glue::glue("<span style='color:#C25100{grootte}'>{tekst}</span>")
}



f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)
f_mp_type <- maak_opzoeker(meetpunten, mp, meetpunttypering)
f_gebied <-  maak_opzoeker(meetpunten, mp, gebiednaam)
f_landgebruik <- maak_opzoeker(meetpunten, mp, landgebruik)
# f_landgebruik <- meetpunten %>%
#   rename(landgebruik = `landgebruik 2015`) %>%
#   mutate(landgebruik = str_replace(landgebruik, "Afvoer/gemaal", "Polderafvoer"),
#          landgebruik = str_replace(landgebruik, "Gras", "Grasland")) %>%
#   maak_opzoeker(mp, landgebruik)

nieuwste_bestand <- function(pattern, pad = "data"){
  tibble(files =  list.files(path = pad, pattern = pattern, full.names = TRUE)) %>%
    mutate(tijd = file.info(files)$mtime) %>%
    filter(tijd == max(tijd)) %>%
    .[[1,1]]
}

icon <- function(type, kleur){
  fontawesome::fa(type, fill = kleur, prefer_type = "solid", height = "1.2em")
}

reverselog_trans <- function(base = 10, n = 5) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
            scales::log_breaks(n = n, base = base),
            domain = c(1e-100, Inf))
}

## ---- kleuren ----

# kleuren zijn opgenomen in HHSKwkl

# blauw    <- hex(HLS(202.5, 0.38, 1))
# blauw_m  <- hex(HLS(202.5, 0.60, 1))
# blauw_l  <- hex(HLS(202.5, 0.80, 1))
# oranje   <- hex(HLS(25   , 0.38, 1))
# oranje_m <- hex(HLS(25   , 0.60, 1))
# oranje_l <- hex(HLS(25   , 0.80, 1))
grijs    <- "#616161"
# grijs_m  <- "grey60"
# grijs_l  <- "grey80"

## ---- themas ----

# Checken welke gebruikt worden

theme_set(hhskthema())

thema_hor_bar <-
  hhskthema() +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(hjust = 0, margin = margin(r = 8)),
        plot.caption.position = "plot",
        plot.subtitle = element_text(face = "italic"))

thema_vert_bar <-
  hhskthema() +
  theme(axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_blank(),
        plot.title.position = "plot",
        # axis.text.y = element_text(hjust = 0, margin = margin(r = 8)),
        plot.caption.position = "plot",
        plot.subtitle = element_text(face = "italic"))

thema_line_facet <-
  hhskthema() +
  theme(panel.grid.major.x = element_blank(),
        plot.title.position = "plot",
        # axis.text.y = element_text(hjust = 0, margin = margin(r = 8)),
        plot.caption.position = "plot",
        plot.subtitle = element_text(face = "italic"),
        panel.spacing = unit(40, "points"),
        strip.background = ggplot2::element_rect(fill = NA, colour = NA),
        strip.text = ggplot2::element_text(face = "bold", color = "grey50", size = 12)
  )

hhskthema_bar <- theme_light() +
  theme(
    plot.title = element_text(color = hhskgroen, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = hhskgroen, face = "bold", hjust = 0.5, size = rel(1.1)),
    plot.caption = element_text(color = hhskgroen, face = "italic"),
    axis.title = element_text(color = hhskblauw, face = "bold"),
    axis.text = element_text(color = hhskblauw),
    axis.ticks = element_line(color = hhskblauw),
    axis.ticks.x = element_blank(),
    axis.line.y = element_line(color = hhskblauw, size = 1),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(color = hhskgroen, face = "bold", hjust = 0.5),
    legend.text = element_text(color = hhskblauw),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = hhskblauw))


