## ---- libraries ----

library(HHSKwkl)
library(tidyverse)

## ---- parameters ----

rap_jaar <- 2023

## ---- algemene functies ----


copy_data(c("meetpunten.csv", "parameters.csv"))

meetpunten <- HHSKwkl::import_meetpunten()
parameters <- HHSKwkl::import_parameters()

f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
f_aquopar <- maak_opzoeker(parameters, parnr, aquo_parcode)
f_mp_type <- maak_opzoeker(meetpunten, mp, meetpunttypering)
f_gebied <-  maak_opzoeker(meetpunten, mp, gebiednaam)
f_landgebruik <- maak_opzoeker(meetpunten, mp, landgebruik)

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