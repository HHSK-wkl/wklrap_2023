source("R/CODE_index_setup.R")

## ---- kaart-zwemlocaties ----

zwemlocaties <- tibble::tribble(
  ~mp,                            ~naam,     ~oordeel,
  "S_0058",     "Zevenhuizerplas Nesselande", "Uitstekend",
  "S_0124",               "Bleiswijkse Zoom", "Uitstekend",
  "S_0128",                 "Kralingse Plas",       "Goed",
  "S_0131", "Zevenhuizerplas Noordwestzijde", "Uitstekend",
  "S_1120",               "'t Zwarte Plasje", "Uitstekend",
  "S_1124",               "Kralings Zwembad", "Uitstekend",
  "K_1102",                  "Krimpenerhout",       "Goed"
)


zwem_icons <- leaflet::iconList(
  Uitstekend =   leaflet::makeIcon("images/icon_zwemwater_blauw.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Goed =         leaflet::makeIcon("images/icon_zwemwater_groen.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Aanvaardbaar = leaflet::makeIcon("images/icon_zwemwater_oranje.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15),
  Slecht =       leaflet::makeIcon("images/icon_zwemwater_rood.png",
                                   iconWidth = 30, iconHeight = 30,
                                   iconAnchorX = 15, iconAnchorY = 15)
)

# Let op dat de iconen ook in _book/images staan anders dan doet de legenda het niet
zwem_legend <- paste0("<b>Oordeel</b></br>",
                      "<img src='images/icon_zwemwater_blauw.png' height='30' width = '30'> Uitstekend<br/>",
                      "<img src='images/icon_zwemwater_groen.png' height='30' width = '30'> Goed<br/>",
                      "<img src='images/icon_zwemwater_oranje.png' height='30' width = '30'> Aanvaardbaar<br/>",
                      "<img src='images/icon_zwemwater_rood.png' height='30' width = '30'> Slecht")


zwemlocaties %>%
  mutate(popup = paste0(naam, "</br><b>Oordeel:</b> ", oordeel)) %>%
  left_join(meetpunten, by = "mp") %>%
  HHSKwkl::add_lat_long() %>%
  HHSKwkl::basiskaart() %>%
  addMarkers(icon = ~zwem_icons[oordeel], label = ~naam, popup = ~popup, options = markerOptions(riseOnHover = TRUE)) %>%
  addPolylines(data = ws_grens, color = "red", weight = 3) %>%
  addControl(html = zwem_legend, position = "topright")

## ---- waarschuwingen-blauwalg ----

afkortingen <- c("S_0058" = "ZN",
                 "S_0124" = "BZ",
                 "S_0128" = "KP",
                 "S_0131" = "ZH",
                 "S_1120" = "ZP",
                 "S_1124" = "KZ",
                 "K_1102" = "KH")

waarschuwingen <-
  readxl::read_excel("data/Zwemwater maatregelen vanaf 2012 v2.xlsx") %>%
  rename_all(tolower) %>%
  filter(probleem == "Blauwalg", jaar <= rap_jaar) %>%
  left_join(zwemlocaties, by = c("meetpunt" = "mp")) %>%
  filter(!is.na(naam)) %>%
  group_by(jaar, meetpunt, naam) %>%
  summarise(dagen = sum(dagen, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(afkorting = afkortingen[meetpunt])

waarschuwingen_rap_jaar <- waarschuwingen %>% filter(jaar == rap_jaar) %>% summarise(dagen = sum(dagen)) %>% pull(dagen)

titel_tekst <- glue("Aantal dagen met een waarschuwing voor blauwalg in {rap_jaar}: <b style='color:#0079c2'>{waarschuwingen_rap_jaar}</b>")



blauwalgenplot <- waarschuwingen %>%
  ggplot(aes(as.character(jaar), dagen, colour = naam, fill = (jaar == rap_jaar), text = glue("{naam}\n{dagen} dagen"))) +
  geom_col(width = 0.8) +
  geom_text(aes(label = afkorting), size = 3, position = position_stack(vjust = 0.5), data = filter(waarschuwingen, dagen >= 12)) +
  scale_fill_manual(values = c("TRUE" = hhskblauw, "FALSE" = "grey60"), guide = FALSE) +
  scale_colour_manual(values = rep("white", times = 7), guide = FALSE) +
  thema_vert_bar +
  labs(title = titel_tekst,
       x = "",
       y = "Aantal dagen") +
  scale_y_continuous(limits=c(0, 300),expand = c(0, 0),oob = scales::rescale_none) +
  theme(plot.title = element_markdown(face = "bold"),
        axis.line.x = element_line(colour = "grey60"))

blauwalgenplot %>%
  plotly::ggplotly(tooltip = "text") %>%
  plotly::config(displayModeBar = FALSE) %>%
  layout(dragmode = FALSE,
         showlegend = FALSE,
         title = list(x = "0"),
         hoverlabel = list(align = "left"))


