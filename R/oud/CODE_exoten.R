# source("R/CODE_index_setup.R")

aanwezigheid_kreeften <-
  bio %>%
  filter(methode == "KR12", is_taxontype(naam, "Macroinvertebrates") | naam == "Geen kreeften") %>%
  select(-contains("stadium")) %>%
  distinct() %>%
  group_by(mp, jaar) %>%
  summarise(waarde = sum(waarde_totaal)) %>%
  ungroup() %>%
  mutate(aanwezigheid = case_when(
    waarde == 0 ~ "Kreeften afwezig",
    TRUE        ~ "Kreeften aanwezig")
    ) %>%
  left_join(meetpunten) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992, remove = FALSE) %>%

  ggplot() +
  geom_sf(data = st_transform(ws_grens, crs = 28992), colour = "grey80", fill = "grey90", linewidth = 1) +
  geom_sf(aes(colour = aanwezigheid), size = 2) +
  scale_colour_manual(values = c("Kreeften afwezig" = blauw, "Kreeften aanwezig" = oranje), guide = "none") +
  # facet_wrap(~aanwezigheid) +
  labs(#title = "Aanwezigheid Amerikaanse Rivierkreeften",
       subtitle = " ",
       caption = "2020-2022") +
  hhskthema_kaart() +
  theme(strip.background = element_rect(colour = NA),
        strip.text = element_text(size = 12)) +
  NULL

#  Niet te groot opnemen in x
p_kreeftenpilot <-
  bio %>%
  filter(jaar == 2022, methode == "VEG%", f_gebied(mp) == "Krimpenerwaard") %>%
  left_join(planten_info, by = "naam") %>% group_by(mp, bedekkingslaag) %>%
  summarise(waarde = sum(waarde_totaal, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(mp, bedekkingslaag, fill = list(waarde = 0)) %>%
  filter(bedekkingslaag == "submers") %>%
  mutate(pilotgebied = ifelse(str_detect(mp, "^VBWP"), "Pilotgebied", "Krimpenerwaard")) %>%
  group_by(pilotgebied) %>%
  summarise(n_aanwezig = sum(waarde >= 5, na.rm = TRUE),
            aanwezig = n_aanwezig / n(),
            afwezig = 1 - aanwezig) %>%
  ggplot(aes(aanwezig, pilotgebied)) +
  geom_col(fill = blauw_m, width = 0.85, colour = "grey60") +
  # facet_wrap(~pilotgebied) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), labels = percent_format(accuracy = 1)) +
  labs(title = "Effect wegvangen kreeften",
       subtitle = "Locaties met waterplanten",
       y = "",
       x = "",
       caption = "locaties met tenminste 5% begroeiing") +
  thema_hor_bar +
  theme(axis.ticks.y = element_blank(),
        axis.line.y = element_line(colour = "grey40"))



# Invasieve plantenexoten -------------------------------------------------

# ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992)

planten_exoten <-
  st_read("data/ndff/NDFF_exoten_2022.shp") %>%
  st_filter(st_transform(ws_grens, crs = 28992))
# as_tibble() %>%
# sf::st_as_sf(coords = c("x", "y"), crs = 28992, remove = FALSE)

soorten_sel <- c("Grote waternavel", "Parelvederkruid", "Watercrassula", "Waterteunisbloem", "Waterwaaier")

pal <- colorFactor(RColorBrewer::brewer.pal(7, "Set1"), domain = soorten_sel )

kaart_exoten <-
  planten_exoten %>%
  mutate(jaar = year(datm_start)) %>%
  filter(jaar >= 2020) %>%
  filter(soort_ned %in% soorten_sel) %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles(., "CartoDB.Positron", group = "Kaart") %>%
  leaflet::addProviderTiles("Esri.WorldImagery", group = "Luchtfoto") %>%
  leaflet::addLayersControl(baseGroups = c("Kaart", "Luchtfoto"),
                            options = leaflet::layersControlOptions(collapsed = FALSE),
                            position = "topleft") %>%
  addPolylines(data = sf::st_transform(ws_grens, crs = 4326), color = "grey", opacity = 1, weight = 2, label = ~"Waterschapsgrens") %>%
  addCircleMarkers(label = ~paste(soort_ned, "-", jaar), weight = 1, fillOpacity = 1, opacity = 1, radius = 8,
                   fillColor = ~pal(soort_ned), color = "grey") %>%
  addLegend(values = ~soort_ned, pal = pal, opacity = 1, title = "Soorten")
# addPolygons(label = ~soort_ned, stroke = FALSE, fillOpacity = 0.9)

