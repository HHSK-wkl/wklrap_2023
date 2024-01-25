source("R/CODE_index_setup.R")

## ---- bleiswijkse-zoom ----

fys_chem %>%
  filter(year(datum) >= 2016) %>%
  add_jaar_maand() %>%
  filter(mp == "S_0125", parnr == 3) %>%
  mutate(datum = tsibble::yearmonth(datum)) %>%
  group_by(mp, parnr, par, eenheid, jaar, maand, datum) %>%
  summarise(waarde = mean(waarde)) %>%
  ungroup() %>%
  mutate(maand = month(datum, label = TRUE, abbr = FALSE)) %>%
  ggplot(aes(as.factor(maand), waarde, group = jaar, colour = (jaar != 2021), size = (jaar != 2021))) +
  geom_line() +
  scale_colour_manual(values = c(blauw, "grey60"), labels = c("2021","Andere jaren\n(2016 - 2020)")) +
  scale_size_manual(values = c(1.5, 0.5), labels = c("2021","Andere jaren\n(2016 - 2020)")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(expand = expansion(c(0,0.1)), limits = c(0, NA)) +
  labs(x = "",
       y = "mg P/l",
       title = "Fosfaat Bleiswijkse Zoom") +
  hhskthema() +
  guides(colour = guide_legend(title = ""), size = guide_legend(title = ""))


## ---- blauwalg-krimpenerwaard ----

kriwa_grens <- sf::st_transform(ws_grens, crs = 28992) %>% filter(NAAM %in% c("KRIMPENERWAARD", "STORMPOLDER", "DE ZAAG"))

fys_chem %>%
  filter(jaar == 2021, str_detect(mp, "^WISP|^K_"), parnr %in% c(415, 490)) %>%
  group_by(mp, parnr) %>%
  filter(waarde == max(waarde)) %>%
  ungroup() %>%
  mutate(waarde2 = ifelse(parnr == 490, waarde * 0.1135 - 6.401, waarde)) %>%
  mutate(klasse = cut(waarde2, c(-10,12,75,9999), labels = c("Geen gezondheidsrisico", "Gering gezondheidsrisico", "Gezondheidsrisico"))) %>%
  left_join(meetpunten, by = "mp") %>%
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
  ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(data = kriwa_grens, fill = NA, colour = "grey60", size = 1) +
  geom_sf(aes(colour = klasse), size = 4) +
  scale_colour_manual(values = c(blauw_m, oranje_m, oranje)) +
  labs(title = "Blauwalgen in de Krimpenerwaard",
       caption = "De risico-indeling is volgens het blauwalgenprotocol voor zwemwater") +
  guides(colour = guide_legend(title = "")) +
  hhskthema_kaart() +
  theme(legend.position = "bottom")


## ---- inlaat-krimpen ----

fys_chem %>%
  filter(mp == "K_0109", jaar > 2008, parnr == 3) %>%
  add_jaar_maand() %>%
  mutate(maandnaam = month(datum, label = TRUE, abbr = FALSE)) %>%
  # filter(jaar %in% jaren) %>%
  group_by(mp, parnr, jaar, maand, maandnaam) %>% # par, eenheid
  summarise(waarde = mean(waarde)) %>%
  ungroup() %>%
  ggplot(aes(maandnaam, waarde, group = jaar, colour = (jaar != 2021), size = (jaar != 2021))) +
  geom_line() +
  scale_colour_manual(values = c(blauw, "grey60"), labels = c("2021","Andere jaren\n(2009 - 2020)")) +
  scale_size_manual(values = c(1.5, 0.5), labels = c("2021","Andere jaren\n(2009 - 2020)")) +
  scale_y_continuous(expand = expansion(c(0,0.1)), limits = c(0, NA)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "",
       y = "mg P/l",
       title = "Fosfaat in Krimpen aan den IJssel",
       caption = "") +
  hhskthema() +
  guides(colour = guide_legend(title = ""), size = guide_legend(title = ""))


## ---- inlaat-leuvehaven ----

fys_chem %>%
  filter(mp == "S_0042", jaar > 2015, parnr == 3) %>%
  add_jaar_maand() %>%
  mutate(maandnaam = month(datum, label = TRUE, abbr = FALSE)) %>%
  # filter(jaar %in% jaren) %>%
  group_by(mp, parnr, jaar, maand, maandnaam) %>% # par, eenheid
  summarise(waarde = mean(waarde)) %>%
  ungroup() %>%
  ggplot(aes(maandnaam, waarde, group = jaar, colour = (jaar != 2021), size = (jaar != 2021))) +
  geom_line() +
  scale_colour_manual(values = c(blauw, "grey60"), labels = c("2021","Andere jaren\n(2016 - 2020)")) +
  scale_size_manual(values = c(1.5, 0.5), labels = c("2021","Andere jaren\n(2016 - 2020)")) +
  scale_y_continuous(expand = expansion(c(0,0.1)), limits = c(0, NA)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "",
       y = "mg P/l",
       title = "Fosfaat in de Binnenrotte",
       caption = "") +
  hhskthema() +
  guides(colour = guide_legend(title = ""), size = guide_legend(title = ""))
