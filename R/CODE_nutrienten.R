# source("R/CODE_index_setup.R")

library(tidyverse)
library(HHSKwkl)
library(glue)
library(colorspace)
library(scales)

rap_jaar <- 2023

fys_chem <- readRDS("data/fys_chem.rds") %>% HHSKwkl::add_jaar()
meetpunten <- HHSKwkl::import_meetpunten()
parameters <- HHSKwkl::import_parameters()

theme_set(hhskthema())

## ---- nutrienten-1 ----

landgebruik_sel <- c("glastuinbouw", "grasland - agrarisch", "grasland - natuur",
                     "stedelijk", "boezem", "grote plassen", "akkerbouw")
mp_sel <-
  meetpunten %>%
  filter(meetpunttypering == 1, landgebruik %in% landgebruik_sel) %>%
  select(mp, landgebruik) %>%
  mutate(landgebruik = str_remove(landgebruik, " - .*"))
# het onderscheid tussen gras natuur en agrarisch weglaten

grafiek_data <-
  fys_chem %>%
  filter(jaar >= rap_jaar-10,
         jaar <= rap_jaar,
         parnr %in% c(3, 7)) %>%
  inner_join(mp_sel, by = "mp") %>%
  # soms worden in 1 maand meerdere metingen gedaan, die betreffende meetpunten wegen dan zwaarder dan de rest.
  # Daarom is het beter om eerst het maandgemiddelde te nemen om bias te voorkomen.
  add_maand() %>%
  group_by(mp, parnr, landgebruik, jaar, maand) %>%
  summarise(waarde = mean(waarde)) %>%
  group_by(parnr, landgebruik, jaar) %>%
  summarise(waarde_gem = mean(waarde)) %>%
  ungroup() %>%
  left_join(parameters, by = "parnr") %>%
  # group_by(parnr, landgebruik) %>%
  # mutate(rank = rank(waarde_gem),
  #        tekst = glue("{landgebruik} {jaar}<br>Gemiddeld {format(waarde_gem, digits = 2)} {eenheid}")) %>%
  # ungroup() %>%
  # arrange(parnr, landgebruik, desc(waarde_gem)) %>%
  mutate(#order = row_number(),
         fill_group = ifelse(jaar == rap_jaar, rap_jaar, "Andere jaren"),
         landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE))

kleuren_functies_nutrienten <-
  c(Akkerbouw              = RColorBrewer::brewer.pal(12, "Set3")[6], # akkerbouw
    Boezem                 = RColorBrewer::brewer.pal(12, "Set3")[5],# boezem
    "Grasland - natuur"    = RColorBrewer::brewer.pal(12, "Set3")[11], #afvoer
    Glastuinbouw           = RColorBrewer::brewer.pal(12, "Set3")[3],
    "Grasland - agrarisch" = RColorBrewer::brewer.pal(12, "Set3")[7],
    Stedelijk              = RColorBrewer::brewer.pal(12, "Set3")[4],
    "Grote plassen"        = RColorBrewer::brewer.pal(12, "Set3")[1],
    "Grasland"             = RColorBrewer::brewer.pal(12, "Set3")[7])



nutrienten_plot <-
  grafiek_data %>%
  filter(jaar == rap_jaar) %>%
  mutate(landgebruik = str_to_sentence(landgebruik)) %>%
  mutate(par_strip = c("3" = "Fosfaat (mg P/l)", "7" = "Stikstof (mg N/l)")[as.character(parnr)]) %>%
  mutate(landgebruik = fct_reorder(landgebruik, .fun = first, waarde_gem, .desc = FALSE)) %>%
  ggplot(aes(waarde_gem, landgebruik, fill = landgebruik)) +
  geom_col(colour = "grey60", linewidth = 0.85) +
  facet_wrap(vars(par_strip), scales = "free_x", strip.position = "bottom") +
  scale_x_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
  scale_fill_manual(values = kleuren_functies_nutrienten) +
  labs(title = "Nutriënten",
       subtitle = glue("gemiddeld per gebied in {rap_jaar}"),
       y = "",
       x = "") +
  thema_hor_bar +
  theme(axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.background = ggplot2::element_rect(fill = NA, colour = NA),
        strip.text = ggplot2::element_text(face = "bold", color = "grey50", size = 12)) +
  guides(fill = "none") +
  geom_vline(xintercept = 0,color = "grey40", size = 0.5) + # kunstmatige y-aslijn
  NULL

kleuren_functies_lijnen <-
  kleuren_functies_nutrienten %>%
  darken(amount = 0.3, space = "combined", method = "absolute") %>%
  set_names(str_to_lower(names(kleuren_functies_nutrienten)))

fosfor_plot <-
  grafiek_data %>%
  filter(parnr == 3) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  ggrepel::geom_text_repel(aes(label = landgebruik, x = jaar + 0.1), data = filter(grafiek_data, parnr == 3, jaar == rap_jaar),
                           hjust = "left",  size = 5, direction = "y", nudge_x = 0.2, xlim = c(NA, rap_jaar + 3)) +

  # geom_smooth(se = FALSE, span = 0.6) +
  # scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_colour_manual(values = kleuren_functies_lijnen, guide = FALSE) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10), limits = c(NA, NA)) +
  labs(title = "Gemiddelde fosfaatconcentratie",
       x = "",
       y = "mg P/l" )  +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 90, 5.5, 5.5, unit = "pt")) +
  NULL

stikstof_plot <-
  grafiek_data %>%
  filter(parnr == 7) %>%
  mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
  ggplot(aes(jaar, waarde_gem, group = landgebruik, colour = landgebruik)) +
  geom_line(linewidth = 1) +
  ggrepel::geom_text_repel(aes(label = landgebruik, x = jaar + 0.1), data = filter(grafiek_data, parnr == 7, jaar == rap_jaar),
                           hjust = "left",  size = 5, direction = "y", nudge_x = 0.2, xlim = c(NA, rap_jaar + 3)) +

  # geom_smooth(se = FALSE, span = 0.6) +
  # scale_color_brewer(palette = "Dark2", guide = FALSE) +
  scale_colour_manual(values = kleuren_functies_lijnen, guide = FALSE) +
  scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
  scale_x_continuous(breaks = pretty_breaks(n = 10), limits = c(NA, NA)) +
  labs(title = "Gemiddelde stikstofconcentratie",
       x = "",
       y = "mg N/l" )  +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(5.5, 90, 5.5, 5.5, unit = "pt")) +
  NULL

# NIET MEER OPNEMEN ???

# trends <- grafiek_data %>%
#   arrange(parnr, landgebruik, jaar) %>%
#   group_by(parnr, landgebruik) %>%
#   nest() %>%
#   mutate(test = map(data, ~trend::mk.test(.$waarde_gem)),
#          `p-waarde` = map_dbl(test, ~round(.$p.value, digits = 3)),
#          richting = map_dbl(test, ~sign(.$statistic))) %>%
#   ungroup() %>%
#   mutate(Trend = case_when(
#     `p-waarde` > 0.05 ~ "- geen trend",
#     `p-waarde` < 0.05 & richting == -1 ~ '- <span style="color:#0079C2;font-weight:normal;">dalende trend</span>',
#     `p-waarde` < 0.05 & richting == 1 ~ '- <span style="color:#C25100; font-weight:normal;">stijgende trend</span>'
#   ))
#
# grafiek_data2 <-
#   grafiek_data %>%
#   mutate(kleur = kleuren_functies_nutrienten[as.character(landgebruik)]) %>%
#   left_join(trends, by = c("landgebruik", "parnr")) %>%
#   mutate(landgebruik = glue("{landgebruik} {Trend}"))
#
# fosfor_plot <-
#   grafiek_data2 %>%
#   filter(parnr == 3) %>%
#   mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
#   ggplot(aes(as.factor(jaar), waarde_gem, fill = kleur, text = tekst)) +
#   geom_col(width = 0.8, colour = "grey60") +
#   facet_wrap(~landgebruik, ncol = 2, scales = "free") +
#   scale_y_continuous(limits = c(0,NA), expand = expansion(c(0,0.1))) +
#   labs(title = "Gemiddelde fosfaatconcentratie",
#        x = "",
#        y = "mg P/l",
#        caption = "N.B. De schaal van de Y-as verschilt per figuur.") +
#   thema_line_facet +
#   scale_fill_identity() +
#   # scale_fill_manual(values = c(hhskblauw, "grey60"), labels = c(rap_jaar, "Andere jaren"), ) +
#   theme(legend.position = "none",
#         panel.spacing = unit(25, "points"),
#         axis.ticks.x = element_blank(),
#         strip.text = element_markdown())
#
#
# stikstof_plot <-
#   grafiek_data2 %>%
#   filter(parnr == 7) %>%
#   mutate(landgebruik = fct_reorder(landgebruik, waarde_gem, .desc = TRUE)) %>%
#   ggplot(aes(as.factor(jaar), waarde_gem, fill = kleur, text = tekst)) +
#   geom_col(width = 0.8, colour = "grey60") +
#   facet_wrap(~landgebruik, ncol = 2, scales = "free") +
#   scale_y_continuous(limits = c(0, NA), expand = expansion(c(0,0.1))) +
#   labs(title = "Gemiddelde stikstofconcentratie",
#        x = "",
#        y = "mg N/l",
#        caption = "N.B. De schaal van de Y-as verschilt per figuur.") +
#   thema_line_facet +
#   scale_fill_identity() +
#   # scale_fill_manual(values = c(hhskblauw, "grey60"), labels = c(rap_jaar, "Andere jaren"), ) +
#   theme(legend.position = "none",
#         panel.spacing = unit(25, "points"),
#         axis.ticks.x = element_blank(),
#         strip.text = element_markdown())



# Berekening trend en verandering -----------------------------------------

library(trend)

# x <- grafiek_data %>% filter(parnr == 3, landgebruik == "glastuinbouw") %>% pull(waarde_gem)

# y <- sens.slope(x)

# str(y)

# glue("{round(y$estimates * 10, digits = 3)} mg P/l per 10 jaar" )

trends <- 
  grafiek_data %>%
  # filter(jaar > 2013) %>%
  group_by(parnr, par, landgebruik) %>%
  nest() %>%
  mutate(sen = map(data, ~trend::sens.slope(.x$waarde_gem)),
         helling = map_dbl(sen, "estimates"),
         p_value = map_dbl(sen, "p.value"),
         gem = map_dbl(data, ~mean(.x$waarde_gem))) %>%
  mutate(decade = round(helling * 10, digits = 3),
         decade_rel = -decade / (2 * gem)) %>%
  mutate(kans =  1 / p_value) %>%
  # filter(p_value < 0.05) %>%
  arrange(kans)
