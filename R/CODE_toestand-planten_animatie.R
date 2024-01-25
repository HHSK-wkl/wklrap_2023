source("R/CODE_index_setup.R")

library(gganimate)

## ---- planten-voorbewerking ----

planten_info <- readxl::read_excel("data/planten_info.xlsx", sheet = "planten_info")

planten <-
  read_csv2("data/biologie.csv") %>%
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

planten_per_groep <-
  planten_basis %>%
  group_by(mp, datum, bedekkingslaag, jaar) %>%
  summarise(waarde = sum(waarde),
            waarde = ifelse(waarde > 100, 100, waarde)) %>%
  ungroup() %>%
  # filter(mp %in% c("EO 17", "EO 19")) %>%
  complete(nesting(mp, datum, jaar), bedekkingslaag, fill = list(waarde = 0)) %>% # toevoegen van ontbrekende bedekkingslagen
  filter(!is.na(bedekkingslaag))


data_submers <-
  planten_per_groep %>%
  mutate(gebied = f_gebied(mp)) %>%
  filter(bedekkingslaag == "submers",
         gebied %in% c("Schieland", "Krimpenerwaard")) %>%
  group_by(jaar, gebied) %>%
  summarise(perc = perc(waarde > 5))


p <-
  data_submers %>%
  filter(jaar >= 2008) %>%
  filter(gebied == "Krimpenerwaard") %>%
  mutate(tekst = glue("{round(perc)} %")) %>%
  ggplot(aes(jaar, perc)) +
  geom_line(colour = blauw, size = 1.2) +
  geom_point(aes(group = seq_along(jaar)), colour = blauw, size = 4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.0)), limits = c(0, 85)) +
  scale_x_continuous(breaks = scales::breaks_width(1), limits = c(NA, rap_jaar)) +
  labs(title = "Steeds minder waterplanten in de Krimpenerwaard",
       subtitle = "Welk deel van de wateren in de Krimpenerwaard heeft planten onder water?", # "{round(frame_along)}"
       y = "% van alle wateren",
       x = "",
       caption = "locaties met meer dan 5% bedekking met planten onder water") +
  thema_line_facet +
  geom_label(aes(label = tekst, group = seq_along(jaar)), nudge_y = 5, size = 6) + # , direction = "y", point.padding = 0.5
  NULL +
  theme(plot.title = element_text(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, margin = margin(b = 18)),
        axis.title.y = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.caption = element_text(size = 12),
        panel.grid.major.y = element_line(size = 0.8)
        )


p
p + transition_reveal(jaar)
p2 <- p + transition_reveal(jaar) # + shadow_mark()
  # transition_states(jaar2)

p2 %>% animate(width = 720, nframes = 120, end_pause = 40)
anim_save("J:/desktop/planten_kriwa_tm_2021.gif")

# plot_kroos <-
#   planten_per_groep %>%
#   mutate(gebied = f_gebied(mp)) %>%
#   filter(jaar > 2008, bedekkingslaag == "kroos",
#          gebied %in% c("Schieland", "Krimpenerwaard")) %>%
#   group_by(jaar, gebied) %>%
#   summarise(perc = perc(waarde > 50)) %>%
#   ggplot(aes(jaar, perc)) +
#   geom_smooth(se = FALSE, method = "lm", colour = blauw_l, linetype = "dashed") +
#   geom_line(colour = blauw, size = 1) +
#   geom_point(colour = blauw) +
#   facet_wrap(~gebied, scales = "free_y") +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.0)), limits = c(0, 50)) +
#   scale_x_continuous(breaks = scales::breaks_width(2), limits = c(NA, rap_jaar)) +
#   labs(title = "Wateren met veel kroos",
#        subtitle = " ",
#        y = "% van alle wateren",
#        x = "",
#        caption = " meer dan de helft bedekt met kroos") +
#   thema_line_facet
#
#
# # toestand laatste 3 jaar -------------------------------------------------
#
# aantal_bijzonder <-
#   planten_basis %>%
#   group_by(mp, jaar, datum) %>%
#   summarise(n_bijzonder = sum(bijzonder, na.rm = TRUE)) %>%
#   mutate(gebied = f_gebied(mp)) %>%
#   filter(gebied %in% c("Schieland", "Krimpenerwaard")) %>%
#   filter(jaar >= rap_jaar - 2) %>%
#   group_by(gebied) %>%
#   summarise(`Minstens 1 bijzondere soort` = sum(n_bijzonder > 0) / n(),
#             `Meer dan 1 bijzondere soort` = sum(n_bijzonder > 1) / n())
#
# fct_indicator <- c(#"Geen onderwaterplanten (0%)"        ,
#                    "Weinig onderwaterplanten (0%-5%)"      ,
#                    "Beperkt onderwaterplanten (5%-25%)",
#                    "Veel onderwaterplanten (> 25%)"        ,
#                    "Geen drijfbladplanten (0%)"       ,
#                    "Beperkt drijfbladplanten (0,1%-25%)"          ,
#                    "Veel drijfbladplanten (> 25%)"  ,
#                    "Weinig kroos (< 5%)"            ,
#                    "Beperkt kroos (5%-50%)"   ,
#                    "Veel kroos (> 50%)"            ,
#                    "Minstens 1 bijzondere soort"   ,
#                    "Meer dan 1 bijzondere soort"   )
#
#
# planten_toestand <-
#   planten_per_groep %>%
#   pivot_wider(names_from = bedekkingslaag, values_from = waarde) %>%
#   mutate(gebied = f_gebied(mp)) %>%
#   filter(gebied %in% c("Schieland", "Krimpenerwaard")) %>%
#   filter(jaar >= rap_jaar - 2) %>%
#   group_by(gebied) %>%
#   summarise(aantal_locs = n(),
#             `Weinig kroos (< 5%)`                = sum(kroos < 5)       / aantal_locs,
#             `Beperkt kroos (5%-50%)`       = sum(kroos >= 5 & kroos <= 50)  / aantal_locs,
#             `Veel kroos (> 50%)`                = sum(kroos > 50)      / aantal_locs,
#             `Geen drijfbladplanten (0%)`           = sum(drijfblad == 0)  / aantal_locs,
#             `Beperkt drijfbladplanten (0,1%-25%)`              = sum(drijfblad > 0 & drijfblad <= 25)   / aantal_locs,
#             `Veel drijfbladplanten (> 25%)`      = sum(drijfblad > 25)  / aantal_locs,
#             # `Geen onderwaterplanten (0%)`            = sum(submers == 0)    / aantal_locs,
#             `Weinig onderwaterplanten (0%-5%)`          = sum(submers >= 0 & submers <= 5)     / aantal_locs,
#             `Beperkt onderwaterplanten (5%-25%)` = sum(submers > 5 & submers <= 25) / aantal_locs,
#             `Veel onderwaterplanten (> 25%)`     = sum(submers > 25)    / aantal_locs) %>%
#   left_join(aantal_bijzonder, by = "gebied") %>%
#   select(-aantal_locs) %>%
#   pivot_longer(-c(gebied), names_to = "indicator", values_to = "percentage") %>%
#   mutate(groep = case_when(
#     str_detect(indicator, "kroos|Kroos") ~ "Kroos",
#     str_detect(indicator, "drijfblad|Drijfblad") ~ "Drijfblad",
#     str_detect(indicator, "onderwater|Onderwater") ~ "Onderwater",
#     str_detect(indicator, "bijzonder|Bijzonder") ~ "Bijzonder",
#     TRUE ~ NA_character_
#   )) %>%
#   mutate(groep = fct_relevel(groep, c("Onderwater", "Drijfblad", "Kroos", "Bijzonder"))) %>%
#   mutate(indicator = fct_relevel(indicator, fct_indicator))
#
# plot_toestand <-
#   planten_toestand %>%
#   ggplot(aes(percentage, fct_rev(indicator))) +
#   geom_col() +
#   facet_grid(cols = vars(gebied), rows = vars(groep), scales = "free", switch = "y", space = "free_y") +
#   # facet_wrap(vars(gebied, groep), scales = "free_y", switch = "y", ncol = 2) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, NA)), limits = c(0, NA)) +
#   labs(x = "",
#        y = "") +
#   thema_hor_bar +
#   theme(strip.placement = "outside",
#         axis.text.y = element_text(hjust = 1),
#         panel.spacing.y =  unit(15, "points"),
#         panel.spacing.x =  unit(40, "points"),
#         strip.switch.pad.grid = unit(15, "points"))
#
# plot_toestand <-
#   planten_toestand %>%
#   ggplot(aes(percentage, fct_rev(indicator), fill = gebied)) +
#   geom_col(position = position_dodge2(), width = 0.8) +
#   facet_grid(rows = vars(groep), scales = "free", switch = "y", space = "free_y") +
#   # facet_wrap(vars(gebied, groep), scales = "free_y", switch = "y", ncol = 2) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, NA)), limits = c(0, NA)) +
#   # scale_fill_manual() +
#   labs(y = "",
#        x = "% van alle wateren",
#        caption = glue("In de periode {rap_jaar-2}-{rap_jaar}")) +
#   thema_hor_bar +
#   theme(strip.placement = "outside",
#         axis.text.y = element_text(hjust = 1),
#         panel.spacing.y =  unit(15, "points"),
#         panel.spacing.x =  unit(40, "points"),
#         strip.switch.pad.grid = unit(15, "points"),
#         legend.position = "top",
#         legend.justification = c(0,0)) +
#   guides(fill = guide_legend(reverse = TRUE, title = ""))
#
# kaart_bijzonder <-
#   planten_basis %>%
#   group_by(mp, jaar, datum) %>%
#   summarise(n_bijzonder = sum(bijzonder, na.rm = TRUE)) %>%
#   mutate(gebied = f_gebied(mp)) %>%
#   filter(gebied %in% c("Schieland", "Krimpenerwaard")) %>%
#   filter(jaar >= rap_jaar - 2,
#          n_bijzonder > 0) %>%
#   left_join(meetpunten, by = "mp") %>%
#   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>%
#   ggplot() +
#   ggspatial::annotation_map_tile(type = "cartolight", zoomin = 0) +
#   geom_sf(data = ws_grens, fill = NA, colour = "grey60", size = 1) +
#   geom_sf(colour = hex(HLS(202.5, 0.25, 1)), size = 3) +
#   labs(title = "Locaties met tenminste één bijzondere soort",
#        caption = glue("In de periode {rap_jaar-2}-{rap_jaar}")) +
#   hhskthema_kaart() +
#   theme(axis.line = element_blank())

# ggplotly(x)


