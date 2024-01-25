library(tidyverse)
library(readxl)
theme_set(HHSKwkl::hhskthema())
library(HHSKwkl)
library(glue)

doelen <-
  readxl::read_excel("data/waterlichamen_ekrs_en_doelen_2022.xlsx") %>% 
  select(type, naam = OWMNAAM, doelen, groep)


ekrs <- 
  read_excel("data/overzicht ekr nieuwe toetsing 2023 v24-7-2023.xlsx") %>% 
  pivot_longer(cols = starts_with("20"), names_to = "jaar", values_to = "ekr", values_drop_na = TRUE) %>% 
  rename_all(str_to_lower) %>%
  mutate(jaar = as.numeric(jaar)) %>% 
  arrange(type, nr, naam, jaar) %>% 
  group_by(type, nr, naam) %>% 
  mutate(ekr3 = slider::slide_dbl(ekr, ~mean(.x), .before = 2)) %>% 
  ungroup() %>% 
  left_join(doelen) %>% 
  mutate(type = fct_relevel(type, c("Algen", "Waterplanten", "Macrofauna", "Vis"))) %>% 
  mutate(doelbereik = pmin(ekr / doelen, 1),
         doelbereik3 = pmin(ekr3 / doelen, 1),
         doelgat = 1 - doelbereik,
         doelgat3 = 1 - doelbereik3)


  
  

ekrs %>% 
  filter(naam == "Rotteboezem", type == "Algen") %>% 
  ggplot(aes(jaar, ekr)) + geom_line() + geom_point() + geom_line(aes(y = ekr3))


ekrs %>% 
  filter(naam == "Kralingse Plas") %>%
  ggplot(aes(jaar, doelbereik3)) +
  geom_area(aes(y = 1), fill = oranje_m) +
  geom_area(fill = blauw_m) +
  geom_line(colour = blauw) + geom_point(colour = blauw) +
  facet_grid(rows = vars(type)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)))

ekrs %>% 
  group_by(type, naam) %>% 
  filter(jaar == max(jaar)) %>% 
  filter(!is.na(doelbereik3)) %>% 
  group_by(groep, naam) %>% 
  summarise(gem_doelbereik = mean(doelbereik3, na.rm = TRUE)) %>% 
  ggplot(aes(gem_doelbereik, naam)) +
  geom_col(aes(x = 1), fill = oranje_m) +
  geom_col(fill = blauw_m) +
  facet_grid(rows = vars(groep),
             scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(16)) +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
        strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "plot")
  
ekrs %>% 
  group_by(type, naam) %>% 
  filter(jaar == max(jaar)) %>% 
  filter(!is.na(doelbereik3)) %>% 
  group_by(type) %>% 
  summarise(gem_doelbereik = mean(doelbereik3, na.rm = TRUE)) %>% 
  ggplot(aes(gem_doelbereik, type)) +
  geom_col(aes(x = 1), fill = oranje_m) +
  geom_col(fill = blauw_m) +
  # facet_grid(rows = vars(groep), cols = vars(type),
  #            scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(16)) +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
        strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "plot")
  
ekrs %>% 
  group_by(type, naam) %>% 
  filter(jaar == max(jaar)) %>% 
  filter(!is.na(doelbereik3)) %>% 
  group_by(groep, type) %>% 
  summarise(gem_doelbereik = mean(doelbereik3, na.rm = TRUE)) %>% 
  ggplot(aes(gem_doelbereik, groep)) +
  geom_col(aes(x = 1), fill = oranje_m) +
  geom_col(fill = blauw_m) +
  facet_grid(#rows = vars(groep), 
             cols = vars(type),
             scales = "free_y", space = "free_y", switch = "y", labeller = label_wrap_gen(16)) +
  theme(panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
        strip.placement = "outside",
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y =  element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top",
        plot.title.position = "plot")

waterlichamen <- ekrs %>% pull(naam) %>% unique()

pdf("J:/desktop/krw_ontwikkeling_ekrs_3_meetjaren.pdf",width = 7, height = 10)

for (wl_naam in waterlichamen) {
  data_temp <- 
    ekrs %>% 
    filter(naam == wl_naam) %>%
    select(jaar, doelbereik = doelbereik3, doelgat = doelgat3, type, naam) %>% 
    pivot_longer(cols = c(doelbereik, doelgat), names_to = "doelbereik_type", values_to = "fractie") 
  
  p <- 
  data_temp %>% 
    ggplot(aes(jaar, fractie, fill = fct_rev(doelbereik_type))) +
    geom_area() +
    geom_line(colour = blauw, data = filter(data_temp, doelbereik_type == "doelbereik"), show.legend = FALSE) + 
    geom_point(colour = blauw, data = filter(data_temp, doelbereik_type == "doelbereik"), show.legend = FALSE) +
    # facet_grid(rows = vars(type), scales = "free") +
    facet_wrap(~type, ncol = 1, scales = "free") +
    labs(title = glue("Ontwikkeling doelbereik {wl_naam}"),
         x = "",
         y = "Percentage doelbereik",
         caption = "Doelbereik op basis van de 3 meest recente meetjaren") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks(15), limits = c(2009, 2022)) +
    scale_fill_manual(values = c(doelbereik = blauw_m, doelgat = oranje_m), guide = guide_legend(title = "")) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.y =  element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top",
          plot.title.position = "plot") 
  
  print(p)
  p
}

dev.off()


pdf("J:/desktop/krw_ontwikkeling_ekrs.pdf",width = 7, height = 10)

for (wl_naam in waterlichamen) {
  data_temp <- 
    ekrs %>% 
    filter(naam == wl_naam) %>%
    select(jaar, doelbereik, doelgat, type, naam) %>% 
    pivot_longer(cols = c(doelbereik, doelgat), names_to = "doelbereik_type", values_to = "fractie") 
  
  p <- 
    data_temp %>% 
    ggplot(aes(jaar, fractie, fill = fct_rev(doelbereik_type))) +
    geom_area() +
    geom_line(colour = blauw, data = filter(data_temp, doelbereik_type == "doelbereik"), show.legend = FALSE) + 
    geom_point(colour = blauw, data = filter(data_temp, doelbereik_type == "doelbereik"), show.legend = FALSE) +
    # facet_grid(rows = vars(type), scales = "free") +
    facet_wrap(~type, ncol = 1, scales = "free") +
    labs(title = glue("Ontwikkeling doelbereik {wl_naam}"),
         x = "",
         y = "Percentage doelbereik",
         caption = "Doelbereik niet gemiddeld over 3 meest recente meetjaren") +
    scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), labels = scales::percent_format(accuracy = 1)) +
    scale_x_continuous(breaks = scales::pretty_breaks(15), limits = c(2009, 2022)) +
    scale_fill_manual(values = c(doelbereik = blauw_m, doelgat = oranje_m), guide = guide_legend(title = "")) +
    theme(panel.spacing = unit(1.5, "lines"),
          plot.margin = margin(5.5, 15, 5.5, 5.5, unit = "pt"),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major.y =  element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = "top",
          plot.title.position = "plot") 
  
  print(p)
  p
}

dev.off()
