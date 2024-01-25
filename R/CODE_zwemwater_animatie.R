source("R/CODE_index_setup.R")
source("R/CODE_zwemwater.R")

library(gganimate)

blauwe_tekst()
p <-
  waarschuwingen %>%
  complete(naam, jaar, fill = list(dagen = 0)) %>%
  mutate(naam = factor(naam)) %>%
  # filter(jaar > 2020) %>%
  # filter(jaar == 2017) %>%
  ggplot(aes(dagen, fct_rev(naam), fill = naam)) +
  geom_col(colour = "grey60", width = 0.8) +
  geom_text(aes(label = as.character(dagen)), colour = "grey60", nudge_x = 1, size = 5, fontface = "bold", hjust = 0) +
  scale_y_discrete(drop = FALSE) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), breaks = scales::pretty_breaks(10)) +
  labs(title = glue("Waarschuwingen vanwege blauwalg: {blauwe_tekst('{previous_state}')}"),
       # subtitle = "{previous_state}",
       x = "Aantal dagen",
       y = "") +
  scale_fill_discrete(guide = FALSE) +
  theme(plot.title = element_markdown(size = 20, margin = margin(b = 10)),
        plot.subtitle = element_text(size = 16, margin = margin(b = 18)),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 12),
        panel.grid.major.x = element_line(size = 0.8),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank())


p


p2 <- p + transition_states(factor(jaar), state_length = 3, wrap = FALSE) #shadow_mark(alpha = 0.05, color = "grey", exclude_layer = 2) # + shadow_mark()
# transition_states(jaar2)

p2 %>% animate(width = 720, nframes = 60, duration = 15)

p2 %>% animate(width = 720, nframes = 300, end_pause = 40, start_pause = 40)

anim_save("animaties/zwemwater_2012_2021.gif")
