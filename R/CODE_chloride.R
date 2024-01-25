source("R/CODE_index_setup.R")

# start()
#
# rap_jaar <-  2019

# hoogte van de max-waarde
# landgebruik
# herkomst chloride

## ---- chloride-1
# layout_ggplotly <- function(gg, x = -0.05, y = -0.08){
#   # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
#   gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
#   gg
# }

fractie_fun <- function(x, y){
  paste0(x, " van de ", y, " meetpunten (", scales::percent(x / y, accuracy = 0.1, decimal.mark = ","), ")")
}


data_cl <- fys_chem %>%
  add_jaar() %>%
  filter(jaar > rap_jaar - 10, jaar <= rap_jaar, parnr == 1) %>%
  left_join(meetpunten, by = "mp") %>%
  filter(meetpunttypering == 1, gebied %in% c("SCHIEL", "KRIWA")) %>%
  group_by(mp, jaar) %>%
  filter(waarde == max(waarde)) %>%
  filter(datum == min(datum)) %>%
  ungroup() %>%
  mutate(cl_klasse = cut(waarde, c(0,200,400,99999), labels = c("0 - 200 mg/l", "200 - 400 mg/l", "> 400 mg/l"), ordered_result = TRUE)) %>%
  mutate(Chloride = fct_rev(cl_klasse)) %>%
  group_by(jaar, gebied) %>% mutate(aantal_gebied = n()) %>%
  group_by(jaar, gebied, cl_klasse) %>% mutate(aantal_gebied_klasse = n()) %>%
  mutate(text = fractie_fun(aantal_gebied_klasse, aantal_gebied))




# data_cl %>% ggplot(aes(as.factor(jaar), fill = fct_rev(cl_klasse))) + geom_bar() + facet_wrap(~`landgebruik 2015`)
# data_cl %>% ggplot(aes(as.factor(jaar), fill = fct_rev(cl_klasse))) + geom_bar(position = "fill") + facet_wrap(~gebiednaam) + coord_flip() + scale_fill_brewer(palette = "OrRd", direction = -1)
# data_cl %>% filter(jaar == rap_jaar) %>% ggplot(aes(`landgebruik 2015`, fill = fct_rev(cl_klasse))) + geom_bar(position = "fill") + coord_flip()
# data_cl %>% filter(jaar == 2018, waarde > 400) %>% View()
#


# data_cl %>% ggplot(aes(as.factor(jaar), fill = fct_rev(cl_klasse))) +
#   geom_bar(position = "fill") +
#   facet_wrap(~gebiednaam) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Reds", direction = -1)
#
# data_cl %>% ggplot(aes(as.factor(jaar), fill = fct_rev(cl_klasse))) +
#   geom_bar(position = "fill") +
#   facet_wrap(~gebiednaam) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Reds", direction = -1)

pal <- colorRampPalette(c(hhskblauw, colorspace::lighten(hhskblauw, amount = 0.7)))

# grafiek_chloride <- data_cl %>% ggplot(aes(as.factor(jaar), fill = fct_rev(cl_klasse))) +
#   geom_bar(position = "fill") +
#   facet_wrap(~fct_rev(gebiednaam)) +
#   coord_flip() + hhskthema() +
#   scale_fill_manual(values = pal(3), guide = guide_legend(reverse = TRUE)) +
#   theme(panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.ticks.y = element_blank()) +
#   scale_y_continuous(labels = scales::percent) +
#   labs(y = "Fractie van het aantal meetpunten",
#        x = "",
#        fill = "Chloride")

aantal_cl <- data_cl %>% filter(jaar == rap_jaar, waarde > 400) %>% n_distinct()

titel_tekst <- paste0("Aantal meetpunten met meer dan 400 mg/l in ",
                      rap_jaar,
                      ': <span style="color:#0079c2;">',
                      aantal_cl,
                      "</span>")

grafiek_chloride <- data_cl %>%
  ggplot(aes(as.factor(paste0(jaar, "  ")), fill = Chloride, text = text)) +
  geom_bar(position = "fill") +
  facet_wrap(~fct_rev(gebiednaam)) +
  coord_flip() + hhskthema() +
  scale_fill_manual(values = c(oranje_m, oranje_l, blauw_l), guide = guide_legend(reverse = TRUE)) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0), limits = c(0, 1)) +
  labs(title = titel_tekst,
       x = "",
       y = "Fractie van het aantal meetpunten",
       fill = "Chloride") +
  thema_hor_bar +
  theme(axis.text.y = element_text(hjust = 0))

layout_ggplotly_cl <- function(gg, x = -0.05, y = -0.05){
  # The 1 and 2 goes into the list that contains the options for the x and y axis labels respectively
  gg[['x']][['layout']][['annotations']][[1]][['y']] <- x
  # gg[['x']][['layout']][['annotations']][[1]][['x']] <- y
  gg
}

grafiek_chloride %>%
  plotly::ggplotly(tooltip = c("fill", "text")) %>%
  plotly::config(displayModeBar = FALSE) %>%
  layout_ggplotly_cl() %>%
  layout(dragmode = FALSE)#%>% plotly_json()



# pal <- colorRampPalette(c("#0079c2", "#ccebff"))
# testplot + scale_fill_manual(values = pal(3))
#
# pal <- colorRampPalette(c(hhskblauw, colorspace::lighten(hhskblauw, amount = 0.7)))
# testplot + scale_fill_manual(values = pal(3), guide = guide_legend(reverse = TRUE))
# library(RColorBrewer)
#
# HHSKwkl::hhskblauw
