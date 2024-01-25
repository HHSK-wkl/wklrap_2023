library(HHSKwkl)
library(tidyverse)
library(leaflet)
library(glue)

theme_set(hhskthema())  

ws_grens <- sf::st_read("data/ws_grens.gpkg", crs = 28992) %>% sf::st_transform(crs = 4326)

fys_chem <- readRDS("data/fys_chem.rds")
meetpunten <- import_meetpunten("data/meetpunten.csv")
parameters <- import_parameters("data/parameters.csv")
pfas_rpf <- readxl::read_excel("data/rpf_pfas.xlsx")

# normen_pfas <-
#   read_tsv("data/PFASnormen.txt", locale = locale(decimal_mark = ",")) %>% 
#   mutate(normwaarde = ifelse(Eenheid == "µg/l", Waarde * 1000, Waarde),
#          normeenheid = "ng/l") %>% 
#   select(Stofnaam, aquo_parcode = "Aquo-code", normwaarde, normeenheid, normtype = Norm.code) %>% 
#   distinct()

risicogrenzen_rivm <- readxl::read_excel("data/overige_pfasnormen.xlsx") %>% 
  filter(normtype == "voorstel_RIVM") %>% 
  mutate(aquo_parcode = case_when(
    aquo_parcode == "PFOS"    ~ "slinvertPFOS",
    aquo_parcode == "PFOA"    ~ "slinvertPFOA",
    aquo_parcode == "L_PFHxS" ~ "slinverPFHxS",
    TRUE ~ aquo_parcode
  ))
  

# wl_namen <- tibble::tribble(
#        ~mp,                  ~waterlichaam,
#   "K_0108",        "Krimpen aan de IJssel",
#   "K_0209",            "Natuurgebied Zuid",
#   "K_0222",        "Kromme, Geer en Zijde",
#   "K_0309",          "Natuurgebied Midden",
#   "K_0427",                     "Stolwijk",
#   "K_0474", "Sloten waterrijk Berkenwoude",
#   "K_0801",                  "Bergambacht",
#   "K_1035",         "Den Hoek en Schuwagt",
#   "K_1405",            "Natuurgebied Oost",
#   "S_0010",                  "Rotteboezem",
#   "S_0030",               "Bergse Plassen",
#   "S_0040",              "Zevenhuizerplas",
#   "S_0041",              "Hoge Bergse Bos",
#   "S_0054",              "Vaart Bleiswijk",
#   "S_0060",                    "Ringvaart",
#   "S_0067",               "Kralingse Plas",
#   "S_0125",             "Bleiswijkse Zoom",
#   "S_0130",              "Lage Bergse Bos",
#   "S_0144",              "Eendragtspolder",
#   "S_0201",           "Binnenwegse Polder",
#   "S_0601",          "Zuidplaspolder Zuid",
#   "S_0609",         "Zuidplaspolder Noord",
#   "S_0705",                    "'t Weegje",
#   "S_0801",       "Polder Prins Alexander",
#   "S_0910",         "Sloten Waterrijk EGB",
#   "S_1201",             "Polder Bleiswijk"
# ) %>% 
#   maak_opzoeker()

pfas <- 
  fys_chem %>% 
  add_jaar_maand() %>% 
  filter(jaar == 2023) %>% 
  inner_join(filter(parameters, cluster == "PFAS")) %>% 
  mutate(par = str_replace(par, "FRD-903", "GenX"))

# pfas %>% filter(is.na(detectiegrens), !str_detect(par, "slinver")) %>% 
#   count(mp, datum) %>% View()

# plot_pfas_normen <- 
# pfas %>% 
#   filter(!str_detect(mp, "ADHOC|S_0075|S_1120|S_1124|S_1149|S_1133|K_1102")) %>% 
#   inner_join(filter(normen_pfas, normtype == "JG-MKN"), by = "aquo_parcode") %>% 
#   mutate(normfactor = waarde / normwaarde) %>% 
# 
#   mutate(Locatie = fct_reorder(wl_namen(mp), normfactor, .fun = max)) %>% 
#   mutate(par = fct_reorder(par, normfactor, .fun = max, .desc = TRUE)) %>% 
#   ggplot(aes(normfactor, Locatie)) + 
#   geom_col() +
#   geom_vline(xintercept = 1, linetype = "dashed", colour = oranje, linewidth = 1) +
#   scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), breaks = scales::pretty_breaks(6)) +
#   facet_wrap(~par) +
#   labs(title = "PFAS t.o.v. de norm voor het jaargemiddelde",
#        x = "Overschrijdingsfactor van de jaargemiddelde norm",
#        y = "Locatie") +
#   theme(panel.spacing.x = unit(15, "points"),
#         panel.grid.major.y = element_blank(),
#         plot.title.position = "plot")

pfas_eq <-
  pfas %>% 
  # filter(!str_detect(mp, "ADHOC|S_0075|S_1120|S_1124|S_1149|S_1133")) %>% 
  left_join(pfas_rpf, by = "aquo_parcode") %>% 
  mutate(pfoa_equivalent = waarde * rpf, .after = waarde) %>% 
  summarise(pfoa_equivalent = sum(pfoa_equivalent, na.rm = TRUE), .by = c(mp, datum)) %>%
  summarise(pfoa_equivalent = mean(pfoa_equivalent, na.rm = TRUE), .by = mp) %>% # gemiddelde van locs met meer metingen
  mutate(pfoa_eq_radius = case_when(
    pfoa_equivalent < 50 ~ 7,
    pfoa_equivalent < 100 ~ 7,
    pfoa_equivalent < 200 ~ 9,
    pfoa_equivalent < 1000 ~ 9,
    pfoa_equivalent < 2000 ~ 9,
    pfoa_equivalent < 10000 ~ 9,
  )) %>% 
  left_join(meetpunten)

# pal <- colorFactor("Reds", levels = levels(pfas_eq$pfoa_eq_klasse), ordered = TRUE)
pal <- colorBin(RColorBrewer::brewer.pal(8, "Reds")[3:8], bins = c(20,50,100,200,1000,2000,10000))

kaart_pfoa_eq <-
  pfas_eq %>% 
  arrange(pfoa_equivalent) %>% 
  sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  sf::st_transform(crs = 4326) %>% 
  basiskaart(type = "cartolight") %>%
  addPolylines(data = ws_grens, color = "#616161", weight = 3, label = ~"waterschapsgrens") %>%
  addCircleMarkers(label = ~glue("{signif(pfoa_equivalent, digits = 2)} ng PEQ/l"), 
                   radius = ~pfoa_eq_radius, weight = 1, 
                   fillOpacity = 1, fillColor = ~pal(pfoa_equivalent), 
                   opacity = 1, color = "#616161") %>% 
  addLegend(pal = pal, values = ~pfoa_equivalent, opacity = 1, title = "PFOA-equivalenten",
            labFormat = labelFormat(suffix = " ng PEQ/l", big.mark = ".")) %>% 
  leaflet.extras::addFullscreenControl()

# JT Vanaf hier uitzoeken hoe risicogrenzen handig weer te geven als een dotplot. Labels met plotly werkt niet lekker
  
# cat <- tibble::tribble(
#                 ~mp,             ~categorie,
#            "S_0144",                  "krw",
#            "S_0130",                  "krw",
#            "K_0801",                  "krw",
#            "S_0040",                  "krw",
#            "K_0427",                  "krw",
#            "K_1035",                  "krw",
#            "K_0209",                  "krw",
#            "K_0222",                  "krw",
#            "S_0125",                  "krw",
#            "K_0108",                  "krw",
#            "K_0309",                  "krw",
#            "K_0474",                  "krw",
#            "S_0609",                  "krw",
#            "S_0060",                  "krw",
#            "K_1102",            "zwemwater",
#            "S_0705",                  "krw",
#            "K_1405",                  "krw",
#            "S_1201",                  "krw",
#            "S_0601",                  "krw",
#            "S_0010",                  "krw",
#            "S_0910",                  "krw",
#            "S_0054",                  "krw",
#            "S_1149", "aanvullend onderzoek",
#    "ADHOC_23_KE_01", "aanvullend onderzoek",
#            "S_0801",                  "krw",
#            "S_0201",                  "krw",
#            "S_0041",                  "krw",
#            "S_0067",                  "krw",
#            "S_0030",                  "krw",
#            "S_1120",            "zwemwater",
#            "S_1133", "aanvullend onderzoek",
#            "S_0075", "aanvullend onderzoek",
#   "ADHOC_23_ZWP_01", "aanvullend onderzoek",
#            "S_1124",            "zwemwater",
#    "ADHOC_23_KE_05", "aanvullend onderzoek",
#    "ADHOC_23_KE_03", "aanvullend onderzoek",
#    "ADHOC_23_KE_02", "aanvullend onderzoek",
#    "ADHOC_23_KE_04", "aanvullend onderzoek"
#   )


plot_grenswaarden <- 
  pfas %>% 
  summarise(waarde = mean(waarde), .by = c(mp, aquo_parcode)) %>% 
  inner_join(risicogrenzen_rivm, by = "aquo_parcode") %>% 
  summarise(risico_tot = sum(waarde / normwaarde), 
            .by = mp) %>%
  # left_join(cat) %>% View()
  ggplot(aes(x = risico_tot)) + 
  # geom_hline(yintercept = 0, colour = grijs_m) +
  # geom_jitter(width = 0, height = 0.05) +
  geom_dotplot(dotsize = 1, binwidth = 0.05, binaxis='x', fill = oranje) +
  scale_x_log10(labels = scales::label_number(big.mark = ".", suffix = ""), breaks = scales::breaks_log(12), limits = c(500, NA)) +
  # scale_y_continuous(limits = c(0, 0.5), expand = expansion(c(0, 0.1))) +
  labs(title = "PFAS t.o.v. de nieuwe risicogrenzen",
       x = "Aantal keer de risicogrenswaarde (logaritmisch)") +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") 


# zwemlocaties ------------------------------------------------------------

zwem_namen <- tibble::tribble(
  ~mp,                        ~Zwemlocatie,
  "S_0040",              "Zevenhuizerplas",
  "S_0067",               "Kralingse Plas",
  "S_0125",             "Bleiswijkse Zoom",
  "S_0144",              "Eendragtspolder",
  "S_1120",             "'t Zwarte Plasje",
  "S_1124",             "Kralings Zwembad",
  "K_1102",                "Krimpenerhout"
) %>% 
  maak_opzoeker()

plot_zwem_pfas <- 
  pfas_eq %>% 
  mutate(Zwemlocatie = zwem_namen(mp)) %>% 
  filter(!is.na(Zwemlocatie)) %>% 
  select(Zwemlocatie, pfoa_equivalent) %>% 
  mutate(frac_tdi = pfoa_equivalent / 849) %>% 
  mutate(Zwemlocatie = fct_reorder(Zwemlocatie, frac_tdi, .fun = max)) %>% 
  ggplot(aes(frac_tdi, Zwemlocatie)) + 
  geom_col() +
  scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), 
                     labels = scales::label_percent(), position = "top",
                     breaks = scales::pretty_breaks(7),
                     sec.axis = sec_axis(trans = \(waarde) waarde * 849, name = "PFOA-equivalenten (ng/l)")) +
  labs(title = "Risico van PFAS voor zwemmen",
       x = "Percentage van de toelaatbare dagelijkse inname",
       y = "",
       caption = "Volgens het scenario van een kind van 15,7 kg dat 25 keer per jaar zwemt en per keer 0,17 l binnenkrijgt.") +
  theme(panel.spacing.x = unit(15, "points"),
        panel.grid.major.y = element_blank(),
        plot.title.position = "plot",
        axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank())


# vrouw 7452
# man 5072
  
  
  # 
  # mutate(fact = waarde / normwaarde) %>% 
  # mutate(Locatie = reorder(wl_namen(mp), fact)) %>%
  # mutate(par = reorder(par, fact)) %>% 
  # filter(is.na(detectiegrens)) %>% 
  # group_by(par) %>% 
  # filter(any(fact > 1)) %>%
  # ungroup() %>% 
  # # group_by(mp) %>% 
  # # summarise(factor = sum(factor)) %>% 
  # # ungroup() %>% 
  # ggplot(aes(fact, Locatie, fill = par)) + 
  # geom_col(colour = "grey50", linewidth = 0.2) +
  # scale_fill_brewer(palette = "Set3") +
  # # scale_fill_viridis_d() +
  # scale_x_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)), breaks = scales::pretty_breaks(12), position = "top") +
  # labs(title = "PFAS-concentratie t.o.v. voorgestelde risicogrenswaarden",
  #      y = "Locatie",
  #      x = "Opgetelde overschrijdingsfactor") +
  # guides(fill = guide_legend(title = "Stof", reverse = TRUE)) +
  # theme(panel.grid.major.y = element_blank(),
  #       plot.title.position = "plot")
