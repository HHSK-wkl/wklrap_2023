library(trend)

# x <- grafiek_data %>% filter(parnr == 3, landgebruik == "glastuinbouw") %>% pull(waarde_gem)

# y <- sens.slope(x)

# str(y)

# glue("{round(y$estimates * 10, digits = 3)} mg P/l per 10 jaar" )

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



# grafiek_data %>%
#   group_by(parnr, landgebruik) %>%
#   nest() %>%
#   mutate(mod = map(data, ~lm(waarde_gem ~ jaar, data = .x)),
#          coef = map_dbl(mod, ~coef(.x)[2]),
#          lm_p = map_dbl(mod, ~summary(.x)$coefficients[2,4]))
#
#   mutate(decade = round(helling * 10, digits = 3)) %>%
#   mutate(kans =  1 / p_value) %>%
#   arrange(kans) %>%
#   filter(parnr == 7)
#
# sens.slope(c(5,4,3,2,1,
#              1,1,1,1,1,1))

