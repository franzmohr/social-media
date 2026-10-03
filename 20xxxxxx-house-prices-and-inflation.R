rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

# Inflation
inflation <- get_eurostat(id = "prc_hicp_midx",
                              filters = list(geo = "AT",
                                             unit = "I15",
                                            coicop = "CP00"),
                              cache = FALSE) %>%
  mutate(date = as.yearqtr(time)) %>%
  arrange(date) %>%#
  rename(ctry = geo) %>%
  group_by(date, ctry) %>%
  filter(n() == 3) %>%
  summarise(value = mean(values),
            name = "infl",
            .groups = "drop")

# House prices
hp <- get_data("RESR.Q.._T.N._TR.TVAL.4D0.TB.N.IX") %>%
  rename(ctry = ref_area,
         date = obstime,
         value = obsvalue) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.yearqtr(date, "%Y-Q%q")) %>%
  select(date, ctry, value) %>%
  mutate(name = "hp")


temp <- bind_rows(inflation, hp) %>%
  filter(date >= "2010 Q1") %>%
  filter(!is.na(value)) %>%
  arrange(date) %>%
  group_by(ctry) %>%
  filter(length(unique(name)) == 2) %>%
  group_by(name, ctry) %>%
  mutate(idx = value / value[1] * 100) %>%
  ungroup()

source("theme_instagram.R")

ggplot(temp, aes(x = date, y = idx, colour = name)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  guides(colour = guide_legend(ncol = 2)) +
  facet_wrap(~ ctry) +
  labs(title = "Immobilienpreise und Inflation in Österreich",
       subtitle = "Index (2005Q1 = 100)",
       caption = "Quelle: Eurostat, OeNB. Eigene Berechnungen. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "_house_prices_and_inflation_de.jpeg"), height = 5, width = 5)

# g <- ggplot(temp, aes(x = date, y = value, colour = name_en)) +
#   geom_hline(yintercept = 0, colour = "black") +
#   geom_line(size = 1.2) +
#   scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   guides(colour = guide_legend(ncol = 2)) +
#   labs(title = "Residential real estate prices (Austria)",
#        subtitle = "Annual growth in %",
#        caption = "Source: Eurostat, OeNB. Code available at https://github.com/franzmohr/instagram.") +
#   scale_colour_insta +
#   theme_instagram
# 
# ggsave(g, filename = "pics/20210214_house_price_growth_en.jpeg", height = 5, width = 5)
# 
