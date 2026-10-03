rm(list = ls())

#library(alfred)
library(dplyr)
library(eurostat)
library(ggplot2)
library(oenb)
library(tidyr)
library(zoo)

# Download data
# eurostat_hh <- get_eurostat(id = "prc_hpi_q", filters = list(geo = "AT",
#                                                           purchase = c("TOTAL"))) %>%
#   #filter(!is.na(values)) %>%
#   mutate(date = as.yearqtr(time)) %>%
#   pivot_wider(names_from = "unit", values_from = "values") %>%
#   mutate(value = RCH_A / 100,
#          name = "eurostat") %>%
#   select(date, name, value)

eurostat_rent <- get_eurostat(id = "prc_hicp_manr",
                              filter = list(geo = "AT",
                                            coicop = "CP041")) %>%
  mutate(date = as.yearqtr(time)) %>%
  arrange(date) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarise(value = mean(values) / 100,
            name = "euro_rent",
            .groups = "drop")

oenb <- oenb_data(id = "6", pos = c("VDBPLIMOPATGEZBN", "VDBPLIMOPAT00", "VDBPLIMOPWIEN00"), freq = "Q") %>%
  arrange(period) %>%
  group_by(pos) %>%
  mutate(value = value / lag(value, 4) - 1) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  mutate(date = as.yearqtr(period, "%Y-Q%q")) %>%
  #filter(date >= "2015-01-01") %>%
  mutate(name = case_when(pos == "VDBPLIMOPATGEZBN" ~ "gesamt",
                          pos == "VDBPLIMOPAT00" ~ "wo_wien",
                          pos == "VDBPLIMOPWIEN00" ~ "wien")) %>%
  select(date, value, name)

temp <- bind_rows(eurostat_rent, oenb) %>%
  filter(date >= "2015 Q1") %>%
  filter(!is.na(value)) %>%
  #pivot_wider(names_from = "name", values_from = "value") %>%
  #pivot_longer(cols = -c("date")) %>%
  #filter(name %in% c("euro_rent", "gesamt")) %>%
  mutate(name_de = factor(name, levels = c("euro_rent", "gesamt", "wo_wien", "wien"),
                          labels = c("Miete - Österreich", "Eigentum - Österreich",
                                     "Eigentum - Österreich ohne Wien", "Eigentum - Wien")),
         name_en = factor(name, levels = c("euro_rent", "gesamt", "wo_wien", "wien"),
                          labels = c("Rents - Austria", "House prices - Austria",
                                     "House prices - Austria w/o Vienna", "House prices - Vienna")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name_de)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "Immobilienpreise (Österreich)",
       subtitle = "Jahreswachstum in Prozent",
       caption = "Quelle: Eurostat, OeNB. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20210221_house_price_growth_de.jpeg", height = 5, width = 5)

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
