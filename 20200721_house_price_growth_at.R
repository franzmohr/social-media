rm(list = ls())

#library(alfred)
library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)

country <- "AT"

# House prices
prices <- get_eurostat(id = "prc_hpi_q", filters = list(geo = country,
                                                     purchase = c("TOTAL"))) %>%
  mutate(date = ceiling_date(time, unit = "quarter") - 1) %>%
  #filter(!is.na(values)) %>%
  #mutate(date = as.yearqtr(time)) %>%
  pivot_wider(names_from = "unit", values_from = "values") %>%
  mutate(value = RCH_A / 100,
         name = "price") %>%
  select(date, name, value)

# Rents and inflation
rents <- get_eurostat(id = "prc_hicp_manr", filters = list(geo = country,
                                                         coicop = c("CP00", "CP041"))) %>%
  rename(name = coicop) %>%
  mutate(date = ceiling_date(time, unit = "month") - 1) %>%
  mutate(value = values / 100,
         name = ifelse(name == "CP00", "inflation", "rent")) %>%
  select(date, name, value)

earnings <- get_eurostat("earn_nt_net", filters = list(geo = country,
                                                       currency = "EUR",
                                                       ecase = "P1_NCH_AW100",
                                                       estruct = "GRS")) %>%
  arrange(time) %>%
  mutate(value = values / lag(values, 1) - 1,
         date = ceiling_date(time, "year") - 1,
         name = "income") %>%
  select(date, name, value)
  
temp <- bind_rows(prices, rents, earnings) %>%
  #filter(date >= Sys.Date() - 365 * 10) %>%
  filter(date >= "2013-12-31") %>%
  mutate(name_en = factor(name, levels = c("price", "rent", "inflation", "income"),
                          labels = c("House prices", "Rentals for housing", "Inflation", "Net income")),
         name_de = factor(name, levels = c("price", "rent", "inflation", "income"),
                          labels = c("Hauspreise", "Wohnungsmieten", "Inflation", "Bruttoeinkommen*")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name_de)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "Wohnimmobilienpreise (Österreich)",
       subtitle = "Jahreswachstum in Prozent",
       caption = "Quelle: Eurostat Hauspreis-, Inflations- und Einkommensstatistik. *Durchschnittliches Einkom-\nmen von Singles ohne Kinder. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200718_rre_price_growth_at_de.jpeg", height = 5, width = 5)

