rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
min_date <- "2010-01-01"
date_title <- format(Sys.Date(), "%Y%m%d")

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry,
                                       coicop = c("FOOD", "NRG", "IGD_NNRG", "SERV")),
                        cache = FALSE)

weights <- weights %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry,
                                     coicop = c("CP00", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV")),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= min_date) %>%
  filter(!is.na(values)) %>%
  mutate(values = values / 100)

comp <- index %>%
  filter(!coicop %in% c("CP00", "TOT_X_NRG_FOOD")) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight),
         values = values * weight) %>%
  ungroup() %>%
  mutate(var_de = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Dienstleistungen", "Industriegüter ohne Energie",
                                    "Energie", "Nahrungsmittel")),
         var_en = factor(coicop, levels = c("SERV", "IGD_NNRG", "NRG", "FOOD"),
                         labels = c("Services", "Non-energy industrial goods",
                                    "Energy", "Food including alcohol and tobacco")))

line <- index %>%
  filter(coicop %in% c("CP00", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  mutate(line_de = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HVPI-Inflation", "Kerninflation (ohne Energie & Nahrungsmittel)")),
         line_en = factor(coicop, levels = c("CP00", "TOT_X_NRG_FOOD"),
                          labels = c("HCPI-inflation", "Core inflation (w/o energy & food)")))

source("theme_instagram.R")

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var_de), alpha = 1) +
  geom_line(data = line, aes(linetype = line_de), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "12 months", date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Beitrag zur Inflation (Österreich)", 
       subtitle = "Inflationsraten in %; Beiträge der Komponenten in Prozentpunkten",
       caption = "Quelle: Eurostat. Idee: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für\nÖsterreich 2020 bis 2023. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

ggsave(g, filename = paste0("pics/", date_title, "_infl_comp_de.jpeg"), height = 5, width = 5)


g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var_en), alpha = 1) +
  geom_line(data = line, aes(linetype = line_en), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "2 months", date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Contribution to inflation (Austria)", 
       subtitle = "Inflation in %; Contribution of component in percentage points",
       caption = "Source: Eurostat. Idea: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich\n2020 bis 2023. Code available at https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

ggsave(g, filename = paste0("pics/", date_title, "_infl_comp_en.jpeg"), height = 5, width = 5)

