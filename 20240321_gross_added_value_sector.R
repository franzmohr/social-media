rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
min_date <- "2010-01-01"
date_title <- format(Sys.Date(), "%Y%m%d")

# Download data
raw <- get_eurostat(id = "namq_10_a10",
                    filters = list(geo = ctry,
                                   na_item = "B1G",
                                   nace_r2 = c("A", "B-E", "C", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U"),
                                   unit = c("CLV15_MEUR"),
                                   s_adj = c("SCA")),
                    cache = FALSE)

var_levels <- c("A", "B-E", "C", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U")
var_labels_de <- c("Land- und Forstwirtschaft,\nFischerei",
                   "Industrie\n(ohne Baugewerbe)",
                   "Verarbeitendes Gewerbe/\nHerstellung von Waren",
                   "Baugewerbe/Bau",
                   "Handel, Instandhaltung,\nVerkehr, Gastgewerbe",
                   "Information und\nKommunikation",
                   "Finanz- und Versicherungs-\ndienstleistungen",
                   "Grundstücks- und\nWohnungswesen",
                   "Erbringung von\nDienstleistungen",
                   "Öffentliche Verwaltung,\nVerteidigung, Erziehung und\nUnterricht, Gesundheits-\nund Sozialwesen",
                   "Kunst/Unterhaltung/Erholung;\nSonstige Dienstleistungen;\nPrivate Haushalte,\nexterritoriale Organisationen")
# var_labels_en <- c(c("Gross capital formation",
#                      "Changes in inventories",
#                      "Gross fixed capital formation",
#                      "Aquisitions and disposals of valuables"))

temp <- raw %>%
  mutate(date = as.yearqtr(time, "%Y-Q%q")) %>%
  select(date, na_item = nace_r2, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  na.omit() %>% # Drop empty rows
  pivot_longer(cols = -c("date")) %>%
  filter(date >= min_date) %>%
  filter(!is.na(value)) %>%
  mutate(name_de = factor(name, levels = var_levels, labels = var_labels_de),
         name_en = name, # factor(name, levels = var_levels, labels = var_labels_en),
         value = value / 1000)

source("theme_instagram.R")

g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_de), alpha = 1, show.legend = FALSE) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  facet_wrap(~ name_de) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Reale Bruttowertschöpfung nach Sektor (Österreich)",
       subtitle = "Mrd EUR (2015 Preise, Quartalswerte)",
       caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

ggsave(g, filename = paste0("pics/", date_title, "_gross_value_added_sector.jpeg"), height = 5, width = 5)
# 
# 
# g <- ggplot(real, aes(x = date, y = value)) +
#   geom_col(aes(fill = name_en), alpha = 1) +
#   scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_fill_insta +
#   guides(fill = guide_legend(ncol = 2)) +
#   labs(title = "Investment (Austria)",
#        subtitle = "Bn EUR (current prices, quarterly data)",
#        caption = "Source: Eurostat. Seasonally and calendar adjusted data.\nCode available at https://github.com/franzmohr/instagram.") +
#   theme_instagram
# 
# ggsave(g, filename = paste0("pics/", date_title, "_invest_component_level_en.jpeg"), height = 5, width = 5)
# 
