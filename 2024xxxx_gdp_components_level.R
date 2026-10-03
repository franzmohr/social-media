rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
min_date <- "1995-01-01"
date_title <- format(Sys.Date(), "%Y%m%d")

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PX")
var_labels_de <- c(c("Privater Konsum",
                     "Bruttoinvestitionen",
                     "Öffentlicher Konsum",
                     #"Nettoexporte",
                     "Exporte", "Importe",
                     "Sonstige"))
var_labels_en <- c(c("Private consumption",
                     "Gross investment",
                     "Public consumption",
                     #"Net exports",
                     "Exports", "Imports",
                     "Other"))

temp <- raw %>%
  mutate(date = as.yearqtr(time, "%Y-Q%q")) %>%
  select(date, na_item, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  #mutate(PNX = P6 - P7) %>% # Net exports
  #select(-P6, -P7) %>% # Drop redundant columns
  na.omit() %>% # Drop empty rows
  mutate(PX = B1GQ - (P3_S13 + P31_S14_S15 + P5G + P6 - P7), # Residual
         P7 = -P7) %>%
  select(-B1GQ) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(date >= min_date) %>%
  filter(!is.na(value)) %>%
  mutate(name_de = factor(name, levels = var_levels,
                          labels = var_labels_de),
         name_en = factor(name, levels = var_levels,
                          labels = var_labels_en),
         value = value / 1000)

source("theme_instagram.R")

g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_de), alpha = 1) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_colour_manual(values = "black") +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Reales BIP (Österreich)",
       subtitle = "Mrd EUR (2015 Preise, Quartalsdaten)",
       caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten.\nCode unter https://github.com/franzmohr/instagram.") +
  theme_instagram

g

ggsave(g, filename = paste0("pics/", date_title, "_gdp_level_growth_de.jpeg"), height = 5, width = 5)


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_en), alpha = 1) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Real GDP (Austria)",
       subtitle = "Bn EUR (2015 prices, quarterly data)",
       caption = "Source: Eurostat. Seasonally and calendar adjusted data.\nCode available at https://github.com/franzmohr/instagram.") +
  theme_instagram

ggsave(g, filename = paste0("pics/", date_title, "_gdp_component_level_en.jpeg"), height = 5, width = 5)

