rm(list = ls())

ctry <- "AT"

lang <- "de"

min_date <- "2017-01-01"


library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_franz.R")


# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PNX", "PX")

if (lang == "de") {
  var_labels <- c("Privater Konsum",
                  "Bruttoinvestitionen",
                  "Öffentlicher Konsum",
                  "Exporte", "Importe",
                  "Nettoexporte",
                  "Sonstige")
  temp_title <- "Zusammensetzung des realen BIP-Wachstums (Österreich)"
  temp_subtitle <- "Änderung in Prozentpunkten im Vergleich zum Vorjahreswert"
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten. Eigene Berechnungen."
}

if (lang == "en") {
  var_labels <- c("Private consumption",
                  "Gross investment",
                  "Public consumption",
                  "Exports", "Imports",
                  "Net exports",
                  "Other")
  temp_title <- "Contribution to real GDP growth (Austria)"
  temp_subtitle <- "Percentage point change compared to value in the previous year"
  temp_caption <- "Source: Eurostat. Seasonally and calendar adjusted data. Own calculations."
}


temp <- raw %>%
  select(date = time, na_item, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  mutate(PNX = P6 - P7) %>% # Net exports
  #select(-P6, -P7) %>% # Drop redundant columns
  na.omit() %>% # Drop empty rows
  mutate(PX = B1GQ - (P3_S13 + P31_S14_S15 + P5G + PNX),
         P7 = -P7,
         P3_S13 = P3_S13 - lag(P3_S13, 4),
         P31_S14_S15 = P31_S14_S15 - lag(P31_S14_S15, 4),
         P5G = P5G - lag(P5G, 4),
         PNX = PNX - lag(PNX, 4),
         P6 = P6 - lag(P6, 4),
         P7 = P7 - lag(P7, 4),
         PX = PX - lag(PX, 4),
         # Calculate contributions to total growth
         P3_S13 = P3_S13 / lag(B1GQ, 4),
         P31_S14_S15 = P31_S14_S15 / lag(B1GQ, 4),
         P5G = P5G / lag(B1GQ, 4),
         PNX = PNX / lag(B1GQ, 4),
         P6 = P6 / lag(B1GQ, 4),
         P7 = P7 / lag(B1GQ, 4),
         PX = PX / lag(B1GQ, 4)) %>%
  select(-B1GQ, -PNX) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(date >= min_date) %>%
  filter(!is.na(value)) %>%
  mutate(var = factor(name, levels = var_levels,
                      labels = var_labels))

real_agg <- temp %>%
  filter(na_item == "B1GQ") %>%
  mutate(values = values / lag(values, 4) - 1) %>%
  filter(date >= min_date) %>%
  filter(!is.na(values)) %>%
  rename(value = values)

last_value <- format(as.yearqtr(max(real$date)), "%YQ%q")
if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last value: ", last_value, ".")
}

g <- ggplot(real, aes(x = date, y = value)) +
  geom_zeroline() +
  geom_col(aes(fill = var), alpha = 1) +
  geom_line(data = real_agg, aes(colour = "Gesamt"), linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = "black") +
  scale_fill_franz() +
  guides(fill = guide_legend(nrow = 2)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank())

g
save_post(g, "gdp-components-growth", lang = lang, format = "portrait")
