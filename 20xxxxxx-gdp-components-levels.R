rm(list = ls())

ctry <- "AT"

lang <- "de"

min_date <- "2015-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_franz.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PNX", "PX")

if (lang == "de") {
  var_labels <- c("Privater Konsum",
                  "Bruttoinvestitionen",
                  "Öffentlicher Konsum",
                  "Exporte", "Importe (-)",
                  "Nettoexporte",
                  "Sonstige")
  temp_title <- "Zusammensetzung des realen BIPs (Österreich)"
  temp_subtitle <- "Mrd EUR (2015 Preise, Quartalswerte)"
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten."
}

if (lang == "en") {
  var_labels <- c("Private consumption",
                  "Gross investment",
                  "Public consumption",
                  "Exports", "Imports (-)",
                  "Net exports",
                  "Other")
  temp_title <- "Composition of real GDP (Austria)"
  temp_subtitle <- "Bn EUR (2015 prices, quarterly values)"
  temp_caption <- "Source: Eurostat. Seasonally and calendar adjusted data."
}

temp <- raw %>%
  select(date = time, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  mutate(PNX = P6 - P7) %>% # Net exports
  select(-PNX) %>%
  #select(-P6, -P7) %>% # Drop redundant columns
  na.omit() %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(var = factor(name, levels = var_levels,
                      labels = var_labels),
         value = value / 1000)

last_value <- format(as.yearqtr(max(temp$date)), "%YQ%q")
if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last value: ", last_value, ".")
}

max_value <- max(temp$value)

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(limits = c(0, max_value * 1.06), expand = c(0, 0)) +
  scale_fill_franz() +
  facet_wrap(~var, ncol = 2) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank())

g
save_post(g, "gdp-components-levels", lang = lang, format = "portrait")

