rm(list = ls())

# Choose language
lang <- "de" # "de" or "en"

# Choose country
ctry <- "AT"

# Load packages
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

source("theme_franz.R")

raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = "MIO_EUR"),
                    cache = FALSE) %>%
  filter(!is.na(values))


if (lang == "de") {
  var_names <- c("Allgemeine Angelegenheiten\nder Wirtschaft und\ndes Arbeitsmarkts",
                 "Land- und Forstwirtschaft,\nFischerei und Jagd", "Brennstoffe und Energie",
                 "Bergbau, Herstellung von\nWaren und Bauwesen", "Verkehr",
                 "Nachrichtenübermittlung", "Andere Wirtschaftsbereiche",
                 "Angewandte Forschung und\nexperimentelle Entwicklung",
                 "Sonstige wirtschaftliche\nAngelegenheiten")
  temp_y <- "Mrd EUR"
  temp_title <- "Ausgaben des Staates für wirtschaftliche Angelegenheiten (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
}

if (lang == "en") {
  var_names <- c("General economic, commercial and labour affairs",
                 "Agriculture, forestry, fishing and hunting",
                 "Fuel and energy", "Mining, manufacturing and construction",
                 "Transport", "Communication", "Other industries", "R&D Economic affairs",
                 "Other economic affairs")
  temp_sector <- c("General government", "Central government", "State government",
                   "Local government", "Social security funds")
  temp_y <- "Bn EUR"
  temp_title <- "General government expenditure for economic affairs (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
}

var_levels <- c("GF0401", "GF0402", "GF0403", "GF0404", "GF0405",
                "GF0406", "GF0407", "GF0408", "GF0409")

temp <- raw %>%
  rename(var = cofog99) %>%
  filter(!is.na(values),
         substring(var, 1, 4) == "GF04",
         var != "GF04") %>%
  mutate(var = factor(var, levels = var_levels, labels = var_names),
         values = values / 1000)

# temp %>%
#   select(time, var, values) %>%
#   pivot_wider(names_from = "var", values_from = "values") %>%
#   tail()

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
}

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0)) +
  facet_wrap(~var, ncol = 3) +
  labs(title = temp_title,
       subtitle = temp_y,
       caption = temp_caption) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  theme_franz(base_size = 13) +
  theme(legend.position="bottom", legend.box = "vertical")

g
save_post(g, "government-expenditures-economy", lang = lang, format = "portrait")

