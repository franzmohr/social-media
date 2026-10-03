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

source("r-corporate-design-functions-ggplot2.R")

raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = "MIO_EUR"),
                    cache = FALSE)

if (lang == "de") {
  var_names <- c("Allgemeine öffentliche\nVerwaltung", "Verteidigung", "Öffentliche Ordnung\nund Sicherheit",
                 "Wirtschaftliche Angelegenheiten", "Umweltschutz", "Wohnungswesen und\nkommunale Einrichtungen",
                 "Gesundheitswesen", "Freizeitgestaltung,\nKultur und Religion", "Bildungswesen", "Soziale Sicherung")
  temp_sector <- c("Staat", "Zentralstaat", "Länder", "Gemeinden", "Sozialversicherung")
  temp_y <- "Mrd EUR"
  temp_title <- "Ausgaben des Staates nach Aufgabenbereichen (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
}

if (lang == "en") {
  var_names <- c("General public services", "Defence", "Public order and safety",
                 "Economic affairs", "Environmental protection", "Housing and community amenities",
                 "Health", "Recreation, culture and religion", "Education", "Social protection")
  temp_sector <- c("General government", "Central government", "State government",
                   "Local government", "Social security funds")
  temp_y <- "Bn EUR"
  temp_title <- "General government expenditure by function (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
}

var_levels <- c("GF01", "GF02", "GF03", "GF04", "GF05", "GF06", "GF07", "GF08", "GF09", "GF10")
sector_levels <- c("S13", "S1311", "S1312", "S1313", "S1314")

temp <- raw %>%
  rename(var = cofog99) %>%
  filter(!is.na(values),
         nchar(var) == 4) %>%
  mutate(var = factor(var, levels = var_levels, labels = var_names),
         sector = factor(sector, levels = sector_levels, labels = temp_sector),
         values = values / 1000)

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
}

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "5 years") +
  facet_wrap(~var, ncol = 3) +
  labs(title = temp_title,
       subtitle = temp_y,
       caption = temp_caption) +
  theme_corporate_design(base_size = 13) +
  theme(legend.position="bottom", legend.box = "vertical") +
  theme(axis.title = element_blank())
  
g
save_chart(g, "government-expenditures-overview", lang = lang, format = "portrait")

