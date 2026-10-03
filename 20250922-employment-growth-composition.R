rm(list = ls())

# Choose language
lang <- "en"

min_date <- "2015-01-01"

# Choose countries
ctry_eurostat <- c("AT", "DE", "EU27_2020", "EA20")


library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

ctry_levels <- c("AT", "DE", "EA20", "EU27_2020")


sectors <- c("A", "B-E", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U")


if (lang == "de") {
  ctry_labels <- c("Österreich", "Deutschland", "Euroraum", "Europäische Union")
  
  temp_title <- "Beschäftigung nach Sektor"
  temp_subtitle <- "Veränderung im Vergleich zum Vorjahreswert in tausend Personen"
  fig_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten. Letzter Wert: "
  sector_labels <- c("Land- und Forstwirtschaft,\nFischerei",
                     "Industrie\n(ohne Baugewerbe)",
                     "Baugewerbe/Bau",
                     "Handel, Instandhaltung,\nVerkehr, Gastgewerbe\nund Gastronomie",
                     "Information und\nKommunikation",
                     "Erbringung von Finanz-\nund Versicherungs-\ndienstleistungen",
                     "Grundstücks- und\nWohnungswesen",
                     "Erbringung freiberuflicher\nund sonstiger wirtschaft-\nlicher Dienstleistungen",
                     "Öffentliche Verwaltung,\nVerteidigung, Erziehung,\nGesundheits-/Sozialwesen",
                     "Kunst, Unterhaltung,\nsonst. Dienstleistungen,\npriv. Haushalte, exter-\nritoriale Organisationen")
  temp_other <- "Andere"
}

if (lang == "en") {
  ctry_labels <- c("Austria", "Germany", "Euro area", "European Union")
  
  temp_title <- "Employment by sector"
  temp_subtitle <- "Absolute change to previous year's value in thousand persons"
  fig_caption <- "Source: Eurostat. Seasonally and calendar adjusted data. Last value: "
  sector_labels <- c("Agriculture, forestry\nand fishing",
                     "Industry\n(except construction)",
                     "Construction",
                     "Wholesale and retail trade,\ntransport, accommodation and\nfood service activities",
                     "Information and\ncommunication",
                     "Financial and insurance\nactivities",
                     "Real estate activities",
                     "Professional, scientific and\ntechnical activities; administrative\nand support service activities",
                     "Public administration, defence,\neducation, human health and\nsocial work activities",
                     "Arts, entertainment and recreation;\nother service activities; activities\nof household and extra-territorial\norganizations and bodies")
  temp_other <- "Other"
}


temp <- get_eurostat(id = "namq_10_a10_e",
                     filters = list(geo = ctry_eurostat,
                                    na_item = "EMP_DC",
                                    s_adj = "SCA",
                                    unit = "THS_PER"),
                     cache = FALSE) %>%
  filter(!is.na(values),
         !nace_r2 %in% c("TOTAL", "C")) %>%
  rename(date = time,
         ctry = geo,
         sctr = nace_r2,
         value = values) %>%
  select(date, ctry, sctr, value) %>%
  arrange(date) %>%
  group_by(ctry, sctr) %>%
  mutate(value = value - lag(value, 4)) %>%
  ungroup() %>%
  filter(!is.na(value),
         date >= min_date) %>%
  mutate(sctr = factor(sctr, levels = sectors, labels = sector_labels),
         sctr = as.character(sctr))

top_sctr <- temp %>%
  filter(ctry == "AT") %>%
  filter(date == max(date)) %>%
  mutate(value = abs(value)) %>%
  arrange(desc(value)) %>%
  slice(1:5) %>%
  pull("sctr")

temp <- temp %>%
  mutate(sctr = ifelse(sctr %in% top_sctr, sctr, "other"),
         sctr = factor(sctr, levels = c(top_sctr, "other"),
                       labels = c(top_sctr, temp_other)),
         ctry = factor(ctry, levels = ctry_levels, labels = ctry_labels))

max_date <- format(as.yearqtr(max(temp$date)), "%YQ%q")
fig_caption <- paste0(fig_caption, max_date, ".")

g <- ggplot(temp, aes(x = date, y = value, fill = sctr)) +
  geom_col() +
  facet_wrap(~ ctry, scales = "free_y") +
  scale_x_date(expand = c(0, 0)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = fig_caption) +
  theme_instagram +
  scale_fill_insta

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-employment-growth-composition-", lang, ".jpeg"), height = 7, width = 7)

