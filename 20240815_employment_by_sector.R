
rm(list = ls())


# Choose language
lang <- "de"

# Choose countries
ctry_eurostat <- "AT"
ctry <- "Österreich"

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


sectors <- c("A", "B-E", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U")

if (lang == "de") {
  temp_title <- "Wo arbeiten die Österreicher:innen?"
  temp_subtitle <- "Beschäftigung nach Sektor in tausend Personen"
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten. Code unter https://github.com/franzmohr/instagram."
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
}
if (lang == "en") {
  temp_title <- paste0("Employment by sector (", ctry, ")")
  temp_subtitle <- "Thousand persons"
  temp_caption <- "Source: Eurostat. Seasonally and calendar adjusted data. Code at https://github.com/franzmohr/instagram."
  sector_labels <- c("Agriculture, forestry and fishing",
                     "Industry (except construction)",
                     "Construction",
                     "Wholesale and retail trade, transport, accommodation and food service activities",
                     "Information and communication",
                     "Financial and insurance activities",
                     "Real estate activities", 
                     "Professional, scientific and technical activities; administrative and support service activities",
                     "Public administration, defence, education, human health and social work activities",
                     "Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies")
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
  mutate(sctr = factor(sctr, levels = sectors, labels = sector_labels))

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp, aes(x = date, y = value, fill = ctry)) +
  geom_area(show.legend = FALSE) +
  facet_wrap(~ sctr) +
  scale_x_date(expand = c(0, 0)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_fill_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))
g
date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "_employment_by_sector.jpeg"), height = 5, width = 5)
