
rm(list = ls())


# Choose language
lang <- "de"

# Choose countries
hl_ctry <- "AT"

library(dplyr)
library(ecb)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


loans <- get_data(paste0("CBD2.A..W0.11.S11..A.F.A1100._X.ALL.GC._Z.LE._T.EUR")) %>%
  select(obstime, obsvalue, ref_area, bs_nfc_activity) %>%
  filter(!is.na(obsvalue)) %>%
  pivot_wider(names_from = "bs_nfc_activity", values_from = "obsvalue", values_fill = 0) %>%
  mutate(`B-E` = B + C + D,
         `G-I` = G + H + I,
         `M_N` = M + N,
         `O-Q` = O + P + Q) %>%
  select(obstime, ref_area, A, `B-E`, `F`, `G-I`, J, K, L, `M_N`, `O-Q`) %>%
  pivot_longer(cols = -c("obstime", "ref_area"), names_to = "sctr", values_to = "loans") %>%
  rename(date = obstime,
         ctry = ref_area) %>%
  mutate(ctry = ifelse(ctry == "U2", "EA20", ctry))

emp <- get_eurostat(id = "namq_10_a10_e",
                     filters = list(na_item = "EMP_DC",
                                    s_adj = "SCA",
                                    unit = "THS_PER"),
                     cache = FALSE) %>%
  filter(!is.na(values),
         !nace_r2 %in% c("TOTAL", "C"),
         substring(time, 6, 7) == "10") %>%
  rename(date = time,
         ctry = geo,
         sctr = nace_r2,
         emp = values) %>%
  select(date, ctry, sctr, emp) %>%
  mutate(date = substring(date, 1, 4))

temp <- full_join(emp, loans, by = c("date", "sctr", "ctry")) %>%
  mutate(share = loans / emp) %>%
  filter(!is.na(share)) %>%
  arrange(date, ctry) %>%
  mutate(date = as.Date(paste0(date, "-01-01")))

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

source("theme_instagram.R")

ggplot(temp, aes(x = date, y = share, colour = ctry)) +
  geom_line() +
  facet_wrap(~ sctr) +
  scale_x_date(expand = c(.01, 0)) +
  #labs(title = temp_title,
  #     subtitle = temp_subtitle,
  #     caption = temp_caption) +
  #scale_colour_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g
date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "_credit_to_employment_by_sector.jpeg"), height = 5, width = 5)
