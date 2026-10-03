rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
min_date <- "2010-01-01"
date_title <- format(Sys.Date(), "%Y%m%d")


rm(list = ls())

library(dplyr)
library(tidyr) 
library(lubridate)
library(readxl)

# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en


ec_folder <- "data/bronze/european_commission"


## Main indicators ----
ec_zip <- "data/bronze/european_commission/ec_survey_main.zip"

# Try download for current month
curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/main_indicators_sa_nace2.zip"),
                  destfile = ec_zip))
sdmx_files <- unzip(ec_zip, exdir = ec_folder)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/main_indicators_sa_nace2.zip"),
                    destfile = ec_zip))
  sdmx_files <- unzip(ec_zip, exdir = ec_folder)
}



## Consumer survey ----
ec_zip <- "data/bronze/european_commission/ec_survey_consumer.zip"

curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/consumer_total_sa_nace2.zip"),
                  destfile = ec_zip))
sdmx_files <- unzip(ec_zip, exdir = ec_folder)

if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/consumer_total_sa_nace2.zip"),
                    destfile = ec_zip))
  sdmx_files <- unzip(ec_zip, exdir = ec_folder)
}

## Construction survey ----
ec_zip <- "data/bronze/european_commission/ec_survey_construction.zip"

curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_total_sa_nace2.zip"),
                  destfile = ec_zip))
sdmx_files <- unzip(ec_zip, exdir = ec_folder)

if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_total_sa_nace2.zip"),
                    destfile = ec_zip))
  sdmx_files <- unzip(ec_zip, exdir = ec_folder)
}



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
                   "Kunst, Unterhaltung und Erholung;\n Erbringung von sonstigen Dienstleistungen; Private Haushalte, exterritoriale Organisationen und Körperschaften")
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
  labs(title = "Bruttowertschöpfung nach Sektor (Österreich)",
       subtitle = "Mrd EUR (2015 Preise, Quartalswerte)",
       caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

ggsave(g, filename = paste0("pics/", date_title, "_valueadded_level_growth_de.jpeg"), height = 5, width = 5)
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
