rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(zoo)

ctry_code <- c("AT", "DE", "EU")
min_date <- "2010-01-01"
date_title <- format(Sys.Date(), "%Y%m%d")

# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en

## Main indicators ----
tfile <- tempfile(tmpdir = tdir <- tempdir())

# Try download for current month
curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_total_sa_nace2.zip"),
                  destfile = tfile))
sdmx_files <- unzip(tfile, exdir = tdir)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_total_sa_nace2.zip"),
                    destfile = tfile))
  sdmx_files <- unzip(tfile, exdir = tdir)
}

ecdata <- readxl::read_xlsx(sdmx_files,
                            sheet = "BUILDING MONTHLY", na = "NA", col_types = c("date", rep("numeric", 455))) %>%
  mutate(date = as.Date(`...1`)) %>%
  select(-`...1`) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(substring(name, 1, 3) != "...",
         grepl(".BS.", name),
         !is.na(value)) %>%
  mutate(ctry = substring(name, 6, 7),
         name = gsub(".BS.M", "", name),
         var = substring(name, 13, nchar(name))) %>%
  select(date, ctry, var, value) %>%
  filter(ctry %in% ctry_code)

# Combine ----

var_levels <- c("COF", 1, 3, 4, 5)

var_labels_de <- c("Allgemeine Stimmung",
                   "Entwicklung der Bautätigkeit\nin den letzten 3 Monaten",
                   "Aktuelle Auftragslage",
                   "Beschäftigslage\nin den nächtsen 3 Monaten",
                   "Veranschlagte Preise\nin den nächsten 3 Monaten")

var_labels_en <- c("Confidence Indicator",                            
                   "Building activity development over the past 3 months",
                   "Evolution of your current overall order books",
                   "Employment expectations over the next 3 months",
                   "Prices expectations over the next 3 months")

result <- ecdata %>%
  filter(var %in% var_levels) %>%
  filter(date >= min_date) %>%
  mutate(name_de = factor(var, levels = var_levels, labels = var_labels_de),
         name_en = factor(var, levels = var_levels, labels = var_labels_en))

source("theme_instagram.R")

g <- ggplot(result, aes(x = date, y = value)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = ctry)) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(~ name_de, ncol = 2) +
  labs(title = "Stimmungsindikatoren zum Bausektor",
       #subtitle = "Index",
       caption = "Quelle: Europäische Kommission. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

ggsave(g, filename = paste0("pics/", date_title, "_ecsurvey_build_main_de.jpeg"), height = 5, width = 5)

