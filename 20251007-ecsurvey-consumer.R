
rm(list = ls())

lang <- "en"

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(zoo)

eu_ctry <- eu_countries$code

ctry_code <- c("AT", "EU")
min_date <- "2019-01-01"
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
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/consumer_total_sa_nace2.zip"),
                  destfile = tfile))
sdmx_files <- unzip(tfile, exdir = tdir)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/consumer_total_sa_nace2.zip"),
                    destfile = tfile))
  sdmx_files <- unzip(tfile, exdir = tdir)
}


ecdata_monthly <- readxl::read_xlsx(sdmx_files,
                              sheet = "CONSUMER MONTHLY", na = "NA", col_types = c("date", rep("numeric", 455))) %>%
  mutate(date = as.Date(`...1`)) %>%
  select(-`...1`) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(substring(name, 1, 3) != "...") %>%
  mutate(ctry = substring(name, 6, 7),
         name = gsub(".BS.M", "", name),
         var = substring(name, 13, nchar(name))) %>%
  filter(!is.na(value)) %>%
  select(date, ctry, var, value)

ecdata_quarterly <- readxl::read_xlsx(sdmx_files, sheet = "CONSUMER QUARTERLY", na = "NA") %>%
  mutate(date = as.Date(as.yearqtr(`...1`, "%Y-Q%q"))) %>%
  select(-`...1`) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(substring(name, 1, 3) != "...") %>%
  mutate(ctry = substring(name, 6, 7),
         name = gsub(".BS.Q", "", name),
         var = substring(name, 13, nchar(name))) %>%
  filter(!is.na(value)) %>%
  select(date, ctry, var, value)

ecdata <- bind_rows(ecdata_monthly, ecdata_quarterly)

# Combine ----

var_levels <- c(1, 2, 3, 4, 8, 9, 14, 15, 11, 12, 7, "COF")

if (lang == "de") {
  var_labels <- c("Finanzielle Situation\nin den letzten 12 Monaten",
                  "Finanzielle Situation\nin den nächsten 12 Monaten",
                  "Allg. wirtschaftliche Situation\nin den letzten 12 Monaten",
                  "Allg. wirtschaftliche Situation\nin den nächsten 12 Monaten",
                  "Größere Ausgaben\nin der Gegenwart",
                  "Großere Ausgaben\nin den nächsten 12 Monaten",
                  "Kauf/Bau eines Eigenheims\nin den nächsten 12 Monaten",
                  "Sanierung des Eigenheims\nin den nächsten 12 Monaten",
                  "Sparen\nin den nächsten 12 Moanten",
                  "Finanzielle Situation\nder Haushalte",
                  "Arbeitslosigkeit\nin den nächsten 12 Monaten",
                  "Allgemein")
  
  fig_title <- "Indikatoren zur Stimmung von Verbraucher:innen"
  fig_caption <- "Quelle: Europäische Kommission. Grau hinterlegte Fläche beschreibt die Bandbreite zwischen dem Minimum\nund Maximum von EU-Ländern. Letzter Wert: "
}

if (lang == "en") {
  var_labels <- c("Financial situation\nover last 12 months",
                  "Financial situation\nover next 12 months",
                  "General economic situation\nover last 12 months",
                  "General economic situation\nover next 12 months",
                  "Major purchases\nat present",
                  "Major purchases\nover next 12 months",
                  "Purchase or build a home\nwithin the next 12 months",
                  "Home improvements\nover the next 12 months",
                  "Savings\nover next 12 months",
                  "Statement on financial\nsituation of household",
                  "Unemployment expectations\nover next 12 months",
                  "Overall")
  
  fig_title <- "Consumer sentiment indicators"
  fig_caption <- "Source: European Commission. Ribbon describes the range between the minimum and maximum value of\nEU countries. Last value: "
}

raw <- ecdata %>%
  filter(var %in% var_levels) %>%
  #filter(!var %in% "COF") %>%
  filter(!is.na(value),
         date >= min_date) %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels))

temp_ribbon <- raw %>%
  filter(ctry %in% eu_ctry) %>%
  group_by(date, var) %>%
  summarise(ymin = quantile(value, 0),
            ymax = quantile(value, 1),
            .groups = "drop")


temp_line <- raw %>%
  filter(ctry %in% ctry_code)

max_date <- format(max(temp_line$date), "%YM%m")
fig_caption <- paste0(fig_caption, max_date, ".")

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp_line, aes(x = date)) +
  geom_ribbon(data = temp_ribbon, aes(ymin = ymin, ymax = ymax), alpha = .2) +
  geom_hline(yintercept = 0) +
  geom_line(aes(y = value, colour = ctry)) +
  scale_x_date(expand = c(.01, 0)) +
  facet_wrap(~ var, nrow = 3) +
  labs(title = fig_title,
       caption = fig_caption) +
  scale_colour_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

ggsave(g, filename = paste0("pics/", date_title, "-ecsurvey-consumer-", lang, ".jpeg"), height = 5, width = 5)

