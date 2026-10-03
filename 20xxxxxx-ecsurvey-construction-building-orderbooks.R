rm(list = ls())

# Country codes
ctry_code <- c("AT", "EA", "DE")
ctry_names <- c("Österreich", "Euroraum", "Deutschland")


# Minimum data in plot
min_date <- "2015-01-01"

# Language of the plot
lang <- "de"


# ******************************************************************************
# From here on everything should work automatically

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(zoo)


# Download data ----

# Source: https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en


tfile <- tempfile(tmpdir = tdir <- tempdir())

# Try download for current month
curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_subsectors_sa_nace2.zip"),
                  destfile = tfile))
sdmx_files <- unzip(tfile, exdir = tdir)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/building_subsectors_sa_nace2.zip"),
                    destfile = tfile))
  sdmx_files <- unzip(tfile, exdir = tdir)
}

# Prepare data ----

temp_sdmx_files <- sdmx_files[which(grepl("sa_m_nace", sdmx_files))]

var_levels <- as.character(1:7)

ecdata <- readxl::read_xlsx(temp_sdmx_files, sheet = "41", na = "NA", col_types = c("date", rep("numeric", 455))) %>%
  rename(date = `41`) %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(value)) %>%
  mutate(name = gsub("BUIL.", "", name),
         ctry = substring(name, 1, 2),
         name = substring(name, 7, nchar(name))) %>%
  filter(substring(name, 1, 1) == "2") %>%
  mutate(name = gsub("2.F", "", name),
         name = gsub("S.M", "", name),
         name = gsub(".M", "", name)) %>%
  select(date, ctry, name, value) %>%
  filter(ctry %in% ctry_code,
         date >= min_date)

last_value <- format(as.yearmon(max(ecdata$date)), "%YM%m")

if (lang == "de") {
  var_labels <- c("Keine besonderen\nSchwierigkeiten",
                  "Mangel an Aufträgen",
                  "Ungünstige Witterungsverhältnisse",
                  "Arbeitskräftemangel",
                  "Materialknappheit/\nKapazitätsengpässe",
                  "Sonstige Gründe",
                  "Finanzierungsprobleme")
  temp_title <- "Welche Faktoren beeinträchtigen die Bautätigkeit? (EK Umfrage)"
  temp_caption <- "Quelle: Europäische Kommission. Subsektor 41 (Errichtung von Gebäuden). Anteil der Rückmeldungen in\nProzent. Saisonal bereinigte Daten. Code unter: https://github.com/franzmohr/instagram."
}

if (lang == "en") {
  var_labels <- c("None",
                  "Insufficient demand",
                  "Weather conditions",
                  "Shortage of labour force",
                  "Shortage of material and/or equipment",
                  "Other factors",
                  "Financial constraints") 
  temp_title <- "Main factors currently limiting your building activity according to EC survey"
  temp_caption <- "Source: European Commission. Subsector 41 (construction of buildings). Share of responses in percent. Seasonally adjusted series."
}

ecdata <- ecdata %>%
  mutate(name = factor(name, levels = var_levels, labels = var_labels),
         ctry = factor(ctry, levels = ctry_code, labels = ctry_names),
         value = value / 100)

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(ecdata, aes(x = date, y = value)) +
  geom_zeroline() +
  geom_line(aes(colour = ctry)) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .8), expand = c(0, 0)) +
  scale_colour_corporate_design() +
  facet_wrap(~ name, ncol = 3) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_corporate_design(base_size = 13) +
  theme()

g
save_chart(g, "ecsurvey-construction-building-orderbooks", lang = lang, format = "portrait")
