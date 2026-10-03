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
library(tidyr)
library(lubridate)
library(readxl)

# Source
# https://ec.europa.eu/info/business-economy-euro/indicators-statistics/economic-databases/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en


## Main indicators ----
tfile <- tempfile(tmpdir = tdir <- tempdir())

# Try download for current month
curr_date <- Sys.Date()
curr_month <- month(curr_date)
curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
curr_year <- substring(year(curr_date), 3, 4)
curr_month <- paste0(curr_year, curr_month)
try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/main_indicators_sa_nace2.zip"),
                  destfile = tfile))
sdmx_files <- unzip(tfile, exdir = tdir)

# Download failed try it with download of one month earlier
if (length(sdmx_files) == 0) {
  curr_date <- floor_date(Sys.Date(), "month") - 1
  curr_month <- month(curr_date)
  curr_month <- ifelse(nchar(curr_month) == 1, paste0("0", curr_month), curr_month)
  curr_year <- substring(year(curr_date), 3, 4)
  curr_month <- paste0(curr_year, curr_month)
  try(download.file(paste0("https://ec.europa.eu/economy_finance/db_indicators/surveys/documents/series/nace2_ecfin_", curr_month, "/main_indicators_sa_nace2.zip"),
                    destfile = tfile))
  sdmx_files <- unzip(tfile, exdir = tdir)
}

ec_survey <- readxl::read_xlsx(sdmx_files,
                                          sheet = "MONTHLY", na = "NA", col_types = c("date", rep("numeric", 280))) %>%
  mutate(date = as.Date(`...1`)) %>%
  select(-`...1`) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(substring(name, 1, 3) != "...")  %>%
  mutate(ctry = substring(name, 1, 2),
         var = substring(name, 4, nchar(name))) %>%
  filter(!is.na(value)) %>%
  select(date, ctry, var, value) %>%
  filter(ctry %in% ctry_code,
         date >= min_date)

# Combine ----
use_var <- c("INDU", "SERV", "CONS", "RETA", "BUIL")
var_levels <- c("INDU", "SERV", "CONS", "RETA", "BUIL", "ESI", "EEI")

if (lang == "de") {
  var_labels <- c("Industrie", "Dienstleistungen", "Konsument:innen", "Einzelhandel", "Bau", "Gesamt", "Beschäftigungserwartungen")  
  temp_title <- "Stimmungsindikatoren"
  temp_subtitle <- "Index"
  temp_caption <- "Quelle: Europäische Kommission. Code unter https://github.com/franzmohr/instagram."
}

if (lang == "en") {
  var_labels <- c("Industry","Services","Consumers","Retail","Construction","Overall","Emmployment expectations")
  temp_title <- "Confidence indicators"
  temp_subtitle <- "Index"
  temp_caption <- "Source: European Commission. Code unter https://github.com/franzmohr/instagram."
}

result <- ec_survey %>% 
  filter(var %in% use_var) %>%
  mutate(name = factor(var, levels = var_levels, labels = var_labels),
         value = value / 100)

source("theme_instagram.R")

g <- ggplot(result, aes(x = date, y = value)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = ctry)) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ name, ncol = 2) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-ecsurvey-main-", lang, ".jpeg"), height = 5, width = 5)
