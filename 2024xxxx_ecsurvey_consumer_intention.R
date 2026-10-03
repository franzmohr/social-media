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


ecdata <- readxl::read_xlsx(sdmx_files,
                              sheet = "CONSUMER QUARTERLY", na = "NA", col_types = c("text", rep("numeric", 105))) %>%
  mutate(date = as.Date(as.yearqtr(`...1`, "%Y-Q%q"))) %>%
  select(-`...1`) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(substring(name, 1, 3) != "...") %>%
  mutate(ctry = substring(name, 6, 7),
         var = substring(name, 13, 14),
         var = paste0("ecsurvey_", var))%>%
  filter(!is.na(value)) %>%
  select(date, ctry, var, value) %>%
  filter(ctry %in% ctry_code)

# Combine ----

var_levels <- c("ecsurvey_13", "ecsurvey_14", "ecsurvey_15")

var_labels_de <- c("...ein Auto zu kaufen",
                   "...eine Immobilie zu kaufen oder zu errichten",
                   "...das Eigenheim zu renovieren")

var_labels_en <- c("Intention to buy a car within the next 12 months",
                   "Purchase or build a home within the next 12 months",
                   "Home improvements over the next 12 months")

result <- ecdata %>%
  filter(var %in% var_levels) %>%
  filter(!is.na(value),
         date >= min_date) %>%
  mutate(name_de = factor(var, levels = var_levels, labels = var_labels_de),
         name_en = factor(var, levels = var_levels, labels = var_labels_en))

source("theme_instagram.R")

g <- ggplot(result, aes(x = date, y = value)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = ctry)) +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(limits = c(-100, 100)) +
  facet_wrap(~ name_de, ncol = 2) +
  labs(title = "Die Haushalte planen in den nächsten 12 Monaten...",
       subtitle = "Index",
       caption = "Quelle: Europäische Kommission. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

ggsave(g, filename = paste0("pics/", date_title, "_ecsurvey_consumer_intention_de.jpeg"), height = 5, width = 5)
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
