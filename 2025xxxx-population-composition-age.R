rm(list = ls())

# Choose language
lang <- "de" # "de" or "en"

# Choose country
ctry <- "AT"

# First year of the comparison
ref_year <- "1973"

# Load packages
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

source("theme_instagram.R")

raw <- get_eurostat("demo_pjanind",
                    filters = list(geo = ctry),
                    cache = FALSE) %>%
  filter(!is.na(values))

unique(raw$indic_de)

temp_levels <- c("PC_Y0_4", "PC_Y5_9", "PC_Y10_14", "PC_Y15_19", "PC_Y20_24", "PC_Y25_29", "PC_Y30_34", "PC_Y35_39",
                 "PC_Y40_44", "PC_Y45_49", "PC_Y50_54", "PC_Y55_59", "PC_Y60_64", "PC_Y65_69", "PC_Y70_74",
                 "PC_Y75_79", "PC_Y80_84", "PC_Y85_MAX")

if (lang == "de") {
  temp_names <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80-84", "Ab 85")
  temp_y <- "Anteil an der Gesamtbevölkerung"
  temp_title <- "Bevölkerungsstruktur nach Altersgruppen (Österreich)"
  temp_caption <- "Quelle: Eurostat."
}

if (lang == "en") {
  temp_names <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                  "70-74", "75-79", "80-84", "From 85")
  temp_y <- "Share in total population"
  temp_title <- "Population composition by age group (Austria)"
  temp_caption <- "Source: Eurostat."
}

temp <- raw %>%
  rename(var = indic_de) %>%
  mutate(time = substring(time, 1, 4)) %>%
  filter(time %in% c(max(time), ref_year),
         !is.na(values),
         var %in% temp_levels) %>%
  mutate(var = factor(var, levels = temp_levels, labels = temp_names),
         var = forcats::fct_rev(var),
         values = values / 100,
         values = ifelse(time == max(time), values, -values))

temp %>%
  select(time, var, values) %>%
  pivot_wider(names_from = "var", values_from = "values") %>%
  tail()

max_value <- max(abs(temp$values))

g <- ggplot(temp, aes(x = var, y = values)) +
  geom_col(aes(fill = time), show.legend = TRUE) +
  geom_hline(yintercept = 0) +
  scale_fill_insta +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(-max_value * 1.06, max_value * 1.06), expand = c(0, 0)) +
  labs(title = temp_title,
       y = temp_y,
       caption = temp_caption) +
  coord_flip() +
  theme_instagram +
  theme(axis.text.x = element_blank(),
        legend.position = "top")

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-population-composition-age-", lang, ".jpeg"), height = 7, width = 7)

