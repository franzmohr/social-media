# rre, rent, inflation

rm(list = ls())

# Choose countries
ctry_code <- c("AT", "DE", "CZ", "HU", "SI", "EU27_2020")

# Choose country for highlighting
hl_ctry <- c("AT", "EU27_2020")

# Choose language
lang <- "de"


library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)


if (lang == "de") {
  ctry_labels <- c("Österreich", "Deutschland", "Tschechien", "Ungarn", "Slovenien", "EU (27)")
  temp_title <- "Wohnungsmieten" # Titel wie bei Eurostat
  temp_subtitle <- "Jahreswachstum in Prozent"
  temp_caption <- "Quelle: Eurostat (prc_hicp_manr - CP041)."
}
if (lang == "en") {
  ctry_labels <- c("Austria", "Germany", "Czech Republic", "Hungary", "Slovenia", "EU (27)")
  temp_title <- "Actual rentals for housing" # Title like displayed by Eurostat
  temp_subtitle <- "Annual growth in %"
  temp_caption <- "Source: Eurostat (prc_hicp_manr - CP041)."
}

rent <- get_eurostat(id = "prc_hicp_manr",
                     filter = list(geo = ctry_code,
                                   coicop = "CP041"),
                     cache = FALSE) %>%
  mutate(date = as.Date(as.yearmon(as.character(time), "%Y-%m"))) %>%
  arrange(date) %>%
  filter(date >= "2015-01-01") %>%
  filter(!is.na(values)) %>%
  mutate(hl = geo %in% hl_ctry,
         geo = factor(geo, levels = ctry_code, labels = ctry_labels),
         values = values / 100)
  

source("theme_instagram.R")


g <- ggplot(rent, aes(x = date, y = values, colour = geo, alpha = hl)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.2) +
  scale_x_date(expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(.4, 1)) +
  guides(colour = guide_legend(ncol = 2),
         alpha = "none") +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_colour_insta +
  theme_instagram

g

ggsave(g, filename = "pics/20240227_rre_rent_growth_for_eu_countries_de.png", height = 5, width = 5)

