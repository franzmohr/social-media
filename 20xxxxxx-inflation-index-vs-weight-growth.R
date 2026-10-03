
rm(list = ls())

lang <- "de"

min_date <- "2021-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(zoo)

ctry <- c("AT", "DE", "EA20", "EU27_2020")
file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file

# Load data on all COICOPs and selected countries ----

## Weights ----
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  arrange(year) %>%
  group_by(geo, coicop) %>%
  mutate(weight = weight / 1000) %>%
  #mutate(weight = weight - lag(weight, 1)) %>%
  ungroup() %>%
  filter(!is.na(weight))

## Growth prc_hicp_manr ----
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= min_date,
         !is.na(values)) %>%
  select(-freq) %>%
  rename(index = values) %>%
  mutate(index = index / 100)

temp <- index %>%
  filter(coicop != "CP00",
         !grepl("_", coicop),
         !grepl("-", coicop),
         nchar(coicop) > 4) %>%
  filter(geo %in% c("AT", "EA20")) %>%
  group_by(geo) %>%
  filter(time == max(time)) %>%
  ungroup() %>%
  filter(substring(coicop, 1, 2) == "CP") %>%
  # Merge with weight data
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  mutate(var = substring(coicop, 1, 4),
         pointsize = abs(index * weight),
         hl = pointsize > .001 | index > .1,
         txt = ifelse(hl, coicop, ""))

# Mapping of COICOP code and its title
# coicop <- read.csv("coicop_mapping.csv") %>%
#   filter(nchar(coicop) == 4) %>%
#   mutate(var_de = gsub("\\n", "\n", var_de, fixed = TRUE)) %>%
#   arrange(coicop)
# 
# temp <- temp %>%
#   left_join(coicop, by = c("var" = "coicop"))

max_date <- format(max(temp$time), "%YM%m")

if (lang == "de") {
  fig_title <- paste0("Beiträge der Warenkorbbestandteile zur Gesamtinflation (", max_date, ")")
  fig_x <- "Warenkorbgewicht in Prozent"
  fig_y <- "Sub-Indexwachstum in Prozent"
  fig_caption <- "Quelle: Eurostat. Eigene Berechnungen. Punktgröße ist proportional zum Beitrag der Komponente zur Gesamtinflation."
}

source("r-corporate-design-functions-ggplot2.R")

x_max <- max(temp$weight)

g <- ggplot(temp, aes(x = weight, y = index)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(size = pointsize, colour = var, alpha = hl), show.legend = FALSE) +
  geom_text(aes(label = txt, alpha = hl), size = 1, show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(-.002, x_max * 1.06), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_alpha_manual(values = c(.2, 1)) +
  facet_wrap(~geo) +
  labs(title = fig_title,
       x = fig_x,
       y = fig_y,
       caption = fig_caption) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")

g

ggsave(g, filename = paste0("pics/", file_date, "-inflation-index-vs-weight-growth-", lang, ".jpeg"), height = 5, width = 5)

