rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- c("AT", "EA20")
file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file

# Load data on all COICOPs and selected countries ----

## Weights ----
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  # Rescale
  mutate(weight = weight / 1000)

## Growth prc_hicp_manr ----
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values)) %>%
  select(-freq)

comp <- index %>%
  filter(substring(coicop, 1, 2) == "CP", # Only consider "raw data"
         !coicop %in% c("CP00"), # Don't use overall
         nchar(coicop) > 4, # Only use COICOPs at least on sub-section level
         nchar(coicop) <= 6) %>%
  # Merge with weight data
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  # Calculate contribution
  mutate(values = values * weight) %>%
  # Apply filters based on try and error
  # If a COICOP with one sublevel is excluded, its sub-sub-levels MUST be included!
  mutate(cond = case_when(nchar(coicop) == 5 & !coicop %in% c("CP045", "CP094", "CP111") ~ TRUE,
                          nchar(coicop) == 6 & substring(coicop, 1, 5) %in% c("CP045", "CP094", "CP111") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(cond) %>%
  # Drop weight information
  select(-weight)

# Get indicators with highest contribution to inflation in period
top_comp <- comp %>%
  filter(time >= "2023-07-01",
         geo == "AT") %>%
  group_by(coicop) %>%
  summarise(value = sum(values^2),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(1:12) %>%
  pull("coicop") %>%
  as.character()


# Mapping of COICOP code and its title
coicop <- read.csv("coicop_mapping.csv") %>%
  filter(coicop %in% top_comp) %>%
  mutate(coicop = factor(coicop, levels = top_comp),
         var_de = gsub("\\n", "\n", var_de, fixed = TRUE)) %>%
  arrange(coicop)

coicop_levels <- pull(coicop, "coicop")
coicop_labels <- pull(coicop, "var_de")

temp <- comp %>%
  filter(coicop %in% top_comp) %>%
  #mutate(var_de = factor(var_de, levels = top_comp))
  mutate(coicop = factor(coicop, levels = coicop_levels, labels = coicop_labels),
         # Format country names for plotting
         geo = factor(geo, levels = ctry, labels = c("Österreich", "Euroraum-20")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = geo), alpha = 1) +
  facet_wrap(~coicop, nrow = 3) +
  labs(title = "Die stärksten Inflationstreiber seit Juli 2023",
       subtitle = "Beiträge der Komponenten zur Gesamtinflation in Prozentpunkten",
       caption = "Quelle: Eurostat. Code unter https://github.com/franzmohr/instagram.") +
  scale_x_date(expand = c(.01, 0), date_breaks = "6 months", date_labels = "%YM%m") +
  scale_colour_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")

g

ggsave(g, filename = paste0("pics/", file_date, "_infl_main_drivers.jpeg"), height = 5, width = 5)

