
rm(list = ls())

lang <- "de"

min_date <- "2021-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- c("AT", "DE", "EA20", "EU27_2020")

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
  filter(time >= min_date,
         !is.na(values)) %>%
  select(-freq)

split_coicop <- c("CP01", "CP04", "CP09", "CP12")

comp <- index %>%
  filter(substring(coicop, 1, 2) == "CP", # Only consider "raw data"
         !coicop %in% c("CP00"),
         !grepl("_", coicop),
         !grepl("-", coicop)) %>%
  mutate(cond = case_when(substring(coicop, 1, 4) %in% split_coicop & nchar(coicop) == 5 ~ TRUE,
                          nchar(coicop) == 4 & !coicop %in% split_coicop ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(cond) %>%
  # Merge with weight data
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  # Calculate contribution
  mutate(values = values * weight) %>%
  # # Apply filters based on try and error
  # # If a COICOP with one sublevel is excluded, its sub-sub-levels MUST be included!
  # mutate(cond = case_when(nchar(coicop) == 5 & !coicop %in% c("CP045", "CP094", "CP111") ~ TRUE,
  #                         nchar(coicop) == 6 & substring(coicop, 1, 5) %in% c("CP045", "CP094", "CP111") ~ TRUE,
  #                         TRUE ~ FALSE)) %>%
  #filter(cond) %>%
  # Drop weight information
  select(-weight)

# Get indicators with highest contribution to inflation in period
top_comp <- comp %>%
  filter(time == "2025-08-01",
         geo == "AT") %>%
  group_by(coicop) %>%
  summarise(value = sum(abs(values)),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(1:6) %>%
  pull("coicop") %>%
  as.character()


# Mapping of COICOP code and its title
coicop <- read.csv("mapping-coicop-1.csv") %>%
  filter(coicop %in% top_comp) %>%
  mutate(coicop = factor(coicop, levels = top_comp),
         var_de = gsub("\\n", "\n", var_de, fixed = TRUE)) %>%
  arrange(coicop)

coicop_levels <- pull(coicop, "coicop")

if (lang == "de") {
  coicop_labels <- pull(coicop, "var_de")
  ctry_labels <- c("Österreich", "Deutschland", "Euroraum-20", "Europäische Union")
  temp_other <- "Andere"
  
  fig_title <- "Bedeutendste Inflationstreiber"
  fig_subtitle <- "Beiträge der Komponenten zur Gesamtinflation in Prozentpunkten"
  fig_caption <- "Quelle: Eurostat. Eigene Berechnungen. Auswahl basiert auf österr. Werten im August 2025. Letzter Wert: "
}


temp <- comp %>%
  mutate(var = ifelse(coicop %in% top_comp, coicop, "other"),
         var = factor(var, levels = c(top_comp, "other"), labels = c(coicop_labels, temp_other)),
        # Format country names for plotting
         geo = factor(geo, levels = ctry, labels = ctry_labels))

max_date <- format(max(temp$time), "%YM%m")

fig_caption <- paste0(fig_caption, max_date, ".")

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_zeroline() +
  geom_col(aes(fill = var)) +
  facet_wrap(~geo) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10, 12)) +
  scale_fill_corporate_design() +
  theme_corporate_design(base_size = 13) +
  theme() +
  theme(legend.box = "vertical") +
  theme(axis.title = element_blank())

g

save_chart(g, "inflation-main-drivers-coicop-1", lang = lang, format = "portrait")

