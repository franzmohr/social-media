rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(zoo)

lang <- "de"

min_date <- "2019-01-01"
n_top <- 6   # sectors shown in colour; the remainder collapses into grey

raw <- read_excel("D:/Data/20260412-statistik-austria-employment.xlsx",
                  sheet = "1. Erwerbstätig", na = "-",
                  skip = 8)[, -1]

names(raw)[1] <- "date"

temp <- raw %>%
  pivot_longer(cols = -c("date")) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(as.yearqtr(date, "%q. Quartal %Y")),
         name = substring(name, 1, nchar(name) - 4)) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(value = value - lag(value, 4)) %>%
  ungroup() %>%
  na.omit() %>%
  filter(date >= min_date)

# Rank sectors on their contribution over the last four quarters ------------
last_n_periods <- tail(sort(unique(temp$date)), 4)

top_name <- temp %>%
  filter(date %in% last_n_periods) %>%
  group_by(name) %>%
  summarise(value = sum(abs(value)), .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(seq_len(n_top)) %>%
  pull("name")

if (lang == "de") {
  fig_title <- "Wer stellt noch ein - und wer nicht?"
  fig_subtitle <- paste0("Beitrag der Sektoren zur Veränderung der Beschäftigung in\n",
                         "Österreich gegenüber dem Vorjahresquartal, in tausend Personen")
  label_other <- "Übrige Sektoren"
  label_net <- "Gesamt"
  src <- "STATcube, Statistik Austria."
} else {
  fig_title <- "Who is still hiring - and who is not?"
  fig_subtitle <- paste0("Sector contributions to the change in Austrian employment\n",
                         "versus the same quarter a year earlier, in thousand persons")
  label_other <- "Other sectors"
  label_net <- "Total"
  src <- "STATcube, Statistics Austria."
}

# Shorten the long official sector names so the legend stays on two lines ----
shorten <- function(x) {
  x <- sub("^Herstellung von Waren$", "Industrie", x)
  x <- sub("^Beherbergung und Gastronomie$", "Tourismus, Gastro", x)
  x <- sub("^Gesundheits- und Sozialwesen$", "Gesundheit, Soziales", x)
  x <- sub("^Information und Kommunikation$", "IT, Kommunikation", x)
  x <- sub("^Öffentliche Verwaltung.*$", "Öffentl. Verwaltung", x)
  x <- sub("^Sonst.* Dienstleistungen$", "Sonst. Dienstleistungen", x)
  x <- sub("^Erbringung von ", "", x)
  x
}

temp <- temp %>%
  mutate(name = ifelse(name %in% top_name, shorten(name), label_other),
         name = factor(name, levels = c(shorten(top_name), label_other)))

# The net line is what readers actually want to read off a stacked bar chart
net <- temp %>%
  group_by(date) %>%
  summarise(value = sum(value), .groups = "drop")

max_date <- format(as.yearqtr(max(temp$date)), "%YQ%q")

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_col(aes(fill = name), width = 80) +
  geom_zeroline() +
  # net change drawn on top, so the total is readable at a glance
  geom_line(data = net, colour = franz_colours[["ink"]], linewidth = .8) +
  geom_point(data = filter(net, date == max(date)),
             colour = franz_colours[["ink"]], size = 2) +
  geom_text(data = filter(net, date == max(date)),
            aes(label = paste0(label_net, ": ", round(value))),
            hjust = 0, nudge_x = 40, size = 3.6, fontface = "bold",
            colour = franz_colours[["ink"]], family = franz_font) +
  scale_x_date(breaks = seq(as.Date(min_date), max(temp$date), by = "1 year"),
               date_labels = "%Y",
               expand = expansion(mult = c(.02, .17))) +
  scale_y_continuous(breaks = scales::breaks_width(100)) +
  scale_fill_highlight(highlight = shorten(top_name), rest = label_other) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                             override.aes = list(colour = NA))) +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = franz_caption(src, last = max_date, lang = lang)) +
  theme_franz(base_size = 13, grid = "y") +
  theme(axis.title = element_blank(),
        legend.key.size = unit(11, "pt"),
        legend.text = element_text(size = 10.5),
        legend.spacing.x = unit(4, "pt"))

g

save_post(g, "austria-employment-by-sector-change", lang = lang, format = "portrait")
