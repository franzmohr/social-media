
rm(list = ls())


# Choose language
lang <- "de"

min_date <- "2019-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(readxl)
library(ggthemes)

raw <- read_excel("D:/Data/20260412-statistik-austria-employment.xlsx", 
                  sheet = "1. Erwerbstätig", na = "-", 
                  skip = 8)[, -1]

names(raw)[1] <- "date"

temp <- raw %>%
  pivot_longer(cols = -c("date")) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(as.yearqtr(date, "%q. Quartal %Y")),
         nace = substring(name, nchar(name) - 1, nchar(name) - 1),
         name = substring(name, 1, nchar(name) - 4)) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(value = value - lag(value, 4)) %>%
  ungroup() %>%
  na.omit() %>%
  filter(date >= min_date)

last_n_periods <- unique(pull(temp, "date"))
last_n_periods <- last_n_periods[order(last_n_periods)]
last_n_periods <- tail(last_n_periods, 4)

top_name <- temp %>%
  filter(date %in% last_n_periods) %>%
  group_by(name) %>%
  summarise(value = sum(abs(value)),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(1:7) %>%
  pull("name")


if (lang == "de") {
  fig_title <- "Beschäftigung in Österreich nach Sektor"
  fig_subtitle <- "Veränderung im Vergleich zum Vorjahreswert in tausend Personen"
  fig_caption <- "Quelle: STATcube - Statistische Datenbank von Statistik Austria. Eigene Berechnungen. Letzter Wert: "
  label_other <- "Andere"
}

temp <- temp %>%
  mutate(name = ifelse(name %in% top_name, name, "other"),
         name = factor(name, levels = c(top_name, "other"),
                       labels = c(top_name, label_other)))

max_date <- format(as.yearqtr(max(temp$date)), "%YQ%q")
fig_caption <- paste0(fig_caption, max_date, ".")

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp, aes(x = date, y = value, fill = name)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_x_date(expand = c(0, 0), date_labels = "%Y", date_breaks = "1 year") +
  scale_fill_tableau() +
  #scale_fill_insta +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-austria-employment-by-sector-change.jpeg"), height = 7, width = 7)
