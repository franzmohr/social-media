
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

raw <- read_excel("D:/Data/20260412-statistik-austria-employment.xlsx", 
                  sheet = "1. Erwerbstätig", na = "-", 
                  skip = 8)[, -1]

names(raw)[1] <- "date"

temp <- raw %>%
  pivot_longer(cols = -c("date")) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.yearqtr(date, "%q. Quartal %Y"),
         nace = substring(name, nchar(name) - 1, nchar(name) - 1),
         name = substring(name, 1, nchar(name) - 4)) %>%
  arrange(date) %>%
  na.omit() %>%
  filter(date >= min_date,
         name != "Private Haushalte")

last_n_periods <- unique(pull(temp, "date"))
last_n_periods <- last_n_periods[order(last_n_periods)]
last_n_periods <- tail(last_n_periods, 5)

period_1 <- last_n_periods[1]
period_2 <- last_n_periods[length(last_n_periods)]

temp <- temp %>%
  filter(date %in% c(period_1, period_2)) %>%
  mutate(date = format(date, "%YQ%q"),
         name = reorder(name, value, sum))
# top_name <- temp %>%
#   filter(date == max(date)) %>%
#   group_by(name) %>%
#   summarise(value = sum(abs(value)),
#             .groups = "drop") %>%
#   arrange(desc(value)) %>%
#   pull("name")
# 
# temp <- temp %>%
#   mutate(name = factor(name, levels = top_name))

if (lang == "de") {
  fig_title <- "Beschäftigung in Österreich nach Sektor"
  fig_subtitle <- "Tausend Personen"
  fig_caption <- "Quelle: STATcube - Statistische Datenbank von Statistik Austria."
}

max_value <- max(temp$value)

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp, aes(x = name, y = value, fill = date)) +
  geom_col(position = position_dodge(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, max_value * 1.06), expand = c(0, 0)) +
  scale_fill_corporate_design() +
  coord_flip() +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  guides(fill = guide_legend(ncol = 2)) +
  theme_corporate_design(base_size = 13) +
  theme(axis.line = element_blank(),
        axis.title = element_blank())

g
save_chart(g, "austria-employment-by-sector-yoy-comparison", lang = lang, format = "portrait")
