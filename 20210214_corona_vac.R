rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

temp <- tempfile()
download.file("https://covid.ourworldindata.org/data/owid-covid-data.xlsx",
              destfile = temp, mode = "wb")
data <- read_xlsx(temp, col_types = "text")
unlink(temp)

countries <- c("AUT", "DEU", "CAN", "SWE", "BEL", "USA")

temp <- data %>%
  filter(iso_code %in% countries) %>%
  mutate(date = as.Date(date),
         value = as.numeric(total_vaccinations_per_hundred) / 100) %>%
  select(date, iso_code, value) %>%
  mutate(names_de = factor(iso_code, levels = countries,
                           labels = c("Österreich", "Deutschland", "Kanada", "Schweden", "Belgien", "USA")),
         names_en = factor(iso_code, levels = countries,
                           labels = c("Austria", "Germany", "Canada", "Sweden", "Belgium", "USA"))) %>%
  filter(date >= "2020-02-14") %>%
  #select(date, names_de, names_en, value1, value2) %>%
  filter(!is.na(value))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = names_de)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "1 weeks", expand = c(.01, 0), date_labels = "%d.%m.%Y") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",", accuracy = 1)) +
  #facet_wrap(~name, scales = "free_y") +
  labs(title = "Covid-19-Impfung",
       subtitle = "Anteil der geimpften Personen an der Gesamtbevölkerung",
       caption = "Quelle: https://ourworldindata.org/coronavirus.\nCode unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20210214_corona_vac_de.jpeg", height = 5, width = 5)

g <- ggplot(temp, aes(x = date, y = value, colour = names_en)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "1 week", expand = c(.01, 0), date_labels = "%d %b %Y") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ".", accuracy = 1)) +
  labs(title = "Covid-19-Vaccination",
       subtitle = "Share of vaccinated persons in total population",
       caption = "Source: https://ourworldindata.org/coronavirus.\nCode available at https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20210214_corona_vac_en.jpeg", height = 5, width = 5)
