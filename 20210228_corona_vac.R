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

countries <- c("AUT")

temp <- data %>%
  filter(iso_code %in% countries) %>%
  mutate(date = as.Date(date),
         vacc = as.numeric(total_vaccinations_per_hundred) / 100,
         death = as.numeric(total_deaths_per_million) / 10^6,
         cases = as.numeric(total_cases_per_million) / 10^6,
         imm = cases - death) %>%
  select(date, iso_code, vacc, imm) %>%
  pivot_longer(cols = c("vacc", "imm")) %>%
  mutate(names_de = factor(name, levels = c("vacc", "imm"),
                           labels = c("Geimpft", "Genesen")),
         names_en = factor(name, levels = c("vacc", "imm"),
                           labels = c("Vaccinated", "Recovered"))) %>%
  filter(date >= "2020-02-14") %>%
  #select(date, names_de, names_en, value1, value2) %>%
  filter(!is.na(value))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, fill = names_de)) +
  geom_area() +
  scale_x_date(date_breaks = "1 month", expand = c(.01, 0), date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",", accuracy = 1)) +
  #facet_wrap(~name, scales = "free_y") +
  labs(title = "Covid-19 - Genesungen und Impffortschritt (Österreich)",
       subtitle = "Anteil der Personen an der Gesamtbevölkerung",
       caption = "Quelle: https://ourworldindata.org/coronavirus. 'Genesen' definiert als Anzahl der dokumen-\ntierten Fäll abzgl. Todesfälle. Code unter https://github.com/franzmohr/instagram.") +
  scale_fill_insta +
  theme_instagram

ggsave(g, filename = "pics/20210228_corona_de.jpeg", height = 5, width = 5)

# Europe

countries <- eurostat::eu_countries[, 2]
countries <- countries[-which(countries == "United Kingdom")]

temp <- data %>%
  filter(location %in% countries) %>%
  mutate(date = as.Date(date),
         pop = as.numeric(population),
         vacc = as.numeric(total_vaccinations),
         death = as.numeric(total_deaths),
         cases = as.numeric(total_cases),
         imm = cases - death) %>%
  select(date, iso_code, vacc, imm, pop) %>%
  pivot_longer(cols = -c("date", "iso_code")) %>%
  pivot_wider() %>%
  group_by(iso_code) %>%
  mutate(vacc = zoo::na.locf(vacc, na.rm = FALSE)) %>%
  ungroup() %>%
  pivot_longer(cols = -c("date", "iso_code")) %>%
  group_by(date, name) %>%
  summarise(value = sum(value, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider() %>%
  mutate(vacc = vacc / pop,
         imm = imm / pop) %>%
  pivot_longer(cols = -c("date", "pop")) %>%
  mutate(name_de = factor(name, levels = c("vacc", "imm"),
                           labels = c("Geimpft", "Genesen")),
         name_en = factor(name, levels = c("vacc", "imm"),
                           labels = c("Vaccinated", "Recovered"))) %>%
  filter(date >= "2020-02-14") %>%
  #select(date, names_de, names_en, value1, value2) %>%
  filter(!is.na(value))

g <- ggplot(temp, aes(x = date, y = value, fill = name_en)) +
  geom_area() +
  scale_x_date(date_breaks = "1 month", expand = c(.01, 0), date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ",", accuracy = 1)) +
  #facet_wrap(~name, scales = "free_y") +
  labs(title = "Covid-19 - Cases and vaccination progress (EU-27)",
       subtitle = "Share of persons in total population",
       caption = "Source: https://ourworldindata.org/coronavirus. 'Recovered' is defined as total documented\ncases minus deaths. Code available at https://github.com/franzmohr/instagram.") +
  scale_fill_insta +
  theme_instagram

ggsave(g, filename = "pics/20210228_corona_en.jpeg", height = 5, width = 5)
