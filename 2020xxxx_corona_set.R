rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

temp <- tempfile()
download.file("https://covid.ourworldindata.org/data/owid-covid-data.xlsx",
              destfile = temp, mode = "wb")
data <- read_xlsx(temp, col_types = c("text", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
unlink(temp)

countries <- c("AUT", "DEU", "FRA", "ITA", "SWE", "BEL", "USA")

temp <- data %>%
  filter(iso_code %in% countries) %>%
  mutate(date = as.Date(date),
         value1 = new_cases_smoothed_per_million,
         value2 = new_tests_smoothed_per_thousand,
         value3 = positive_rate * 100) %>%
  select(date, iso_code, value1, value2, value3) %>%
  pivot_longer(cols = -c("date", "iso_code")) %>%
  mutate(names_de = factor(iso_code, levels = countries,
                           labels = c("Österreich", "Deutschland", "Frankreich", "Italien", "Schweden", "Belgien", "USA")),
         names_en = factor(iso_code, levels = countries,
                           labels = c("Austria", "Germany", "France", "Italy", "Sweden", "Belgium", "USA")),
         name = factor(name, levels = c("value1", "value2", "value3"),
                       labels = c("Neu Fälle pro\nmio. EinwohnerInnen",
                                  "Neue Tests pro\ntsd. EinwohnerInnen",
                                  "Anteil bestätigter Fälle an den\ngesamten Tests pro Tag (in %)"))) %>%
  filter(iso_code != "ITA") %>%
  filter(date >= "2020-02-14") %>%
  #select(date, names_de, names_en, value1, value2) %>%
  filter(!is.na(value))

source("theme_instagram.R")

ggplot(temp, aes(x = date, y = value, colour = names_de)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "3 weeks", expand = c(.01, 0)) +
  #scale_y_continuous(labels = scales::percent_format(decimal.mark = ",")) +
  facet_wrap(~name, scales = "free_y") +
  labs(title = "Covid-19-Neuinfektionen",
       subtitle = "Anteil bestätigter Fälle an den gesamten Tests pro Tag",
       caption = "Quelle: https://ourworldindata.org/coronavirus.\nCode unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200919_corona_pos_rates_de.jpeg", height = 5, width = 5)

g <- ggplot(temp, aes(x = date, y = value, colour = names_en)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "2 weeks", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ".")) +
  labs(title = "New cases of Covid-19-infections",
       subtitle = "The number of confirmed cases divided by the number of tests per day",
       caption = "Source: https://ourworldindata.org/coronavirus.\nCode available at https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200919_corona_pos_rates_en.jpeg", height = 5, width = 5)


temp <- data %>%
  filter(iso_code %in% "AUT") %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(lnew = log(new_cases / population),
         ltest = log(new_tests / population),
         ldeath = log(new_deaths / population),
         dnew = new_cases - lag(new_cases, 1),
         dtest = new_tests - lag(new_tests, 1),
         ddeath = new_deaths - lag(new_deaths, 1),
         dlnew = lnew - lag(lnew, 1),
         dltest = ltest - lag(ltest, 1),
         dldeath = ldeath - lag(ldeath, 1)) %>%
  select(lnew, ltest, ldeath) %>%
  filter(abs(lnew) != Inf,
         abs(ltest) != Inf,
         abs(ldeath) != Inf) %>%
  na.omit()

pairs(temp)

summary(lm(lnew ~ ., data = temp))


