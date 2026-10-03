
rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)
library(zoo)

temp <- tempfile()
download.file("https://www.oenb.at/dam/jcr:f0e7bcc6-56ef-42c4-b2ba-d4c1ca46860c",
              destfile = temp, mode = "wb")

result <- read_xlsx(temp, sheet = "Daten", skip = 7,
                    col_names = c("date", "at", "ö2" ,"ö3", "ö4", "ö5", "ö6", "ö7", "ö8",
                                  "wien",  "w2" ,"w3", "w4", "w5", "w6", "w7", "w8")) %>%
  mutate(date = as.Date(date),
         date = lubridate::ceiling_date(date, "quarter") - 1) %>%
  select(date, at, wien) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(!is.na(value)) %>%
  mutate(value = value / 100) %>%
  mutate(variable = "wohnimmofund",
         subvariable = case_when(name == "at" ~ "at",
                                 name == "wien" ~ "vienna"),
         subtitle = case_when(name == "at" ~ "Österreich",
                              name == "wien" ~ "Wien")) %>%
  select(date, variable, subvariable, subtitle, value)  %>%
  filter(date >= "2006-01-01") %>%
  mutate(name = factor(subvariable, levels = c("at", "vienna"),
                       labels = c("Österreich", "Wien")),
         date = as.yearqtr(date))

file.remove(temp)

source("theme_instagram.R")

g <- ggplot(result, aes(x = date, y = value, colour = name)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(linewidth = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  guides(colour = guide_legend(ncol = 2)) +
  labs(title = "OeNB-Fundamentalpreisindikator",
       subtitle = "Abweichung vom langfristigen Trend",
       caption = "Quelle: OeNB. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20230906_house_price_fund_de.jpeg", height = 5, width = 5)

