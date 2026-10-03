rm(list = ls())

library(dplyr)
library(ggplot2)
library(OECD) # install.packages("OECD")
library(seasonal)
library(tidyr)
library(zoo)


#### Inflation ####
p <- get_dataset("PRICES_CPI", filter = "AUT.CPALTT01.IXOB.Q") %>%
  select(obsTime, obsValue) %>%
  arrange(obsTime) %>%
  mutate(type = "p",
         obsValue = obsValue / lag(obsValue, 4) - 1) %>%
  filter(!is.na(obsValue))

p$obsValue <- mFilter::hpfilter(p$obsValue, freq = 1600)[["cycle"]]

#ptemp <- ts(p[, "obsValue"], start = c(1958, 1), frequency = 4)
#ptemp <- seasonal::seas(ptemp)$data[, "final"]
#p[, "obsValue"] <- ptemp

#### Unemployment ####
# FRED: LMUNRRTTATQ156S (downloads from OECD)
u <- get_dataset("MEI", filter = "AUT.LMUNRRTT.STSA.Q") %>%
  select(obsTime, obsValue) %>%
  mutate(type = "u",
         obsValue = obsValue / 100)

u$obsValue <- mFilter::hpfilter(u$obsValue, freq = 1600)[["cycle"]]

temp <- bind_rows(p, u) %>%
  arrange(obsTime) %>%
  mutate(obsTime = as.yearqtr(obsTime, "%Y-Q%q")) %>%
  filter(obsTime >= "1980 Q1") %>%
  pivot_wider(names_from = "type", values_from = "obsValue") %>%
  na.omit()

source("theme_instagram.R")

ggplot(temp, aes(x = obsTime, y = obsValue, colour = type)) +
  geom_hline(yintercept = 0)  +
  geom_line() +
  theme_instagram

ggplot(temp, aes(x = u, y = p)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_instagram
  
geom_hline(yintercept = 0, colour = "black")
+
  geom_line(size = 1.2) +
  scale_x_yearmon(expand = c(.01, 0), format = "%YM%m", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Arbeitslosenquote (Österreich)",
       subtitle = "Prozent der Erwerbsbevölkerung",
       caption = "Quelle: AMS, Eurostat. Unbereinigte Daten.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/2020xxxx_unemp_de.jpeg", height = 5, width = 5)
