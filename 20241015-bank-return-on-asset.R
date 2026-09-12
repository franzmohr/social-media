
rm(list = ls())

# Choose language
lang <- "en"

# Choose countries
ctry_ecb <- c("U2", "AT", "DE", "FR")
ctry <- c("Euro area", "Austria", "Germany", "France")


library(dplyr)
library(ecb)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)


if (lang == "de") {
  temp_labels <- "Frühere Jahre"
  temp_title <- "Bankensektor: Return on Assets"
  temp_caption <- "Quelle: EZB-CBD2. Konsolidierte Daten."
}
if (lang == "en") {
  temp_labels <- "Earlier years"
  temp_title <- "Euro area banking sector: Return on assets"
  temp_caption <- "Source: ECB-CBD2. Consolidated data."
}


# Download data
ctry_temp <- paste0(ctry_ecb, collapse = "+")
raw <- get_data(paste0("CBD2.Q.", ctry_temp, ".W0.57+67._Z._Z.A.A.I2004._Z._Z._Z._Z._Z._Z.PC"))

result <- raw %>%
  select(date = obstime,
         geo = ref_area,
         value = obsvalue) %>%
  filter(!is.na(value)) %>%
  mutate(yr = substring(date, 1, 4),
         value = value / 100) %>%
  group_by(geo) %>%
  mutate(maxyr = max(yr)) %>%
  group_by(yr, geo) %>%
  filter(n() == 4 | yr == maxyr) %>%
  ungroup() %>%
  mutate(qr = substring(date, 6, 7),
         date = as.Date(as.yearqtr(date, "%Y-Q%q")),
         is_max = case_when(yr == maxyr ~ yr,
                            yr == as.numeric(maxyr) - 1 ~ yr,
                            TRUE ~ "earlier"),
         geo = factor(geo,
                      levels = ctry_ecb,
                      labels = ctry))

recent <- result %>%
  group_by(geo) %>%
  filter(date == max(date)) %>%
  ungroup()

max_yr <- recent %>%
  pull("yr") %>%
  unique()

last_yr <- as.numeric(max_yr) - 1

result <- result %>%
  mutate(highlt = factor(is_max,
                         levels = c(max_yr, last_yr, "earlier"),
                         labels = c(max_yr, last_yr, temp_labels)))

source("theme_franz.R")

g <- ggplot(result, aes(x = qr, y = value, group = yr)) +
  geom_line(aes(colour = highlt, linetype = highlt), linewidth = .9) +
  geom_point(data = recent, colour = franz_colours[["red"]], size = 2,
             show.legend = FALSE) +
  scale_x_discrete(expand = c(.05, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  # the current year in the brand red, last year for reference, the rest as context
  scale_colour_manual(values = unname(franz_colours[c("red", "blue", "mute")])) +
  scale_linetype_manual(values = c(1, 2, 1)) +
  facet_wrap(~ geo) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank())

g
save_post(g, "bank-return-on-asset", lang = lang, format = "portrait")
