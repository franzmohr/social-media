rm(list = ls())

library(dplyr)
library(ecb)
library(lubridate)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_instagram.R")

raw <- get_data("CBD2.Q..W0.67._Z._Z.A.A.I2410+I2411+I2412._Z._Z._Z._Z._Z._Z.PC")

temp <- raw %>%
  select(date = obstime, value = obsvalue, var = cb_item, ctry = ref_area) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(as.yearqtr(date, "%Y-Q%q")),
         yr = substring(date, 1, 4),
         mnth = quarter(date),
         alph = ifelse(yr == max(yr), yr, "0_Other"))

temp_zero <- temp %>%
  select(var, ctry, yr, alph) %>%
  distinct() %>%
  mutate(value = 0,
         mnth = 0)

temp <- bind_rows(temp, temp_zero) %>%
  filter(yr >= "2017") %>%
  mutate(var = factor(var, levels = c("I2410", "I2411", "I2412"),
                      labels = c("nii", "iinc", "iexp")))

g <- ggplot(temp, aes(x = mnth, y = value / 100, group = yr)) +
  geom_line(aes(alpha = alph)) +
  facet_grid(var ~ ctry) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_insta +
  #labs(title = "Beitrag zum realen BIP-Wachstum (Österreich)",
  #     subtitle = "Änderung in Prozentpunkten im Vergleich zum Vorjahreswert",
  #     caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten.\nCode unter https://github.com/franzmohr/instagram.") +
  theme_instagram

g

ggsave(g, filename = paste0("pics/", date_title, "_gdp_component_growth_de.jpeg"), height = 5, width = 5)


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_en), alpha = 1) +
  geom_line(data = real_agg, aes(colour = "Total"), size = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(values = "black") +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Contribution to real GDP growth (Austria)",
       subtitle = "Percentage point change compared to value in the previous year",
       caption = "Source: Eurostat. Seasonally and calendar adjusted data.\nCode available at https://github.com/franzmohr/instagram.") +
  theme_instagram

ggsave(g, filename = paste0("pics/", date_title, "_gdp_component_growth_en.jpeg"), height = 5, width = 5)

