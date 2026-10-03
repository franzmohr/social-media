
# Franz Mohr
# January 2024

rm(list = ls())

# Präambel ----
library(ecb)
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

# Credit to households for consumption and house purchase ----
raw <- get_data("MIR.M..B.A2B+A2C.A.B.A.2250.EUR.N")

temp <- raw %>%
  mutate(var = bs_item) %>%
  select(date = obstime, ctry = ref_area, var, value = obsvalue) %>%
  filter(!is.na(value)) %>%
  #filter(ctry == "AT") %>%
  arrange(date, ctry) %>%
  mutate(date = as.Date(paste0(date, "-01")),
         yr = substring(date, 1, 4)) %>%
  filter(yr >= "2022") %>%
  # Full years or latest year
  group_by(yr, ctry, var) %>%
  filter(yr == max(yr) | n() == 12) %>%
  ungroup() %>%
  # Get avg value of first year
  group_by(yr, ctry, var) %>%
  mutate(tot_avg = sum(value) / n()) %>%
  group_by(ctry, var) %>%
  mutate(tot_avg = tot_avg[1]) %>%
  ungroup() %>%
  # Calculate index
  mutate(idx = value / tot_avg * 100,
         var = factor(var, levels = c("A2B", "A2C"),
                      labels = c("Consumption", "House purchase")))

temp_ribbon <- temp %>%
  group_by(date, var) %>%
  summarise(ymin = quantile(idx, .25),
            ymax = quantile(idx, .75),
            .groups = "drop")

temp_line <- temp %>%
  #filter(!ctry %in% c("AT", "HR"))
  filter(ctry %in% c("AT", "DE"))

ggplot(temp_ribbon, aes(x = date)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .2) +
  geom_hline(yintercept = 100) +
  geom_line(data = temp_line, aes(y = idx, colour = ctry)) +
  facet_wrap(~var) +
  labs(title = "New credit") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# ggsave(g, filename = "pics/20230828_interestrates_hh.png", height = 5.5, width = 15)

