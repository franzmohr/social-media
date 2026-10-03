rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
min_date <- "1995-01-01"

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("P5G", "P51G", "P52", "P53"),
                                                       unit = c("CP_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P5G", "P51G", "P52", "P53")
var_labels_de <- c(c("Bruttoinvestitionen",
                     "Bruttoanlageinvestitionen",
                     "Vorratsveränderungen",
                     "Nettozugang an Wertsachen"))
var_labels_en <- c(c("Gross capital formation",
                     "Changes in inventories",
                     "Gross fixed capital formation",
                     "Aquisitions and disposals of valuables"))

temp <- raw %>%
  mutate(date = as.yearqtr(time, "%Y-Q%q")) %>%
  select(date, na_item, values)

real <- temp %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  na.omit() %>% # Drop empty rows
  select(-P5G) %>%
  pivot_longer(cols = -c("date")) %>%
  filter(date >= min_date) %>%
  filter(!is.na(value)) %>%
  mutate(name_de = factor(name, levels = var_levels,
                          labels = var_labels_de),
         name_en = factor(name, levels = var_levels,
                          labels = var_labels_en),
         value = value / 1000)

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_de), alpha = 1) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_colour_manual(values = "black") +
  scale_fill_corporate_design() +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Investitionen (Österreich)",
       subtitle = "Mrd EUR (aktuelle Preise, Quartalsdaten)",
       caption = "Quelle: Eurostat. Saison- und kalenderbereinigte Daten.\nCode unter https://github.com/franzmohr/instagram.") +
  theme_corporate_design(base_size = 13)

g

save_chart(g, "investment-components-level", lang = "de", format = "portrait")


g <- ggplot(real, aes(x = date, y = value)) +
  geom_col(aes(fill = name_en), alpha = 1) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_corporate_design() +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Investment (Austria)",
       subtitle = "Bn EUR (current prices, quarterly data)",
       caption = "Source: Eurostat. Seasonally and calendar adjusted data.\nCode available at https://github.com/franzmohr/instagram.") +
  theme_corporate_design(base_size = 13)

save_chart(g, "investment-components-level", lang = "en", format = "portrait")

