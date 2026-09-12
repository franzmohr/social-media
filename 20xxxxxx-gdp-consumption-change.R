
rm(list = ls())

ctry <- "AT"

lang <- "de"

min_date <- "2018-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_franz.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp",
                    filters = list(geo = ctry,
                                   na_item = c("P31_S13", "P31_S14", "P31_S15", "P32_S13"),
                                   unit = c("CLV20_MEUR"),
                                   s_adj = c("SCA")),
                    cache = FALSE)



var_levels <- c("P31_S14", "P31_S15", "P31_S13", "P32_S13")
unit_levels <- c("flow", "chg")

if (lang == "de") {
  var_labels <- c("Private Haushalte",
                  "Priv. Organisationen ohne Erwerbszweck",
                  "Staat (Individualverbrauch)",
                  "Staat (Kollektivverbrauch)")
  unit_labels <- c("Bruttowertschäftung in Mrd EUR", "Veränderung im Vergleich\nzum Vorquartal in Mrd EUR")
  temp_title <- paste0("Realer Konsum (", ctry, ")")
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten."
}

if (lang == "en") {
  var_labels <- c("Private Haushalte",
                  "Priv. Organisationen ohne Erwerbszweck",
                  "Staat (Individualverbrauch)",
                  "Staat (Kollektivverbrauch)")
  unit_labels <- c("Gross value added in bn EUR", "Change from previous\nquarter in bn EUR")
  temp_title <- paste0("Real consumption (", ctry, ")")
  temp_caption <- "Source: Eurostat. Seasonally and calendar adjusted data."
}

temp <- raw %>%
  select(date = time, na_item, values) %>%
  filter(!is.na(values)) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  pivot_longer(cols = -c("date"), values_to = "flow", names_to = "var") %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels),
         flow = flow / 1000) %>%
  arrange(date) %>%
  group_by(var) %>%
  mutate(chg = flow - lag(flow, 1)) %>%
  ungroup() %>%
  filter(!is.na(chg)) %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date", "var"), names_to = "unit") %>%
  mutate(unit = factor(unit, levels = unit_levels, labels = unit_labels))


last_value <- format(as.yearqtr(max(temp$date)), "%YQ%q")
if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last value: ", last_value, ".")
}

max_value <- max(temp$value)

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_zeroline() +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(position = "right") +
  scale_fill_franz() +
  facet_grid(unit ~ var, scales = "free_y", switch = "y") +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_blank())
save_post(g, "gdp-consumption-change", lang = lang, format = "landscape")

