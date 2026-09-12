
rm(list = ls())

ctry <- "AT"

lang <- "de"

min_date <- "2010-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_franz.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("P61", "P62", "P71", "P72"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P61", "P71", "P62", "P72")
unit_levels <- c("flow", "chg", "growth")

if (lang == "de") {
  var_labels <- c("Exporte von Gütern",
                  "Importe von Gütern",
                  "Exporte von Dienstleistungen",
                  "Importe von Dienstleistungen")
  unit_labels <- c("Bruttoinlandsprodukt\nin Mrd EUR", "Veränderung im Vergleich\nzum Vorquartal in Mrd EUR")
  temp_title <- paste0("Zusammensetzung der realen Im- und Exporte (", ctry, ")")
  fig_subtitle <- "Beitrag zum realen Bruttoinlandsprodukt in Mrd EUR"
  temp_caption <- "Quelle: Eurostat. Quartalswerte. Saison- und kalenderbereinigte Daten. Basierend auf Preisen von 2015."
}

if (lang == "en") {
  var_labels <- c("Exports of goods",
                  "Imports of goods",
                  "Exports of services",
                  "Imports of services")
  temp_title <- paste0("Real exports and imports (", ctry, ")")
  fig_subtitle <- "Contribution to real gross domestic product in bn EUR"
  temp_caption <- "Source: Eurostat. Quarterly data. Seasonally and calendar adjusted data. Based on 2015 prices."
}

temp <- raw %>%
  select(date = time, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  na.omit() %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date"), values_to = "flow", names_to = "var") %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels),
         flow = flow / 1000) %>%
  arrange(date) %>%
  pivot_longer(cols = -c("date", "var"), names_to = "unit") %>%
  mutate(unit = factor(unit, levels = c("flow", "chg"), labels = unit_labels))


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
  scale_x_date(expand = c(.01, 0), date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(position = "right") +
  scale_fill_franz() +
  facet_wrap(~ var, ncol = 2) +
  labs(title = temp_title,
       subtitle = fig_subtitle,
       caption = temp_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g
save_post(g, "gdp-exports-and-imports-levels", lang = lang, format = "portrait")

