
rm(list = ls())

ctry <- "AT"

lang <- "de"

min_date <- "2010-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp", filters = list(geo = ctry,
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PNX", "PX")
unit_levels <- c("flow", "chg", "growth")

if (lang == "de") {
  var_labels <- c("Privater Konsum",
                  "Bruttoinvestitionen",
                  "Öffentlicher Konsum",
                  "Exporte", "Importe (-)",
                  "Nettoexporte",
                  "Sonstige")
  unit_labels <- c("Bruttoinlandsprodukt\nin Mrd EUR", "Veränderung im Vergleich\nzum Vorquartal in Mrd EUR")
  temp_title <- paste0("Reales BIP nach Komponenten (", ctry, ")")
  temp_caption <- "Quelle: Eurostat. Quartalswerte. Saison- und kalenderbereinigte Daten.\nBasierend auf Preisen von 2015."
}

if (lang == "en") {
  var_labels <- c("Private consumption",
                  "Gross investment",
                  "Public consumption",
                  "Exports", "Imports (-)",
                  "Net exports",
                  "Other")
  unit_labels <- c("Gross domestic product in bn EUR", "Change from previous\nquarter in bn EUR")
  temp_title <- paste0("Real GDP by component (", ctry, ")")
  temp_caption <- "Source: Eurostat. Quarterly data. Seasonally and calendar adjusted data.\nBased on 2015 prices."
}

temp <- raw %>%
  select(date = time, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  mutate(PNX = P6 - P7) %>% # Net exports
  #select(-PNX) %>%
  #select(-P6, -P7) %>% # Drop redundant columns
  na.omit() %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date", "B1GQ"), values_to = "flow", names_to = "var") %>%
  mutate(var = factor(var, levels = var_levels, labels = wrap_labels(var_labels, 13)),
         flow = flow / 1000) %>%
  arrange(date) %>%
  group_by(var) %>%
  mutate(chg = flow - lag(flow, 1)) %>%
  ungroup() %>%
  select(-B1GQ) %>%
  filter(!is.na(chg)) %>%
  pivot_longer(cols = -c("date", "var"), names_to = "unit") %>%
  mutate(unit = factor(unit, levels = c("flow", "chg"), labels = wrap_labels(unit_labels, 16)))


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
  scale_fill_corporate_design() +
  facet_grid(unit ~ var, scales = "free_y", switch = "y") +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_corporate_design(base_size = 9) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g
save_chart(g, "gdp-components-level-and-change-at", lang = lang, format = "landscape")

