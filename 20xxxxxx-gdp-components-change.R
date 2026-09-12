
rm(list = ls())

ctry <- c("AT", "DE", "EA20", "IT", "FR")

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
                                                       na_item = c("B1GQ", "P3_S13", "P31_S14_S15",
                                                                   "P5G", "P6", "P7"),
                                                       unit = c("CLV15_MEUR"),
                                                       s_adj = c("SCA")),
                    cache = FALSE)


var_levels <- c("P31_S14_S15", "P5G", "P3_S13", "P6", "P7", "PNX", "PX")

if (lang == "de") {
  var_labels <- c("Privater Konsum",
                  "Bruttoinvestitionen",
                  "Öffentlicher Konsum",
                  "Exporte", "Importe (-)",
                  "Nettoexporte",
                  "Sonstige")
  fig_title <- "Reales BIP nach Komponenten"
  fig_subtitle <- "Beitrag zum realen Bruttoinlandsprodukt in Mrd EUR"
  fig_caption <- "Quelle: Eurostat. Quartalswerte. Saison- und kalenderbereinigte Daten.\nBasierend auf Preisen von 2015."
}

# if (lang == "en") {
#   var_labels <- c("Private consumption",
#                   "Gross investment",
#                   "Public consumption",
#                   "Exports", "Imports (-)",
#                   "Net exports",
#                   "Other")
#   unit_labels <- c("Gross domestic product in bn EUR", "Change from previous\nquarter in bn EUR")
#   temp_title <- paste0("Real GDP by component (", ctry, ")")
#   temp_caption <- "Source: Eurostat. Quarterly data. Seasonally and calendar adjusted data.\nBased on 2015 prices."
# }

temp <- raw %>%
  select(date = time, geo, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  mutate(PNX = P6 - P7) %>% # Net exports
  #select(-PNX) %>%
  #select(-P6, -P7) %>% # Drop redundant columns
  na.omit() %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date", "geo", "B1GQ"), values_to = "flow", names_to = "var") %>%
  mutate(var = factor(var, levels = var_levels, labels = wrap_labels(var_labels, 13)),
         flow = flow / 1000) %>%
  arrange(date) %>%
  select(-B1GQ) %>%
  pivot_longer(cols = -c("date", "geo", "var"), names_to = "unit")

last_value <- format(as.yearqtr(max(temp$date)), "%YQ%q")
if (lang == "de") {
  fig_caption <- paste0(fig_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  fig_caption <- paste0(fig_caption, " Last value: ", last_value, ".")
}

max_value <- max(temp$value)

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_zeroline() +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0), date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(position = "right") +
  scale_fill_franz() +
  facet_grid(geo ~ var, scales = "free_y", switch = "y") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_franz(base_size = 9) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g
save_post(g, "gdp-components-international-comparison", lang = lang, format = "portrait")

