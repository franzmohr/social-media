
rm(list = ls())

ctry <- c("AT", "DE", "EA20", "FR", "IT")

lang <- "de"

min_date <- "2010-01-01"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

# Download data
raw <- get_eurostat(id = "namq_10_gdp",
                    filters = list(geo = ctry,
                                   na_item = c("P5G", "P51G", "P61", "P62"),
                                   unit = c("CLV15_MEUR"),
                                   s_adj = c("SCA")),
                    cache = FALSE)

var_levels <- c("P5G", "P51G", "P61", "P62")

if (lang == "de") {
  var_labels <- c("Bruttoinvestitionen",
                  "Bruttoanlageinvestitionen",
                  "Exporte von Gütern",
                  "Exporte von Dienstleistungen")
  fig_title <- paste0("Zusammensetzung der realen Investitionen und Exporte (", ctry, ")")
  fig_subtitle <- "Beitrag zum realen Bruttoinlandsprodukt in Mrd EUR"
  fig_caption <- "Quelle: Eurostat. Quartalswerte. Saison- und kalenderbereinigte Daten. Basierend auf Preisen von 2015."
}

# if (lang == "en") {
#   var_labels <- c("Exports of goods",
#                   "Imports of goods",
#                   "Exports of services",
#                   "Imports of services")
#   temp_title <- paste0("Real exports and imports (", ctry, ")")
#   fig_subtitle <- "Contribution to real gross domestic product in bn EUR"
#   temp_caption <- "Source: Eurostat. Quarterly data. Seasonally and calendar adjusted data. Based on 2015 prices."
# }

temp <- raw %>%
  select(date = time, geo, na_item, values) %>%
  pivot_wider(names_from = "na_item", values_from = "values") %>%
  na.omit() %>%
  filter(date >= min_date) %>%
  pivot_longer(cols = -c("date", "geo"), values_to = "flow", names_to = "var") %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels),
         flow = flow / 1000) %>%
  arrange(date) %>%
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
  scale_fill_corporate_design() +
  facet_grid(geo ~ var, scales = "free_y") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_corporate_design(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g
save_chart(g, "gdp-investments-and-exports-levels", lang = lang, format = "portrait")

