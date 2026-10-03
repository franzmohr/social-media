rm(list = ls())

# Choose language
lang <- "de" # "de" or "en"

# Choose country
ctry <- "AT"

# Load packages
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

source("theme_instagram.R")

raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = "MIO_EUR"),
                    cache = FALSE) %>%
  filter(!is.na(values))


if (lang == "de") {
  var_names <- c("Krankheit und Erwerbsunfähigkeit", "Alter", "Hinterbliebene",
                 "Familien und Kinder", "Arbeitslosigkeit", "Wohnraum", "Soziale Hilfe",
                 "Angewandte Forschung und\nexperimentelle Entwicklung",
                 "Soziale Sicherung")
  temp_y <- "Mrd EUR"
  temp_title <- "Ausgaben des Staates im Bereich soziale Sicherung (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
}

if (lang == "en") {
  var_names <- c("Sickness and disability", "Old age", "Survivors",
                 "Family and children", "Unemployment", "Housing", "Social exclusion",
                 "R&D Social protection", "Social protection")
  temp_sector <- c("General government", "Central government", "State government",
                   "Local government", "Social security funds")
  temp_y <- "Bn EUR"
  temp_title <- "General government expenditure for social protection (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
}

var_levels <- c("GF1001", "GF1002", "GF1003", "GF1004", "GF1005",
                "GF1006", "GF1007", "GF1008", "GF1009")

temp <- raw %>%
  rename(var = cofog99) %>%
  filter(!is.na(values),
         substring(var, 1, 4) == "GF10",
         var != "GF10") %>%
  mutate(var = factor(var, levels = var_levels, labels = var_names),
         values = values / 1000)

# temp %>%
#   select(time, var, values) %>%
#   pivot_wider(names_from = "var", values_from = "values") %>%
#   tail()

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
}

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_col(aes(fill = var), show.legend = FALSE) +
  scale_x_date(expand = c(.01, 0)) +
  scale_fill_insta +
  facet_wrap(~var, ncol = 3) +
  labs(title = temp_title,
       subtitle = temp_y,
       caption = temp_caption) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-government-expenditures-socialsecurity-", lang, ".jpeg"), height = 7, width = 7)

