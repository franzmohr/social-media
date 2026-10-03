rm(list = ls())


ctry <- "AT"

lang <- "de"


library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_instagram.R")

min_date <- "2015-01-01"


# Download data
raw <- get_eurostat(id = "namq_10_an6", filters = list(geo = ctry,
                                                       unit = "CLV15_MEUR",
                                                       s_adj = "SCA"),
                    cache = FALSE) %>%
  filter(!is.na(values)) %>%
  mutate(values = values / 1000)

var_levels <- c("N111G", "N112G", "N1131G", "N1132G", "N11OG", "N115G", "N117G")

if (lang == "de") {
  var_labels <- c("Wohnbauten",
                  "Nichtwohnbauten",
                  "Fahrzeuge",
                  "Ausrüstungen der Informations-\nund Kommunikationstechnik",
                  "Sonstige Ausrüstungen\nund Waffensysteme",
                  "Nutztiere und Nutzpflanzungen",
                  "Geistiges Eigentum")
  temp_title <- "Bruttoanlageinvestitionen nach Anlagearten (Österreich)"
  temp_subtitle <- "Mrd EUR (2015 Preise, Quartalswerte)"
  temp_caption <- "Quelle: Eurostat. Saison- und kalenderbereinigte Daten."
}


temp <- raw %>%
  mutate(var = asset10) %>%
  filter(!var %in% c("N11G", "N11KG", "N11MG")) %>%
  filter(time >= min_date) %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels))

last_value <- format(as.yearqtr(max(temp$time)), "%YQ%q")
if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last value: ", last_value, ".")
}

max_value <- max(temp$values)

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_col(aes(fill = var), show.legend = FALSE) +
  facet_wrap(~var) +
  scale_x_date(expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, max_value * 1.06), expand = c(0, 0)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme_instagram +
  scale_fill_insta

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-investment-by-function-levels-", lang, ".jpeg"), height = 7, width = 7)

