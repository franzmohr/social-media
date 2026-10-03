
rm(list = ls())

# Choose language
lang <- "de" # "de" or "en"

# Choose country
ctry <- "AT"

min_date <- "1995-01-01"

# Load packages
library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

source("r-corporate-design-functions-ggplot2.R")


cofog <- read.csv("cofog-mapping.csv")
if (lang == "de") {
  cofog <- select(cofog, code, name = name_de)
}
if (lang == "en") {
  cofog <- select(cofog, code, name = name_en)
}

cofog_high_level <- cofog %>%
  filter(nchar(code) == 4) %>%
  rename(high_level_code = code,
         high_level_name = name)

cofog_lower_level <- cofog %>%
  filter(nchar(code) == 6)

cofog <- cofog_lower_level %>%
  mutate(high_level_code = substring(code, 1, 4)) %>%
  left_join(cofog_high_level, by = "high_level_code") %>%
  mutate(name = paste0(high_level_name, ": ", name)) %>%
  select(code, name)




raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = "MIO_EUR"),
                    cache = FALSE) %>%
  filter(!is.na(values),
         nchar(cofog99) == 6,
         time >= min_date)

temp <- raw %>%
  rename(var = cofog99) %>%
  select(time, geo, var, unit, values) %>%
  pivot_wider(names_from = "unit", values_from = "values") %>%
  filter(time == max(time)) %>%
  left_join(inflation, by = c("time", "geo")) %>%
  mutate(MIO_EUR_NOM = MIO_EUR / 1000) %>%
  select(-MIO_EUR, -prices) %>%
  pivot_longer(cols = -c("time", "var", "geo"), names_to = "unit", values_to = "values") %>%
  left_join(cofog, by = c("var" = "code"))


top_var <- temp %>%
  arrange(desc(values)) %>%
  mutate(cshare = cumsum(values) / sum(values)) %>%
  filter(cshare <= .95) %>%
  pull("name")

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_title <- "Ausgaben des Staates nach Aufgabenbereichen (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
  temp_other <- "Andere"
}

if (lang == "en") {
  temp_title <- "General government expenditure for social protection (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
  temp_other <- "Other"
}

temp <- temp %>%
  mutate(name = ifelse(name %in% top_var, name, "other"),
         name = factor(name, levels = c(top_var, "other"), labels = c(top_var, temp_other)))


ggplot(temp, aes(x = name, y = values)) +
  geom_col(aes(fill = "a"), show.legend = FALSE) +
  scale_fill_corporate_design() +
  coord_flip() +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_corporate_design(base_size = 13) +
  theme(axis.title = element_blank())

g
save_chart(g, "government-expenditures-top-functions-current", lang = lang, format = "portrait")

