
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

inflation <- get_eurostat("prc_hicp_ainr",
                          filters = list(geo = ctry,
                                         coicop18 = "TOTAL",
                                         unit = "INX_A_AVG"),
                          cache = FALSE) %>%
  select(time, geo, prices = values)

raw <- get_eurostat("gov_10a_exp",
                    filters = list(geo = ctry,
                                   sector = "S13",
                                   na_item = "TE",
                                   unit = c("PC_GDP", "MIO_EUR")),
                    cache = FALSE) %>%
  filter(!is.na(values),
         nchar(cofog99) == 6,
         time >= min_date)

temp <- raw %>%
  rename(var = cofog99) %>%
  select(time, geo, var, unit, values) %>%
  pivot_wider(names_from = "unit", values_from = "values") %>%
  left_join(inflation, by = c("time", "geo")) %>%
  mutate(prices = prices / 100,
         MIO_EUR_NOM = MIO_EUR,
         MIO_EUR_REAL = MIO_EUR_NOM * prices) %>%
  select(-MIO_EUR, -prices) %>%
  pivot_longer(cols = -c("time", "var", "geo"), names_to = "unit", values_to = "values")


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

top_var <- temp %>%
  filter(geo == "AT",
         time == "2021-01-01",
         unit == "PC_GDP") %>%
  arrange(desc(values)) %>%
  slice(1:8) %>%
  left_join(cofog, by = c("var" = "code")) %>%
  pull("name")

if (lang == "de") {
  temp_title <- "Ausgaben des Staates nach Aufgabenbereichen (Österreich)"
  temp_caption <- "Quelle: Eurostat. Summe über Bund, Länder, Gemeinden und Sozialversicherung."
  temp_other <- "Andere"
  unit_name <- c("Ln Mrd EUR (nominal)", "Ln Mrd EUR (real, 2025 Preise = 100%)", "Mrd EUR (nominal)", "Mrd EUR (real, 2025 Preise = 100%)", "Prozent des BIP")
}

if (lang == "en") {
  temp_title <- "General government expenditure for social protection (Austria)"
  temp_caption <- "Source: Eurostat. Sum over central, state and local government as well as social security funds."
  temp_other <- "Other"
  unit_name <- c("Bn EUR (nominal)", "Bn EUR (real, 2025 prices = 100%)", "Percent of GDP")
}

temp <- temp %>%
  left_join(cofog, by = c("var" = "code")) %>%
  mutate(hl = name %in% top_var,
         col = ifelse(name %in% top_var, name, "other"),
         col = factor(col, levels = c(top_var, "other"), labels = c(top_var, temp_other)),
         unit = factor(unit, levels = c("L_MIO_EUR_NOM", "L_MIO_EUR_REAL", "MIO_EUR_NOM", "MIO_EUR_REAL", "PC_GDP"), labels = unit_name))

max_date <- format(max(pull(temp, "time")), "%Y")

if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", max_date, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last observation: ", max_date, ".")
}

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_col(aes(fill = col)) +
  scale_x_date(expand = c(.01, 0), date_breaks = "3 years", date_labels = "%Y") +
  scale_fill_highlight(highlight = top_var, rest = temp_other) +
  facet_wrap(unit~geo, scales = "free_y", ncol = 3) +
  guides(alpha = "none", colour = guide_legend(ncol = 1)) +
  labs(title = temp_title,
       caption = temp_caption) +
  theme_corporate_design(base_size = 13) +
  theme(
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_blank())

g
save_chart(g, "government-expenditures-top-functions", lang = lang, format = "portrait")

