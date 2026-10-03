rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

hl_country <- c("AT", "EU")
hl_country_labels <- c("Österreich", "EU")
hl_other_labels <- "Andere Länder"

sector_levels <- c("B-E", "F", "G-I", "M_N")
sector_labels <- c("Industrie\n(ohne Baugewerbe)", "Baugewerbe/Bau",
                   "Handel, Instandhaltung,\nVerkehr, Gastgewerbe", "Sonstige nichtfinanzielle\nDienstleistungen")

var_levels <- c("RLPR_HW", "RLPR_PER", "NULC_HW", "HW_EMP")
var_labels <- c("Reale\nArbeitsproduktivität\nje Arbeitsstunde",
                "Reale\nArbeitsproduktivität\nje Erwerbstätigen",
                "Nominale Lohnstück-\nkosten (auf Basis von\nArbeitsstunden)",
                "Arbeitsstunden\npro erwerbs-\ntätiger Person")

# sectoral data
sector_prod <- get_eurostat(id = "nama_10_lp_a21",
                              filters = list(nace_r2 = sector_levels,
                                             na_item = var_levels,
                                             unit = "I15"),
                              cache = FALSE) %>%
  rename(date = time,
         ctry = geo,
         sector = nace_r2,
         var = na_item,
         value = values) %>%
  filter(!is.na(value))

temp <- sector_prod %>%
  filter(substring(ctry, 1, 2) != "EU" | ctry == "EU27_2020",
         substring(ctry, 1, 2) != "EA") %>%
  mutate(ctry = case_when(ctry == "EU27_2020" ~ "EU",
                          TRUE ~ ctry)) %>%
  group_by(ctry, var, sector) %>%
  filter(any(date == "2005-01-01")) %>%
  ungroup() %>%
  filter(date >= "2005-01-01") %>%
  group_by(ctry, var, sector) %>%
  mutate(value = value / value[1] * 100) %>%
  ungroup() %>%
  mutate(hl = ifelse(ctry %in% hl_country, ctry, "other"),
         hl = factor(hl, levels = c(hl_country, "other"),
                     labels = c(hl_country_labels, hl_other_labels)),
         al = ctry %in% hl_country,
         var = factor(var, levels = var_levels, labels = var_labels),
         sector = factor(sector, levels = sector_levels, labels = sector_labels))

top_temp <- temp %>%
  group_by(var, sector, ctry) %>%
  filter(date == max(date)) %>%
  group_by(var, sector) %>%
  arrange(desc(value)) %>%
  mutate(id = 1:n(),
         use = ctry %in% hl_country | id <= 3) %>%
  filter(use) %>%
  select(sector, ctry, var, use, date)

temp <- temp %>%
  left_join(top_temp, by = c("ctry", "var", "sector", "date")) %>%
  mutate(labeltext = ifelse(!is.na(use), ctry, ""))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_hline(yintercept = 100, colour = "black") +
  geom_line(aes(colour = hl, alpha = al, group = ctry), linewidth = .4, show.legend = FALSE) +
  geom_text(aes(label = labeltext), size = 1.2, nudge_x = 370, hjust = 1) +
  scale_x_date(expand = c(.01, 0), date_label = "%Y", date_breaks = "2 years") +
  scale_alpha_manual(values = c(.1, 1)) +
  guides(colour = guide_legend(nrow = 1), alpha = "none") +
  facet_grid(var ~ sector, scales = "free_y") +
  labs(title = "Sektorale Produktivitätsentwicklung im internationalen Vergleich",
       subtitle = "Index (2005 = 100)",
       caption = "Quelle: Eurostat. Eigene Berechnungen. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))
g


date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "_productivity_by_sector.jpeg"), height = 5, width = 5)

