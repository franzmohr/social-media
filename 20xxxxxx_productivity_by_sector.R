rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

country <- c("AT", "DE", "EU27_2020", "EA")
country_labels <- c("Österreich", "Deutschland", "Europäische Union (27)", "Euroraum")

# sectoral data
sector_prod <- get_eurostat(id = "nama_10_lp_a21",
                              filters = list(geo = country,
                                             na_item = "RLPR_HW",
                                             unit = "I15"),
                              cache = FALSE) %>%
  rename(date = time,
         ctry = geo,
         sector = nace_r2,
         var = na_item,
         value = values) %>%
  filter(!is.na(value),
         nchar(sector) == 1)

temp <- sector_prod %>%
  group_by(ctry, sector) %>%
  filter(any(date == "2005-01-01")) %>%
  ungroup() %>%
  filter(date >= "2005-01-01") %>%
  group_by(ctry, sector) %>%
  mutate(value = value / value[1] * 100) %>%
  ungroup() %>%
  mutate(hl = ifelse(sector == "F", "hl", "other"),
         hl = factor(hl, levels = c("hl", "other"), labels = c("Bausektor", "Andere")),
         ctry = factor(ctry, levels = country, labels = country_labels))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_hline(yintercept = 100, colour = "black") +
  geom_line(aes(colour = hl, alpha = hl, group = sector)) +
  scale_x_date(expand = c(.01, 0), date_label = "%Y", date_breaks = "2 years") +
  scale_alpha_manual(values = c(1, .2)) +
  guides(colour = guide_legend(ncol = 2)) +
  facet_wrap(~ ctry) +
  labs(title = "Produktivitätsentwicklung des Bausektors im europäischen Vergleich",
       subtitle = "Index (2005 = 100)",
       caption = "Arbeitsproduktivität gemessen als reale Wertschöpfung pro Arbeitstunde\n(vgl. https://ec.europa.eu/eurostat/cache/metadata/en/nama_10_prod_esms.htm).\nQuelle: Eurostat. Eigene Berechnungen. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram +
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))
date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "_productivity_by_sector.jpeg"), height = 5, width = 5)

