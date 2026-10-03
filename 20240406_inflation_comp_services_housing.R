rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- c("AT", "EA20")
file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file

cois <- c("CP041", "CP0432", "CP0442", "CP0443",
          "CP04441", "CP04442", "CP04449", "CP05123", "CP0513", "CP05204",
          "CP0533", "CP05404", "CP0562", "CP1252")

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry,
                                       coicop = cois),
                        cache = FALSE)

weights <- weights %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  mutate(weight = weight / 1000)

# weights %>%  pivot_wider(names_from = "coicop", values_from = "weight")

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry,
                                     coicop = cois),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values)) %>%
  select(-freq)

comp <- index %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  mutate(values = values * weight) %>%
  #filter(geo == "AT") %>%
  select(-weight) %>%
  #pivot_wider(names_from = "coicop", values_from = "values") %>%
  #mutate(value = SERV_COM + SERV_HOUS + SERV_MSC + SERV_REC + SERV_TRA)
  mutate(var_de = factor(coicop, levels = cois),
         geo = factor(geo, levels = ctry, labels = c("Österreich", "Euroraum (20)")))

source("theme_instagram.R")

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_hline(yintercept = 0) +
  geom_line(aes(colour = geo), alpha = 1) +
  scale_x_date(expand = c(.01, 0), date_breaks = "6 months", date_labels = "%YM%m") +
  scale_colour_insta +
  facet_wrap(~var_de, ncol = 3) +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Inflation im Dienstleistungssektor (Wohnkosten)",
       subtitle = "Beiträge der Komponenten zur Gesamtinflation in Prozentpunkten",
       caption = "Quelle: Eurostat. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")

g

ggsave(g, filename = paste0("pics/", file_date, "_infl_comp_services_housing.jpeg"), height = 5, width = 5)

