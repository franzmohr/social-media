rm(list = ls())

lang <- "de"

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

ctry <- "AT"
#ctry <- c("AT", "DE", "EA20", "EU27_2020")
if (lang == "de") {
  #ctry_label <- c("Österreich", "Deutschland", "Euroraum (20)", "Europäische Union")
  var_col_label <- c("Dienstleistungen", "Industriegüter\nohne Energie",
                     "Energie", "Nahrungsmittel")
  var_line_label <- c("HVPI-Inflation", "Kerninflation (ohne Energie, Nahrungsmittel)")
  
  fig_title <- "Zusammensetzung der Inflation in Österreich"
  fig_subtitle <- "Beiträge der Komponenten zur Gesamtinflation in Prozentpunkten"
  fig_caption <- "Quelle: Eurostat. Eigene Berechnungen. Letzter Wert: "
}
if (lang == "en") {
  var_col_label <- c("Services", "Non-energy industrial goods",
                     "Energy", "Food including alcohol and tobacco")
  var_line_label <- c("HCPI-inflation", "Core inflation (w/o energy, food)")
}
var_col_level <- c("SERV", "IGD_NNRG", "NRG", "FOOD")
file_date <- format(Sys.Date(), "%Y%m%d") # For name of exported file

# Weights
weights <- get_eurostat(id = "prc_hicp_iw",
                        filters = list(geo = ctry,
                                       coicop18 = c("FOOD", "NRG", "IGD_NNRG", "SERV")),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time) %>%
  rename(weight = values)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_minr",
                      filters = list(geo = ctry,
                                     unit = "RCH_A",
                                     coicop18 = c("TOTAL", "TOT_X_NRG_FOOD", "FOOD", "NRG", "IGD_NNRG", "SERV")),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= "2019-01-01",
         !is.na(values))

comp <- index %>%
  filter(!coicop18 %in% c("TOTAL", "TOT_X_NRG_FOOD")) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop18")) %>%
  # Calculate weighted averages
  group_by(time, geo) %>%
  mutate(weight = weight / sum(weight)) %>%
  ungroup() %>%
  mutate(values = values * weight) %>%
  #mutate(geo = factor(geo, levels = ctry, labels = ctry_label)) %>%
  mutate(var = factor(coicop18, levels = var_col_level, labels = var_col_label))

line <- index %>%
  filter(coicop18 %in% c("TOTAL", "TOT_X_NRG_FOOD"),
         !is.na(values)) %>%
  #mutate(geo = factor(geo, levels = ctry, labels = ctry_label)) %>%
  mutate(line_var = factor(coicop18, levels = c("TOTAL", "TOT_X_NRG_FOOD"),
                          labels = var_line_label))

max_date <- format(max(line$time), "%YM%m")
fig_caption <- paste0(fig_caption, max_date, ".")


source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var)) +
  geom_line(data = line, aes(linetype = line_var), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_insta +
  #facet_wrap(~var, ncol = 1) +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6)) +
  theme(legend.box = "vertical")

g

ggsave(g, filename = paste0("pics/", file_date, "-inflation-composition-crude-", lang, ".jpeg"), height = 5, width = 5)

