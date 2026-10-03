
rm(list = ls())


# Choose language
lang <- "en"

# Choose countries
ctry_eurostat <- c("AT", "IE", "ES", "EL", "PT", "DE", "EA")

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)

min_date <- "2000-01-01"

levels_sctr <- c("F", "TOTAL")
levels_unit <- c("THS_PER", "THS_HW")

perc <- c(.25, .75)

if (lang == "de") {
  temp_title <- "Beschäftigung im Bausektor"
  fig_subtitle <- "Anteil an der Gesamtbeschäftigung"
  labels_unit <- c("Personen", "Arbeitsstunden")
  fig_caption <- "Quelle: Eurostat. Letzter Wert: "
  labels_sctr <- c("Baugewerbe/Bau",
                     "TOTAL")
}
if (lang == "en") {
  fig_title <- "Share of construction in total employment"
  labels_unit <- c("Calculated based on number of persons in the sector", "Calculated based on hours worked in the sector")
  fig_caption <- paste0("Source: Eurostat. Where available, seasonally adjusted series were used, otherwise the data is not seasonally adjusted.\n",
                        "Ribbons describe the range between the ", min(perc) * 100, "% and ", max(perc) * 100, "% percentile in the EU. ",
                        "Last value: ")
  labels_sctr <- c("Construction",
                     "TOTAL")
}


eu_ctry <- pull(eurostat::eu_countries, "code")
eu_ctry <- c(eu_ctry, "EA")

eu_data <- get_eurostat(id = "namq_10_a10_e",
                     filters = list(geo = eu_ctry,
                                    na_item = "EMP_DC",
                                    s_adj = c("NSA", "SCA"),
                                    unit = c("THS_PER", "THS_HW")),
                     cache = FALSE) %>%
  filter(!is.na(values),
         nace_r2 %in% levels_sctr) %>%
  mutate(s_adj = factor(s_adj, levels = c("SCA", "NSA"))) %>%
  arrange(s_adj) %>%
  group_by(time, geo, unit, nace_r2) %>% slice(1) %>% ungroup() %>%
  mutate(geo = case_when(geo == "EL" ~ "GR",
                         TRUE ~ geo)) %>%
  select(time, geo, unit, nace_r2, values) %>%
  pivot_wider(names_from = "nace_r2", values_from = "values") %>%
  pivot_longer(cols = -c("time", "geo", "unit", "TOTAL")) %>%
  filter(!is.na(value)) %>%
  mutate(value = value / TOTAL) %>%
  rename(date = time,
         ctry = geo,
         sctr = name) %>%
  select(date, ctry, sctr, unit, value) %>%
  filter(date >= min_date) %>%
  mutate(sctr = factor(sctr, levels = levels_sctr, labels = labels_sctr),
         unit = factor(unit, levels = levels_unit, labels = labels_unit))

temp_ribbon <- eu_data %>%
  filter(!ctry %in% c("EA")) %>%
  group_by(date, sctr, unit) %>%
  summarise(ymin = quantile(value, min(perc)),
            ymax = quantile(value, max(perc)),
            .groups = "drop")

temp_selection <- temp_ribbon %>%
  filter(date <= "2010-01-01",
         sctr == "Construction") %>%
  select(date, sctr, unit, ymax)

temp_selection <- eu_data %>%
  left_join(temp_selection, by = c("date", "sctr", "unit")) %>%
  filter(!is.na(ymax)) %>%
  filter(value > ymax) %>%
  group_by(ctry) %>%
  summarise(nobs = n(),
            .groups = "drop") %>%
  arrange(desc(nobs)) %>%
  slice(1:5) %>%
  pull("ctry")

temp_line <- eu_data %>%
  filter(ctry %in% c("AT", temp_selection))
  

max_date <- format(as.yearqtr(max(temp_line$date)), "%YQ%q")
fig_caption <- paste0(fig_caption, max_date, ".")

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(temp_ribbon, aes(x = date)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = .3) +
  geom_line(data = temp_line, aes(y = value, colour = ctry)) +
  facet_wrap(~ unit, ncol = 1) +
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_colour_insta +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = fig_title,
       caption = fig_caption) +
  theme_instagram +
  theme(strip.text = element_text(size = 6),
        axis.title = element_blank(),
        axis.text = element_text(size = 6),
        plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 6))
g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-employment-in-construction.jpeg"), height = 5, width = 5)
