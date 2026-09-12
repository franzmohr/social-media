rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)

lang <- "de"

min_year <- 1990

countries <- c(AUT = "Österreich", DEU = "Deutschland", FRA = "Frankreich",
               ITA = "Italien", SWE = "Schweden", BEL = "Belgien", USA = "USA")

tmp <- tempfile(fileext = ".xlsx")
download.file("https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.xlsx",
              destfile = tmp, mode = "wb")
data <- read_xlsx(tmp)
file.remove(tmp)

temp <- data %>%
  select(country, year, iso_code, value = energy_per_capita) %>%
  filter(!is.na(value), iso_code %in% names(countries), year >= min_year) %>%
  mutate(date = as.Date(paste0(year, "-01-01")),
         # kWh per person -> MWh per person reads better on an axis
         value = value / 1000,
         name = factor(countries[iso_code], levels = unname(countries)))

if (lang == "de") {
  fig_title <- "Energieverbrauch pro Kopf"
  fig_subtitle <- "Primärenergieverbrauch in MWh je EinwohnerIn und Jahr"
  src <- "Our World in Data (Energy Institute, Ember)."
} else {
  countries <- c(AUT = "Austria", DEU = "Germany", FRA = "France", ITA = "Italy",
                 SWE = "Sweden", BEL = "Belgium", USA = "USA")
  temp <- mutate(temp, name = factor(countries[iso_code], levels = unname(countries)))
  fig_title <- "Energy use per person"
  fig_subtitle <- "Primary energy consumption in MWh per capita and year"
  src <- "Our World in Data (Energy Institute, Ember)."
}

ends <- temp %>%
  group_by(name) %>%
  filter(year == max(year)) %>%
  ungroup()

max_year <- max(temp$year)

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = ends, size = 2) +
  ggrepel::geom_text_repel(data = ends, aes(label = name),
                           hjust = 0, nudge_x = 900, direction = "y",
                           size = 4, fontface = "bold", family = franz_font,
                           segment.colour = NA, min.segment.length = Inf,
                           box.padding = .1, seed = 1) +
  scale_x_date(breaks = seq(as.Date(paste0(min_year, "-01-01")),
                            as.Date(paste0(max_year, "-01-01")), by = "10 years"),
               date_labels = "%Y",
               expand = expansion(mult = c(.02, .28))) +
  scale_y_continuous(breaks = scales::breaks_width(20)) +
  scale_colour_manual(values = franz_pal("cat", length(countries)), guide = "none") +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = franz_caption(src, last = max_year, lang = lang)) +
  theme_franz(base_size = 13, grid = "y") +
  theme(axis.title = element_blank())

g

save_post(g, "energy-usage", lang = lang, format = "portrait")
