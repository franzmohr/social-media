rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

lang <- "de"

base_year <- 2005
min_year <- 1995

# Penn World Table 11.0 -------------------------------------------------------
# Same local copy as 20260926-gdp-consumption-capital-growth.R: PWT 11.0 is a fixed
# release and dataverse.nl rate-limits repeated downloads.
pwt_cache <- "D:/projects/MacroData/pwt/pwt110.xlsx"
if (!file.exists(pwt_cache)) {
  dir.create(dirname(pwt_cache), recursive = TRUE, showWarnings = FALSE)
  download.file("https://dataverse.nl/api/access/datafile/554105", pwt_cache,
                mode = "wb", quiet = TRUE)
}

# rgdpna: real GDP, emp: persons engaged, avh: annual hours per worker,
# rkna: capital services (index, assets weighted by user cost; includes
# dwellings). Everything is indexed to the base year, so only the development
# within each country is compared, not levels across countries.
ctry_iso <- c("AUT", "DEU", "USA")

temp <- read_excel(pwt_cache, sheet = "Data") %>%
  filter(countrycode %in% ctry_iso, year >= min_year) %>%
  transmute(iso = countrycode, year,
            y_worker = rgdpna / emp,
            y_hour = rgdpna / (emp * avh),
            hours = avh,
            k_hour = rkna / (emp * avh)) %>%
  filter(if_all(c(y_worker, y_hour, hours, k_hour), ~ !is.na(.x))) %>%
  pivot_longer(-c(iso, year), names_to = "var", values_to = "value") %>%
  group_by(iso, var) %>%
  mutate(value = value / value[year == base_year] * 100) %>%
  ungroup()

max_year <- max(temp$year)

if (lang == "de") {
  ctry <- c(AUT = "Österreich", DEU = "Deutschland", USA = "USA")
  panel_names <- c(y_worker = "BIP je Erwerbstätigen", y_hour = "BIP je Arbeitsstunde",
                   hours = "Arbeitsstunden je Erwerbstätigen", k_hour = "Kapitaleinsatz je Arbeitsstunde")
  fig_title <- "Mehr Kapital, kaum mehr Output je Erwerbstätigen"
  fig_subtitle <- paste0("Österreich, Deutschland und USA, real, Index ", base_year, " = 100.\n",
                         "Je Stunde hält Österreich mit, je Erwerbstätigen nicht: es wird weniger gearbeitet.")
  src <- "Penn World Table 11.0 (rgdpna, emp, avh, rkna)."
  fig_note <- paste0("Kapitaleinsatz: Kapitaldienstleistungen einschließlich Wohnbauten. ",
                     "Eigene Skala je Feld. ", max_year, ": Rezessionsjahr in Österreich und Deutschland.")
} else {
  ctry <- c(AUT = "Austria", DEU = "Germany", USA = "USA")
  panel_names <- c(y_worker = "GDP per worker", y_hour = "GDP per hour worked",
                   hours = "Hours per worker", k_hour = "Capital input per hour worked")
  fig_title <- "More capital, barely more output per worker"
  fig_subtitle <- paste0("Austria, Germany and the US, real, index ", base_year, " = 100.\n",
                         "Per hour Austria keeps up, per worker it does not: people work fewer hours.")
  src <- "Penn World Table 11.0 (rgdpna, emp, avh, rkna)."
  fig_note <- paste0("Capital input: capital services including dwellings. ",
                     "Own scale per panel. ", max_year, ": recession year in Austria and Germany.")
}

temp <- temp %>%
  mutate(name = factor(ctry[iso], levels = unname(ctry)),
         var = factor(panel_names[var], levels = unname(panel_names)))

temp_last <- temp %>%
  filter(year == max_year)

source("r-corporate-design-functions-ggplot2.R")

ctry_colours <- setNames(unname(design_colours()[c("violet", "teal", "red")]), unname(ctry))

g <- ggplot(temp, aes(x = year, y = value, colour = name)) +
  geom_hline(yintercept = 100, colour = design_colours()[["ink"]], linewidth = .4) +
  geom_vline(xintercept = base_year, colour = design_colours()[["grid"]], linewidth = .5) +
  geom_line(linewidth = .9) +
  ggrepel::geom_text_repel(data = temp_last, aes(label = round(value)),
                           hjust = 0, direction = "y", nudge_x = 1,
                           xlim = c(max_year + .5, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .15, size = 3, fontface = "bold",
                           family = font_corporate_design, seed = 1, show.legend = FALSE) +
  facet_wrap(~ var, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = ctry_colours) +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     labels = function(x) ifelse(x %% 10 == 5, "", x),
                     limits = c(min_year, max_year + 5),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        panel.spacing.x = unit(16, "pt"),
        panel.spacing.y = unit(14, "pt"))

g

save_chart(g, "output-hours-capital", lang = lang, format = "portrait")
