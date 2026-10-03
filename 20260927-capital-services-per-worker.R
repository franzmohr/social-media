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

# rkna: capital services at constant 2021 national prices (2021 = 1). Assets
# are weighted by their user cost, so short-lived machinery, IT and software
# count for more than long-lived buildings. An index only: good for growth,
# not for comparing levels across countries.
ctry_iso <- c("AUT", "DEU", "USA")

temp <- read_excel(pwt_cache, sheet = "Data") %>%
  filter(countrycode %in% ctry_iso, year >= min_year) %>%
  transmute(iso = countrycode, year,
            services_per_worker = rkna / emp,
            output_per_services = rgdpna / rkna) %>%
  filter(!is.na(services_per_worker), !is.na(output_per_services)) %>%
  pivot_longer(-c(iso, year), names_to = "var", values_to = "value") %>%
  group_by(iso, var) %>%
  mutate(value = value / value[year == base_year] * 100) %>%
  ungroup()

max_year <- max(temp$year)

if (lang == "de") {
  ctry <- c(AUT = "Österreich", DEU = "Deutschland", USA = "USA")
  panel_names <- c(services_per_worker = "Kapitaleinsatz je Erwerbstätigen",
                   output_per_services = "Produktion je Einheit Kapitaleinsatz")
  fig_title <- "Mehr Kapital je Beschäftigten, weniger Output je Kapital"
  fig_subtitle <- paste0("Index ", base_year, " = 100. Kapitaleinsatz: Kapitaldienstleistungen, Anlagen\n",
                         "gewichtet nach ihren jährlichen Nutzungskosten statt nach ihrem Wert.")
  src <- "Penn World Table 11.0 (rkna, rgdpna, emp)."
  fig_note <- paste0("Enthält Wohnbauten mit geringem Gewicht. ",
                     "Preisindizes für IT und Software nach US-Methode für alle Länder.")
} else {
  ctry <- c(AUT = "Austria", DEU = "Germany", USA = "USA")
  panel_names <- c(services_per_worker = "Capital input per worker",
                   output_per_services = "Output per unit of capital input")
  fig_title <- "More capital per worker, less output per capital"
  fig_subtitle <- paste0("Index ", base_year, " = 100. Capital input: capital services, assets weighted\n",
                         "by their annual cost of use rather than by their value.")
  src <- "Penn World Table 11.0 (rkna, rgdpna, emp)."
  fig_note <- paste0("Includes dwellings with a small weight. ",
                     "IT and software price indices follow US methods for all countries.")
}

temp <- temp %>%
  mutate(name = factor(ctry[iso], levels = unname(ctry)),
         var = factor(panel_names[var], levels = unname(panel_names)))

temp_last <- temp %>%
  filter(year == max_year) %>%
  mutate(label = paste0(name, " ", round(value)))

source("r-corporate-design-functions-ggplot2.R")

ctry_colours <- setNames(unname(design_colours()[c("violet", "teal", "red")]), unname(ctry))

g <- ggplot(temp, aes(x = year, y = value, colour = name)) +
  geom_hline(yintercept = 100, colour = design_colours()[["ink"]], linewidth = .4) +
  geom_vline(xintercept = base_year, colour = design_colours()[["grid"]], linewidth = .5) +
  geom_line(linewidth = 1) +
  ggrepel::geom_text_repel(data = temp_last, aes(label = label),
                           hjust = 0, direction = "y", nudge_x = 1,
                           xlim = c(max_year + .5, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .2, size = 3.2, fontface = "bold",
                           family = font_corporate_design, seed = 1) +
  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = ctry_colours) +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     limits = c(min_year, max_year + 7),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(breaks = scales::breaks_extended(n = 5)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.spacing.y = unit(20, "pt"))

g

save_chart(g, "capital-services-per-worker", lang = lang, format = "portrait")
