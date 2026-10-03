rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

lang <- "de"

# Penn World Table. Dataverse blocks scripted downloads, so the file has to be
# fetched by hand from https://dataverse.nl/api/access/datafile/554105 and put
# into one of pwt_dirs (pwt*.xlsx or pwt*.dta). Without it the script falls
# back to the CRAN package pwt10, which ends in 2019.
pwt_dirs <- c(".", "D:/models/sf-penn-world-tables")
pwt_files <- list.files(pwt_dirs, pattern = "^pwt.*\\.(xlsx|dta)$",
                        ignore.case = TRUE, full.names = TRUE)
pwt_file <- pwt_files[order(basename(pwt_files), decreasing = TRUE)][1]

# Levels are indexed to the first year in which every country has data: the
# Baltics, Croatia, Czechia, Slovakia and Slovenia only start in 1990
base_year <- 1990

# Annual changes beyond this (in %) are left out of the growth panel and named
# in the caption instead -- Cyprus 1974 (-18 %) would otherwise flatten it
growth_cap <- 5

# Highlighted countries. Everything else is drawn as grey context.
hl <- c("AUT", "DEU", "ITA", "IRL", "ESP", "BGR")

# EU-27 plus Iceland, Norway and Switzerland ----------------------------------
ctry_codes <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
                "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
                "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE",
                "ISL", "NOR", "CHE")

if (lang == "de") {
  ctry <- c(AUT = "Österreich", DEU = "Deutschland", ITA = "Italien",
            IRL = "Irland", ESP = "Spanien", BGR = "Bulgarien")
  panels <- c(level = paste0("Bevölkerung (", base_year, " = 100)"),
              growth = "Veränderung gegenüber dem Vorjahr (in %)")
  fig_title <- "Wer wächst, wer schrumpft?"
  fig_subtitle <- paste0("Bevölkerung europäischer Länder, %s–%s.\n",
                         "Jede graue Linie ist ein weiteres europäisches Land.")
  src <- "Penn World Table %s (Feenstra, Inklaar und Timmer)."
  note_cap <- "Nicht dargestellt: "
} else {
  ctry <- c(AUT = "Austria", DEU = "Germany", ITA = "Italy",
            IRL = "Ireland", ESP = "Spain", BGR = "Bulgaria")
  panels <- c(level = paste0("Population (", base_year, " = 100)"),
              growth = "Change on previous year (in %)")
  fig_title <- "Who grows, who shrinks?"
  fig_subtitle <- paste0("Population of European countries, %s–%s.\n",
                         "Each grey line is another European country.")
  src <- "Penn World Table %s (Feenstra, Inklaar and Timmer)."
  note_cap <- "Not shown: "
}

dec_mark <- ifelse(lang == "de", ",", ".")

# Population in millions ------------------------------------------------------
if (!is.na(pwt_file)) {
  raw <- if (grepl("\\.xlsx$", pwt_file, ignore.case = TRUE)) {
    readxl::read_excel(pwt_file, sheet = "Data")
  } else {
    haven::read_dta(pwt_file)
  }
  # "pwt110.xlsx" -> "11.0"
  pwt_version <- sub("^(\\d+)(\\d)$", "\\1.\\2",
                     gsub("\\D", "", tools::file_path_sans_ext(basename(pwt_file))))
} else {
  raw <- pwt10::pwt10.01
  pwt_version <- "10.01"
}

temp <- raw %>%
  # the package calls the ISO code "isocode", the published files "countrycode"
  rename(any_of(c(geo = "countrycode", geo = "isocode"))) %>%
  mutate(geo = as.character(geo), year = as.integer(year)) %>%
  filter(geo %in% ctry_codes, !is.na(pop)) %>%
  arrange(geo, year) %>%
  group_by(geo) %>%
  mutate(growth = if_else(year - lag(year) == 1,
                          (pop / lag(pop) - 1) * 100, NA_real_),
         level = pop / pop[year == base_year] * 100) %>%
  ungroup()

min_year <- min(temp$year)
max_year <- max(temp$year)

outliers <- filter(temp, abs(growth) > growth_cap)
fig_note <- if (nrow(outliers) > 0) {
  outlier_names <- countrycode::countrycode(outliers$geo, "iso3c",
                                            paste0("country.name.", lang))
  paste0(note_cap, paste0(outlier_names, " ", outliers$year, " (",
                          formatC(outliers$growth, format = "f", digits = 1,
                                  decimal.mark = dec_mark), " %)",
                          collapse = ", "), ".")
}

temp <- mutate(temp, growth = if_else(abs(growth) > growth_cap,
                                      NA_real_, growth))

temp <- temp %>%
  select(geo, year, level, growth) %>%
  pivot_longer(c(level, growth), names_to = "panel", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(panel = factor(panel, levels = names(panels), labels = unname(panels)))

# Split into context and story so each gets its own layer -------------------
back <- filter(temp, !geo %in% hl)
front <- temp %>%
  filter(geo %in% hl) %>%
  mutate(name = factor(ctry[geo], levels = unname(ctry[hl])))

# Direct labels at the end of the level lines replace the legend
ends <- front %>%
  filter(panel == panels[["level"]]) %>%
  group_by(geo) %>%
  filter(year == max(year)) %>%
  ungroup()

# Reference lines: the base year in the level panel, no growth in the other
ref <- data.frame(panel = factor(unname(panels), levels = unname(panels)),
                  y = c(100, 0))

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(mapping = aes(x = year, y = value)) +
  geom_hline(data = ref, aes(yintercept = y),
             colour = design_colours()[["ink"]], linewidth = .5) +
  # context: every other country, readable as a cloud but never as a series
  geom_line(data = back, aes(group = geo),
            colour = design_colours()[["mute"]], linewidth = .45) +
  # story: the series that carry the point
  geom_line(data = front, aes(colour = name), linewidth = 1) +
  geom_point(data = ends, aes(colour = name), size = 1.8) +
  ggrepel::geom_text_repel(data = ends, aes(colour = name, label = name),
                           hjust = 0, nudge_x = 2, direction = "y",
                           size = 3.9, fontface = "bold", family = font_corporate_design,
                           segment.colour = NA, min.segment.length = Inf,
                           box.padding = .1, seed = 1) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = seq(1950, max_year, 10),
                     expand = expansion(mult = c(.02, .2))) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1,
                                                   decimal.mark = dec_mark)) +
  scale_colour_manual(values = palette_corporate_design("cat", length(hl)), guide = "none") +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = sprintf(fig_subtitle, min_year, max_year),
       caption = caption_corporate_design(sprintf(src, pwt_version), last = max_year,
                               note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 13, grid = "y") +
  theme(axis.title = element_blank(),
        panel.spacing.y = unit(20, "pt"))

g

save_chart(g, "population-levels-and-growth", lang = lang, format = "portrait")
