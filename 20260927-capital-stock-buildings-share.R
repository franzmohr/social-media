rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

lang <- "de"

min_year <- 1995

# Net capital stock by asset, total economy, current replacement cost --------
# Europe: Eurostat nama_10_nfa_st (the filtered JSON request fails for this
# table, so the whole table is downloaded). USA and Japan: OECD Table 9A, same
# national-accounts concept; it reproduces the Eurostat shares for AT and DE.
assets <- c(N111N = "dw", N112N = "ob", N11N = "total")

eu_codes <- c("EU27_2020", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL",
              "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
              "NL", "PL", "PT", "RO", "SE", "SI", "SK")

nfa_all <- get_eurostat("nama_10_nfa_st", cache = FALSE) %>%
  filter(nace_r2 == "TOTAL", asset10 %in% names(assets), geo %in% eu_codes,
         !is.na(values)) %>%
  transmute(geo, unit, year = as.integer(format(TIME_PERIOD, "%Y")),
            asset = assets[asset10], values)

nfa <- nfa_all %>%
  filter(unit == "CRC_MEUR") %>%
  select(-unit)

oecd_file <- tempfile(fileext = ".csv")
download.file(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/",
                     "A.USA+JPN.S1.S1.LE.N11N+N111N+N112N._T._Z.XDC.V.N.",
                     "?startPeriod=", min_year),
              oecd_file, quiet = TRUE,
              headers = c(Accept = "application/vnd.sdmx.data+csv; charset=utf-8"))

oecd <- read.csv(oecd_file) %>%
  transmute(geo = c(USA = "US", JPN = "JP")[REF_AREA], year = as.integer(TIME_PERIOD),
            asset = assets[INSTR_ASSET], values = OBS_VALUE)

shares <- bind_rows(nfa, oecd) %>%
  filter(year >= min_year) %>%
  pivot_wider(names_from = asset, values_from = values) %>%
  filter(!is.na(total), !is.na(dw), !is.na(ob)) %>%
  # build first: transmute evaluates in order, so dw must still be the level
  transmute(geo, year, build = (dw + ob) / total, dw = dw / total)

max_year <- max(shares$year)

build_floor <- .6

hl_geo <- c("AT", "DE", "EU27_2020", "US", "JP")

if (lang == "de") {
  ctry <- c(AT = "Österreich", DE = "Deutschland", EU27_2020 = "EU-27", US = "USA", JP = "Japan")
  panel_names <- c(dw = "Wohnbauten", build = "Alle Bauten (Wohn- und sonstige Bauten)")
  fig_title <- "Wie viel des Kapitals steckt in Wohnungen?"
  fig_subtitle <- paste0("Anteil am Nettokapitalstock in Prozent, zu Wiederbeschaffungspreisen.\n",
                         "Grau: übrige EU-Länder.")
  src <- "Eurostat (nama_10_nfa_st), USA und Japan: OECD (Tabelle 9A)."
  fig_note <- paste0("Anteile zu laufenden Preisen: sie verschieben sich auch, wenn Baupreise stärker ",
                     "steigen als die Preise von Maschinen.\nBeginn der Reihen je nach Land unterschiedlich.\n",
                     "Alle Bauten: Werte unter 60 % abgeschnitten (u. a. Irland ab 2015: Verlagerung geistigen Eigentums).")
} else {
  ctry <- c(AT = "Austria", DE = "Germany", EU27_2020 = "EU-27", US = "USA", JP = "Japan")
  panel_names <- c(dw = "Dwellings", build = "All buildings and structures")
  fig_title <- "How much of the capital stock is housing?"
  fig_subtitle <- paste0("Share of the net capital stock in percent, at current replacement cost.\n",
                         "Grey: other EU countries.")
  src <- "Eurostat (nama_10_nfa_st), USA and Japan: OECD (Table 9A)."
  fig_note <- paste0("Shares at current prices: they also shift when construction prices rise faster ",
                     "than machinery prices.\nSeries start at different years depending on the country.\n",
                     "All buildings: values below 60% cut off (incl. Ireland from 2015: relocation of intellectual property).")
}

temp <- shares %>%
  pivot_longer(c(dw, build), names_to = "var", values_to = "value") %>%
  mutate(var = factor(panel_names[var], levels = unname(panel_names)),
         hl = geo %in% hl_geo,
         name = factor(ifelse(hl, ctry[geo], "Other"), levels = c(unname(ctry), "Other")),
         # Ireland's 2015 relocation of intellectual property pushes its
         # buildings share down to ~45 %; cut below 60 % so the panel stays readable
         value = ifelse(var == panel_names[["build"]] & value < build_floor, NA_real_, value))

temp_last <- temp %>%
  filter(hl) %>%
  group_by(geo, var) %>%
  filter(year == max(year)) %>%
  ungroup()

dec_mark <- ifelse(lang == "de", ",", ".")

source("r-corporate-design-functions-ggplot2.R")

ctry_colours <- setNames(c(unname(design_colours()[c("violet", "teal")]), unname(design_colours()[["ink"]]),
                           unname(design_colours()[c("red", "amber")])), unname(ctry))

g <- ggplot(temp, aes(x = year, y = value, group = geo)) +
  geom_line(data = filter(temp, !hl), colour = design_colours()[["mute"]], linewidth = .3, alpha = .7) +
  geom_line(data = filter(temp, hl), aes(colour = name), linewidth = .9) +
  ggrepel::geom_text_repel(data = temp_last, aes(label = name, colour = name),
                           hjust = 0, direction = "y", nudge_x = 1,
                           xlim = c(max_year + .5, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .15, size = 3, fontface = "bold",
                           family = font_corporate_design, seed = 1) +
  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = ctry_colours) +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     limits = c(min_year, max_year + 7),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = scales::breaks_extended(n = 5)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.spacing.y = unit(20, "pt"))

g

save_chart(g, "capital-stock-buildings-share", lang = lang, format = "portrait")


# Chart 2: dwellings and all buildings at constant prices ---------------------
# Removes the relative-price effect (construction costs rising faster than
# machinery prices). Chain-linked volumes are not additive across assets, so
# each asset's share is built as: current-price share in the reference year,
# moved with that asset's volume growth relative to the total stock. For
# Eurostat this equals the ratio of CLV20 volumes; it also puts the OECD series
# (US volumes are at 2017 prices) on the same 2020 basis. All buildings =
# dwellings + other buildings. Exact in 2020, approximate away from it.
ref_year <- 2020

oecd_vol_file <- tempfile(fileext = ".csv")
download.file(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/",
                     "A.USA+JPN.S1.S1.LE.N11N+N111N+N112N._T._Z.XDC.L.N.",
                     "?startPeriod=", min_year),
              oecd_vol_file, quiet = TRUE,
              headers = c(Accept = "application/vnd.sdmx.data+csv; charset=utf-8"))

volumes <- bind_rows(
  nfa_all %>%
    filter(unit == "CLV20_MEUR") %>%
    select(-unit),
  read.csv(oecd_vol_file) %>%
    transmute(geo = c(USA = "US", JPN = "JP")[REF_AREA], year = as.integer(TIME_PERIOD),
              asset = assets[INSTR_ASSET], values = OBS_VALUE)) %>%
  filter(year >= min_year) %>%
  pivot_wider(names_from = asset, values_from = values) %>%
  filter(!is.na(dw), !is.na(ob), !is.na(total))

ref_shares <- shares %>%
  filter(year == ref_year) %>%
  transmute(geo, dw_ref = dw, ob_ref = build - dw)

const <- volumes %>%
  inner_join(ref_shares, by = "geo") %>%
  group_by(geo) %>%
  filter(any(year == ref_year)) %>%
  mutate(total_idx = total / total[year == ref_year],
         dw = dw_ref * (dw / dw[year == ref_year]) / total_idx,
         build = dw + ob_ref * (ob / ob[year == ref_year]) / total_idx) %>%
  ungroup() %>%
  select(geo, year, dw, build) %>%
  pivot_longer(c(dw, build), names_to = "var", values_to = "value") %>%
  mutate(var = factor(panel_names[var], levels = unname(panel_names)),
         hl = geo %in% hl_geo,
         name = factor(ifelse(hl, ctry[geo], "Other"), levels = c(unname(ctry), "Other")),
         value = ifelse(var == panel_names[["build"]] & value < build_floor, NA_real_, value),
         # Far from the reference year the approximation can exceed 100 % where
         # relative prices shifted strongly (some Eastern European countries)
         value = ifelse(value > 1, NA_real_, value))

# Austria at current prices, dashed, to show how much of the movement is prices
at_current <- temp %>%
  filter(geo == "AT") %>%
  mutate(geo = "AT_current")

# Label at the start of the dashed line, below it, clear of the other lines
at_current_first <- at_current %>%
  group_by(var) %>%
  filter(year == min(year)) %>%
  ungroup()

const_last <- const %>%
  filter(hl, !is.na(value)) %>%
  group_by(geo, var) %>%
  filter(year == max(year)) %>%
  ungroup()

max_year_const <- max(const$year)

if (lang == "de") {
  fig_title <- "Real sinkt der Gebäudeanteil am Kapital"
  fig_subtitle <- paste0("Anteil am Nettokapitalstock, konstante Preise ", ref_year, ".\n",
                         "Gestrichelt: Österreich zu laufenden Preisen. Grau: übrige EU-Länder.")
  at_current_label <- "Österreich, laufende Preise"
  fig_note <- paste0("Konstante Preise: Anteile ", ref_year, ", fortgeschrieben mit dem Volumenwachstum je Anlageart ",
                     "relativ zum Gesamtbestand.\nVerkettete Volumen sind nicht additiv; Anteile abseits von ",
                     ref_year, " daher Näherung. USA: Volumen erst ab 2015.\n",
                     "Alle Bauten: Werte unter 60 % abgeschnitten (u. a. Irland ab 2015). Näherungswerte über 100 % ausgeblendet.")
} else {
  fig_title <- "In real terms, buildings are a shrinking share of capital"
  fig_subtitle <- paste0("Share of the net capital stock, constant ", ref_year, " prices.\n",
                         "Dashed: Austria at current prices. Grey: other EU countries.")
  at_current_label <- "Austria, current prices"
  fig_note <- paste0("Constant prices: ", ref_year, " shares, moved with each asset's volume growth ",
                     "relative to the total stock.\nChain-linked volumes are not additive, so shares away from ",
                     ref_year, " are approximate. USA: volumes only from 2015.\n",
                     "All buildings: values below 60% cut off (incl. Ireland from 2015). Approximations above 100% hidden.")
}

g_const <- ggplot(const, aes(x = year, y = value, group = geo)) +
  geom_line(data = filter(const, !hl), colour = design_colours()[["mute"]], linewidth = .3, alpha = .7) +
  geom_line(data = at_current, colour = design_colours()[["violet"]], linewidth = .7, linetype = "22") +
  geom_line(data = filter(const, hl), aes(colour = name), linewidth = .9) +
  ggrepel::geom_text_repel(data = const_last, aes(label = name, colour = name),
                           hjust = 0, direction = "y", nudge_x = 1,
                           xlim = c(max_year_const + .5, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .15, size = 3, fontface = "bold",
                           family = font_corporate_design, seed = 1) +
  geom_text(data = at_current_first, aes(label = at_current_label),
            hjust = 0, vjust = 1.8, size = 2.8, family = font_corporate_design,
            colour = design_colours()[["violet"]]) +
  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = ctry_colours) +
  scale_x_continuous(breaks = seq(1995, 2020, 5),
                     limits = c(min_year, max_year_const + 7),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = scales::breaks_extended(n = 5)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year_const, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.spacing.y = unit(20, "pt"))

g_const

save_chart(g_const, "capital-stock-buildings-share-constant-prices", lang = lang, format = "portrait")
