rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

lang <- "de"

# Net capital stock by asset, total economy, current replacement cost --------
# Eurostat nama_10_nfa_st. The filtered JSON request fails for this table, so
# the whole table is downloaded and filtered here.
assets <- c(N111N = "dw", N112N = "ob", N11MN = "me", N117N = "ip", N11N = "total")

nfa <- get_eurostat("nama_10_nfa_st", cache = FALSE) %>%
  filter(unit == "CRC_MEUR", nace_r2 == "TOTAL", asset10 %in% names(assets),
         !is.na(values)) %>%
  mutate(year = as.integer(format(TIME_PERIOD, "%Y")),
         asset = assets[asset10]) %>%
  select(geo, year, asset, values) %>%
  pivot_wider(names_from = asset, values_from = values) %>%
  filter(!is.na(total), !is.na(dw), !is.na(ob), !is.na(me), !is.na(ip))

# EU-27 members plus the EU aggregate; euro area aggregates left out
ctry_codes <- c("EU27_2020", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL",
                "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
                "NL", "PL", "PT", "RO", "SE", "SI", "SK")

nfa <- filter(nfa, geo %in% ctry_codes)

# Latest year that every country reports, so all bars refer to the same year
ref_year <- nfa %>%
  group_by(geo) %>%
  summarise(last = max(year), .groups = "drop") %>%
  pull(last) %>%
  min()

# USA and Japan: OECD Table 9A, same national-accounts concept (net stock at
# current prices, total economy). For Austria and Germany it reproduces the
# Eurostat shares exactly.
oecd_file <- tempfile(fileext = ".csv")
download.file(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE9A,/",
                     "A.USA+JPN.S1.S1.LE.N11N+N111N+N112N+N11MN+N117N._T._Z.XDC.V.N.",
                     "?startPeriod=", ref_year, "&endPeriod=", ref_year),
              oecd_file, quiet = TRUE,
              headers = c(Accept = "application/vnd.sdmx.data+csv; charset=utf-8"))

oecd <- read.csv(oecd_file) %>%
  transmute(geo = c(USA = "US", JPN = "JP")[REF_AREA], year = as.integer(TIME_PERIOD),
            asset = assets[INSTR_ASSET], values = OBS_VALUE) %>%
  pivot_wider(names_from = asset, values_from = values)

shares <- bind_rows(nfa, oecd) %>%
  filter(year == ref_year) %>%
  # Cultivated biological resources and the rest (<1 % almost everywhere) are
  # folded into machinery and equipment so the bars add up to 100 %
  mutate(me = total - dw - ob - ip,
         across(c(dw, ob, me, ip), ~ .x / total),
         build = dw + ob) %>%
  select(geo, dw, ob, me, ip, build)

if (lang == "de") {
  ctry <- c(EU27_2020 = "EU-27", AT = "Österreich", BE = "Belgien", BG = "Bulgarien",
            CY = "Zypern", CZ = "Tschechien", DE = "Deutschland", DK = "Dänemark",
            EE = "Estland", EL = "Griechenland", ES = "Spanien", FI = "Finnland",
            FR = "Frankreich", HR = "Kroatien", HU = "Ungarn", IE = "Irland",
            IT = "Italien", LT = "Litauen", LU = "Luxemburg", LV = "Lettland",
            MT = "Malta", NL = "Niederlande", PL = "Polen", PT = "Portugal",
            RO = "Rumänien", SE = "Schweden", SI = "Slowenien", SK = "Slowakei",
            US = "USA", JP = "Japan")
  asset_names <- c(dw = "Wohnbauten", ob = "Sonstige Bauten und Infrastruktur",
                   me = "Maschinen, Ausrüstung, Fahrzeuge", ip = "Geistiges Eigentum (F&E, Software)")
  fig_title <- "Vier von fünf Euro Kapital stecken in Gebäuden"
  fig_subtitle <- paste0("Nettokapitalstock nach Anlageart, Anteile in Prozent, ", ref_year, ".\n",
                         "Zahl rechts: Anteil aller Bauten (Wohn- und sonstige Bauten).")
  src <- "Eurostat (nama_10_nfa_st), USA und Japan: OECD (Tabelle 9A). Wiederbeschaffungspreise."
  fig_note <- "Irland: hoher Anteil geistigen Eigentums durch multinationale Konzerne."
} else {
  ctry <- c(EU27_2020 = "EU-27", AT = "Austria", BE = "Belgium", BG = "Bulgaria",
            CY = "Cyprus", CZ = "Czechia", DE = "Germany", DK = "Denmark",
            EE = "Estonia", EL = "Greece", ES = "Spain", FI = "Finland",
            FR = "France", HR = "Croatia", HU = "Hungary", IE = "Ireland",
            IT = "Italy", LT = "Lithuania", LU = "Luxembourg", LV = "Latvia",
            MT = "Malta", NL = "Netherlands", PL = "Poland", PT = "Portugal",
            RO = "Romania", SE = "Sweden", SI = "Slovenia", SK = "Slovakia",
            US = "United States", JP = "Japan")
  asset_names <- c(dw = "Dwellings", ob = "Other buildings and infrastructure",
                   me = "Machinery, equipment, vehicles", ip = "Intellectual property (R&D, software)")
  fig_title <- "Four in five euros of capital are buildings"
  fig_subtitle <- paste0("Net capital stock by asset, shares in percent, ", ref_year, ".\n",
                         "Number on the right: share of all buildings and structures.")
  src <- "Eurostat (nama_10_nfa_st), USA and Japan: OECD (Table 9A). Current replacement cost."
  fig_note <- "Ireland: high share of intellectual property due to multinational firms."
}

# Countries ordered by their share of dwellings
shares <- shares %>%
  filter(geo %in% names(ctry)) %>%
  arrange(dw) %>%
  mutate(name = factor(ctry[geo], levels = ctry[geo]))

temp <- shares %>%
  pivot_longer(c(dw, ob, me, ip), names_to = "asset", values_to = "value") %>%
  mutate(asset = factor(asset_names[asset], levels = unname(asset_names)))

dec_mark <- ifelse(lang == "de", ",", ".")

source("r-corporate-design-functions-ggplot2.R")

asset_colours <- setNames(unname(design_colours()[c("blue", "teal", "amber", "red")]),
                          unname(asset_names))

# Austria and the EU average are outlined so they can be found at once
hl <- filter(shares, geo %in% c("AT", "EU27_2020"))

g <- ggplot(temp, aes(x = value, y = name)) +
  geom_col(aes(fill = asset), width = .75,
           position = position_stack(reverse = TRUE)) +
  geom_tile(data = hl, aes(x = .5, width = 1, height = .75), fill = NA,
            colour = design_colours()[["ink"]], linewidth = .9) +
  geom_text(data = shares,
            aes(x = 1.02, label = formatC(build * 100, format = "f", digits = 0),
                fontface = ifelse(geo %in% hl$geo, "bold", "plain")),
            hjust = 0, size = 3, family = font_corporate_design,
            colour = design_colours()[["ink"]]) +
  scale_fill_manual(values = asset_colours) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = seq(0, 1, .25),
                     expand = expansion(mult = c(0, .06))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = ref_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "x") +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 8.5),
        legend.text = element_text(size = 8.5))

g

save_chart(g, "capital-stock-composition", lang = lang, format = "portrait")
