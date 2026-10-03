rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

lang <- "de"

min_year <- 1970

# Downloads -------------------------------------------------------------------
# download.file() first, the system curl as fallback: dataverse.nl sits behind
# a bot filter that rejects curl's user agent but lets R through, while R cannot
# always resolve imf.org where curl can. Five attempts with growing waits,
# because dataverse.nl and the World Bank API fail intermittently.
fetch <- function(url, ext = "") {
  dest <- tempfile(fileext = ext)
  for (attempt in 1:5) {
    ok <- tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok) ok <- system2("curl", c("-sfL", "-o", shQuote(dest), shQuote(url))) == 0
    if (ok) return(dest)
    Sys.sleep(2^attempt)
  }
  stop("download failed: ", url)
}

# Penn World Table 11.0 -------------------------------------------------------
# rgdpna: real GDP, rconna: real consumption of households and government,
# rnna: capital stock, all at constant 2021 national prices -- the PWT series
# meant for growth rates. delta: average depreciation rate of the capital stock.
# PWT 11.0 is a fixed release: downloaded once into the local data folder and
# reused, which also spares the rate-limited dataverse.nl server.
pwt_cache <- "D:/projects/MacroData/pwt/pwt110.xlsx"
if (!file.exists(pwt_cache)) {
  dir.create(dirname(pwt_cache), recursive = TRUE, showWarnings = FALSE)
  file.copy(fetch("https://dataverse.nl/api/access/datafile/554105", ".xlsx"), pwt_cache)
}

pwt <- read_excel(pwt_cache, sheet = "Data") %>%
  select(iso = countrycode, country, year, pop, emp, delta,
         gdp = rgdpna, con = rconna, cap = rnna) %>%
  filter(!is.na(gdp), !is.na(con), !is.na(cap), !is.na(pop), year >= min_year)

pwt_last <- max(pwt$year)

# Countries with the full window, so the world aggregate has a fixed sample
pwt <- pwt %>%
  group_by(iso) %>%
  filter(min(year) == min_year, max(year) == pwt_last) %>%
  ungroup()

# Extension to 2025 with IMF WEO and World Bank ------------------------------
# PWT ends in 2023. Later years are carried forward from the PWT levels with
# growth rates:
#   GDP          IMF WEO real GDP growth, World Bank if missing
#   population   World Bank population, IMF WEO if missing
#   consumption  World Bank final consumption growth, GDP growth if missing
#   capital      perpetual inventory with PWT's last depreciation rate; real
#                investment grows with World Bank gross fixed capital formation,
#                with GDP if missing
# Checked against PWT for 2000-2023: GDP and population growth match to within
# 0.1 pp in the median; a capital stock rebuilt this way from 2013 lands within
# 2 % of PWT's 2023 value for about 70 % of countries.
ext_last <- 2025

wb_growth <- function(indicator, level = FALSE) {
  j <- jsonlite::fromJSON(fetch(paste0("https://api.worldbank.org/v2/country/all/indicator/",
                                       indicator, "?date=", pwt_last - 1, ":", ext_last,
                                       "&format=json&per_page=20000"), ".json"))[[2]]
  d <- tibble(iso = j$countryiso3code, year = as.integer(j$date), value = j$value)
  if (level) {
    d <- d %>% group_by(iso) %>% arrange(year) %>%
      mutate(value = value / lag(value) * 100 - 100) %>% ungroup()
  }
  d %>% filter(year > pwt_last) %>% transmute(iso, year, g = value / 100)
}

imf_growth <- function(indicator, level = FALSE) {
  v <- jsonlite::fromJSON(fetch(paste0("https://www.imf.org/external/datamapper/api/v1/",
                                       indicator), ".json"))$values[[indicator]]
  d <- bind_rows(lapply(names(v), function(i)
    tibble(iso = i, year = as.integer(names(v[[i]])), value = unlist(v[[i]]))))
  if (level) {
    d <- d %>% group_by(iso) %>% arrange(year) %>%
      mutate(value = value / lag(value) * 100 - 100) %>% ungroup()
  }
  d %>% filter(year > pwt_last, year <= ext_last) %>% transmute(iso, year, g = value / 100)
}

ext_growth <- expand_grid(iso = unique(pwt$iso), year = (pwt_last + 1):ext_last) %>%
  left_join(rename(imf_growth("NGDP_RPCH"), g_gdp_imf = g), by = c("iso", "year")) %>%
  left_join(rename(wb_growth("NY.GDP.MKTP.KD.ZG"), g_gdp_wb = g), by = c("iso", "year")) %>%
  left_join(rename(wb_growth("SP.POP.TOTL", level = TRUE), g_pop_wb = g), by = c("iso", "year")) %>%
  left_join(rename(imf_growth("LP", level = TRUE), g_pop_imf = g), by = c("iso", "year")) %>%
  left_join(rename(wb_growth("NE.CON.TOTL.KD.ZG"), g_con = g), by = c("iso", "year")) %>%
  left_join(rename(wb_growth("NE.GDI.FTOT.KD.ZG"), g_inv = g), by = c("iso", "year")) %>%
  mutate(g_gdp = coalesce(g_gdp_imf, g_gdp_wb),
         g_pop = coalesce(g_pop_wb, g_pop_imf),
         con_fill = is.na(g_con),
         inv_fill = is.na(g_inv),
         g_con = coalesce(g_con, g_gdp),
         g_inv = coalesce(g_inv, g_gdp))

# Without GDP or population growth a country cannot be carried forward: it
# leaves the sample so the world aggregate stays consistent
ext_drop <- ext_growth %>%
  filter(is.na(g_gdp) | is.na(g_pop)) %>%
  distinct(iso) %>%
  pull(iso)

pwt <- filter(pwt, !iso %in% ext_drop)
ext_growth <- filter(ext_growth, !iso %in% ext_drop)

pwt_ext <- pwt %>%
  group_by(iso) %>%
  arrange(year) %>%
  # Real investment implied by PWT's own capital accumulation in the last year;
  # computed first, before cap is collapsed to its last value
  summarise(inv = last(cap) - (1 - last(delta)) * nth(cap, -2),
            country = last(country), delta = last(delta),
            gdp = last(gdp), con = last(con), cap = last(cap), pop = last(pop),
            .groups = "drop") %>%
  # Guard against a non-positive implied investment: replacement only
  mutate(inv = ifelse(inv > 0, inv, delta * cap))

ext_rows <- list()
for (t in (pwt_last + 1):ext_last) {
  pwt_ext <- pwt_ext %>%
    select(-any_of("year")) %>%
    inner_join(filter(ext_growth, year == t), by = "iso") %>%
    mutate(gdp = gdp * (1 + g_gdp),
           pop = pop * (1 + g_pop),
           con = con * (1 + g_con),
           inv = inv * (1 + g_inv),
           cap = (1 - delta) * cap + inv) %>%
    select(iso, country, delta, gdp, con, cap, pop, inv, year)
  ext_rows[[length(ext_rows) + 1]] <- pwt_ext
}

pwt <- bind_rows(pwt, bind_rows(ext_rows) %>% select(-inv)) %>%
  arrange(iso, year)

max_year <- ext_last

n_ctry <- n_distinct(pwt$iso)
n_fill_con <- n_distinct(ext_growth$iso[ext_growth$con_fill])
n_fill_inv <- n_distinct(ext_growth$iso[ext_growth$inv_fill])
message("extension: ", length(ext_drop), " countries dropped (", paste(ext_drop, collapse = ", "),
        "); consumption filled with GDP growth for ", n_fill_con,
        ", investment for ", n_fill_inv)

world <- pwt %>%
  group_by(year) %>%
  summarise(across(c(pop, gdp, con, cap), sum), .groups = "drop") %>%
  mutate(iso = "WLD")

temp <- bind_rows(world, pwt) %>%
  mutate(across(c(gdp, con, cap), ~ .x / pop)) %>%
  arrange(iso, year)

# Average annual growth per capita over the whole window
cagr <- function(x) (last(x) / first(x))^(1 / (max_year - min_year)) - 1

avg <- temp %>%
  group_by(iso) %>%
  summarise(across(c(gdp, con, cap), cagr), .groups = "drop") %>%
  mutate(gap_con = con - gdp,
         gap_cap = cap - gdp)

if (lang == "de") {
  ctry <- c(WLD = paste0("Welt (", n_ctry, " Länder)"), AUT = "Österreich",
            DEU = "Deutschland", FRA = "Frankreich", GBR = "Großbritannien",
            USA = "USA", JPN = "Japan", KOR = "Südkorea", CHN = "China",
            IND = "Indien", IRL = "Irland", SAU = "Saudi-Arabien")
  # Outliers picked by the data rather than by hand; anything not listed
  # falls back to the English PWT name
  extra <- c(KWT = "Kuwait", ARE = "VAE", SYR = "Syrien", QAT = "Katar",
             VGB = "Brit. Jungferninseln", BGR = "Bulgarien", MMR = "Myanmar",
             BHS = "Bahamas", ZMB = "Sambia", ZWE = "Simbabwe", GHA = "Ghana")
  var_names <- c(gdp = "BIP", con = "Konsum", cap = "Kapitalstock", pop = "Bevölkerung")
} else {
  ctry <- c(WLD = paste0("World (", n_ctry, " countries)"), AUT = "Austria",
            DEU = "Germany", FRA = "France", GBR = "United Kingdom",
            USA = "United States", JPN = "Japan", KOR = "South Korea", CHN = "China",
            IND = "India", IRL = "Ireland", SAU = "Saudi Arabia")
  extra <- c(ARE = "UAE", SYR = "Syria", VGB = "British Virgin Islands")
  var_names <- c(gdp = "GDP", con = "Consumption", cap = "Capital stock", pop = "Population")
}
src <- if (lang == "de") {
  paste0("Penn World Table 11.0; ", pwt_last + 1, "–", max_year, " fortgeschrieben mit IMF WEO und Weltbank.")
} else {
  paste0("Penn World Table 11.0; ", pwt_last + 1, "–", max_year, " extended with IMF WEO and World Bank.")
}

pwt_names <- distinct(pwt, iso, country)
all_names <- c(ctry, extra)
all_names <- c(all_names,
               setNames(pwt_names$country, pwt_names$iso)[setdiff(pwt_names$iso, names(all_names))])

dec_mark <- ifelse(lang == "de", ",", ".")

signed <- function(x) {
  paste0(ifelse(x >= 0, "+", "−"),
         formatC(abs(x) * 100, format = "f", digits = 1, decimal.mark = dec_mark))
}

source("r-corporate-design-functions-ggplot2.R")

# Chart 1: 10-year growth by indicator, countries as lines -------------------
# One panel per indicator. Every country is drawn in grey for context; the
# highlighted ones get a colour and their ISO code at the end of the line.
hl_iso <- c("WLD", "USA", "CHN", "IND", "DEU", "AUT")

y_cap <- c(-.08, .15)

# Labour productivity: real GDP per person employed, PWT years only. The world
# line uses the countries with employment data for every year, so gaps in the
# data do not move it.
prod_ctry <- pwt %>%
  filter(year <= pwt_last) %>%
  group_by(iso) %>%
  filter(all(!is.na(emp))) %>%
  ungroup()
n_prod <- n_distinct(prod_ctry$iso)

prod <- bind_rows(
  prod_ctry %>%
    group_by(year) %>%
    summarise(prod = sum(gdp) / sum(emp), .groups = "drop") %>%
    mutate(iso = "WLD"),
  pwt %>%
    filter(year <= pwt_last) %>%
    transmute(iso, year, prod = gdp / emp))

if (lang == "de") {
  fig_title <- "Produktion, Konsum, Kapital und Produktivität"
  fig_subtitle <- paste0("Wachstum gegenüber dem Vorjahr, gleitender 5-Jahres-Durchschnitt.\n",
                         "Grau: alle ", n_ctry, " Länder. Gleiche Skala für alle Indikatoren.")
  fig_note <- paste0("WLD: Welt (Summe der ", n_ctry, " Länder). Konsum: Haushalte und Staat. Grau hinterlegt: Fortschreibung. ",
                     "Achse begrenzt auf −8 bis +15 %.\n",
                     "BIP je Erwerbstätigen bis ", pwt_last, " (keine Beschäftigungsdaten danach); ",
                     "WLD dort: ", n_prod, " Länder mit vollständigen Beschäftigungsdaten.")
  panel_names <- c(gdp = "Reales BIP pro Kopf", con = "Realer Konsum pro Kopf",
                   cap = "Realer Kapitalstock pro Kopf", prod = "Reales BIP je Erwerbstätigen")
} else {
  fig_title <- "Production, consumption, capital and productivity"
  fig_subtitle <- paste0("Year-on-year growth, 5-year moving average.\n",
                         "Grey: all ", n_ctry, " countries. Same scale for all indicators.")
  fig_note <- paste0("WLD: world (sum of the ", n_ctry, " countries). Consumption: households and government. Shaded: extension. ",
                     "Axis capped at −8 to +15%.\n",
                     "GDP per worker until ", pwt_last, " (no employment data after); ",
                     "WLD there: ", n_prod, " countries with complete employment data.")
  panel_names <- c(gdp = "Real GDP per capita", con = "Real consumption per capita",
                   cap = "Real capital stock per capita", prod = "Real GDP per worker")
}

temp_g10 <- temp %>%
  left_join(prod, by = c("iso", "year")) %>%
  group_by(iso) %>%
  arrange(year) %>%
  # Year-on-year growth, averaged over the year and the four before it
  # (trailing, so the extended years 2024-2025 stay in the chart)
  mutate(across(c(gdp, con, cap, prod), ~ .x / lag(.x) - 1),
         across(c(gdp, con, cap, prod), ~ as.numeric(stats::filter(.x, rep(1 / 5, 5), sides = 1)))) %>%
  ungroup() %>%
  filter(year >= min_year + 5) %>%
  pivot_longer(c(gdp, con, cap, prod), names_to = "var", values_to = "value") %>%
  mutate(var = factor(panel_names[var], levels = unname(panel_names)),
         hl = iso %in% hl_iso,
         iso_hl = factor(ifelse(hl, iso, "Other"), levels = c(hl_iso, "Other")))

hl_colours <- c(WLD = unname(design_colours()[["ink"]]),
                setNames(palette_corporate_design("cat", length(hl_iso) - 1), hl_iso[-1]))

# Labels sit at each line's last value (productivity ends earlier), clamped
# into the visible range
g10_last <- temp_g10 %>%
  filter(hl, !is.na(value)) %>%
  group_by(iso, var) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(value = pmin(pmax(value, y_cap[1]), y_cap[2]))

g_time <- ggplot(temp_g10, aes(x = year, y = value, group = iso)) +
  # Extended years (IMF WEO / World Bank) shaded
  annotate("rect", xmin = pwt_last + .5, xmax = max_year + .5, ymin = -Inf, ymax = Inf,
           fill = design_colours()[["grid"]], alpha = .6) +
  geom_line(data = filter(temp_g10, !hl), colour = design_colours()[["mute"]],
            linewidth = .2, alpha = .35) +
  geom_zeroline() +
  geom_line(data = filter(temp_g10, hl), aes(colour = iso_hl),
            linewidth = .8) +
  ggrepel::geom_text_repel(data = g10_last, aes(label = iso, colour = iso_hl),
                           hjust = 0, direction = "y", nudge_x = 1.5,
                           xlim = c(max_year + 1, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .2, force = 2, size = 2.8, fontface = "bold",
                           family = font_corporate_design, seed = 1) +
  facet_wrap(~ var, ncol = 2) +
  scale_colour_manual(values = hl_colours) +
  scale_x_continuous(breaks = c(1980, 2000, 2020),
                     limits = c(min_year + 5, max_year + 9),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = .1, decimal.mark = dec_mark, drop0trailing = TRUE),
                     breaks = seq(-.06, .15, .03)) +
  coord_cartesian(ylim = y_cap) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.spacing.x = unit(18, "pt"))

g_time

save_chart(g_time, "gdp-consumption-capital-growth", lang = lang, format = "portrait")

# Chart 1b: levels per capita, countries as lines ------------------------------
# Same design as chart 1 but in levels: thousand 2021 US$ per head, linear
# scale. Each panel is capped a little above the highlighted countries; the few
# small, very rich economies above that are cut off rather than squashing the rest.
# The capital stock is shown as an index instead: its dollar level hinges on the
# capital-goods PPP, which makes cross-country level comparisons unreliable.
# rnna is the national constant-price series times one fixed 2021 conversion
# factor (rnna equals cn in 2021), so the index is the domestic-currency index.
if (lang == "de") {
  fig_title <- "Produktion, Konsum und Kapital pro Kopf"
  fig_subtitle <- paste0("BIP und Konsum: tausend US-Dollar pro Kopf, Preise und Kaufkraftparitäten von 2021.\n",
                         "Kapitalstock: Index ", min_year, " = 100, konstante nationale Preise. ",
                         "Grau: alle ", n_ctry, " Länder.")
  fig_note <- paste0("WLD: Welt (Summe der ", n_ctry, " Länder). Konsum: Haushalte und Staat. Grau hinterlegt: Fortschreibung.\n",
                     "Länder oberhalb der Skala abgeschnitten (sehr reiche Kleinstaaten, Ölexporteure; beim Kapital schnell aufholende Länder).\n",
                     "Kapitalindex auf Basis nationaler Kapitalstöcke zu konstanten Preisen; nationale Methoden für Investitionsdeflatoren\n",
                     "und die Abgrenzung der Anlagegüter unterscheiden sich.")
  level_names <- c(gdp = "Reales BIP pro Kopf", con = "Realer Konsum pro Kopf",
                   cap = paste0("Realer Kapitalstock pro Kopf, Index ", min_year, " = 100"))
} else {
  fig_title <- "Production, consumption and capital per capita"
  fig_subtitle <- paste0("GDP and consumption: thousand US dollars per head, 2021 prices and PPPs.\n",
                         "Capital stock: index ", min_year, " = 100, constant national prices. ",
                         "Grey: all ", n_ctry, " countries.")
  fig_note <- paste0("WLD: world (sum of the ", n_ctry, " countries). Consumption: households and government. Shaded: extension.\n",
                     "Countries above the scale are cut off (very rich small states, oil exporters; for capital fast catch-up economies).\n",
                     "Index based on national constant-price capital stocks; national methods for investment deflators\n",
                     "and asset coverage differ.")
  level_names <- c(gdp = "Real GDP per capita", con = "Real consumption per capita",
                   cap = paste0("Real capital stock per capita, index ", min_year, " = 100"))
}

# PWT money values are in millions of 2021 US$ and population in millions, so
# per-capita values are in US$
temp_lvl <- temp %>%
  pivot_longer(c(gdp, con, cap), names_to = "var", values_to = "value") %>%
  group_by(iso, var) %>%
  arrange(year) %>%
  mutate(value = ifelse(var == "cap", value / first(value) * 100, value / 1e3)) %>%
  ungroup() %>%
  mutate(var = factor(level_names[var], levels = unname(level_names)),
         hl = iso %in% hl_iso,
         iso_hl = factor(ifelse(hl, iso, "Other"), levels = c(hl_iso, "Other")))

# Panel ceiling: 20 % above the highest highlighted value, rounded up. The
# capital index is capped at 1,000 so China's ~80-fold rise does not flatten
# every other country; its line leaves the panel at the top.
lvl_cap <- temp_lvl %>%
  filter(hl) %>%
  group_by(var) %>%
  summarise(value = max(pretty(c(0, max(value) * 1.2))), .groups = "drop") %>%
  mutate(value = ifelse(var == level_names[["cap"]], 1000, value))

# End labels; a line that has left the panel is labelled at the top with an arrow
lvl_last <- temp_lvl %>%
  filter(hl, year == max(year)) %>%
  left_join(rename(lvl_cap, cap_value = value), by = "var") %>%
  mutate(label = ifelse(value > cap_value, paste(iso, "↑"), iso),
         value = pmin(value, cap_value))

# Values above the ceiling are dropped; the first point above it is set to the
# ceiling so a highlighted line runs up to the panel edge instead of stopping short
temp_lvl <- temp_lvl %>%
  left_join(rename(lvl_cap, cap_value = value), by = "var") %>%
  group_by(iso, var) %>%
  arrange(year) %>%
  mutate(over = value > cap_value,
         value = case_when(over & !lag(over, default = FALSE) ~ cap_value,
                           over ~ NA_real_,
                           TRUE ~ value)) %>%
  ungroup()

g_level <- ggplot(temp_lvl, aes(x = year, y = value, group = iso)) +
  # Extended years (IMF WEO / World Bank) shaded
  annotate("rect", xmin = pwt_last + .5, xmax = max_year + .5, ymin = -Inf, ymax = Inf,
           fill = design_colours()[["grid"]], alpha = .6) +
  geom_line(data = filter(temp_lvl, !hl), colour = design_colours()[["mute"]],
            linewidth = .2, alpha = .35) +
  geom_line(data = filter(temp_lvl, hl), aes(colour = iso_hl), linewidth = .8) +
  geom_blank(data = mutate(lvl_cap, iso = NA, year = min_year)) +
  ggrepel::geom_text_repel(data = lvl_last, aes(label = label, colour = iso_hl),
                           hjust = 0, direction = "y", nudge_x = 1.5,
                           xlim = c(max_year + 1, NA),
                           segment.colour = design_colours()[["grid"]],
                           segment.size = .3, min.segment.length = 0,
                           box.padding = .2, force = 2, size = 2.8, fontface = "bold",
                           family = font_corporate_design, seed = 1) +
  facet_wrap(~ var, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = hl_colours) +
  scale_x_continuous(breaks = seq(1970, 2020, 10),
                     limits = c(min_year, max_year + 5),
                     expand = expansion(mult = c(.01, 0))) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1, decimal.mark = dec_mark,
                                                   big.mark = ifelse(lang == "de", ".", ",")),
                     limits = c(0, NA), breaks = scales::breaks_extended(n = 5),
                     expand = expansion(mult = c(0, .02))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        panel.spacing.y = unit(20, "pt"))

g_level

save_chart(g_level, "gdp-consumption-capital-levels", lang = lang, format = "portrait")

# Charts 2 and 3: average growth across all countries -------------------------
scatter_growth <- function(var, fig_title, fig_subtitle, y_title, fig_note = NULL) {

  d <- avg %>%
    filter(iso != "WLD") %>%
    mutate(y = .data[[var]], gap = y - gdp)

  # Label the selected countries plus the largest gaps on either side
  lab_iso <- c(setdiff(names(ctry), "WLD"),
               d %>% slice_max(gap, n = 3) %>% pull(iso),
               d %>% slice_min(gap, n = 2) %>% pull(iso))

  d <- d %>% mutate(hl = iso %in% lab_iso)

  d_lab <- d %>%
    filter(hl) %>%
    mutate(name = all_names[iso])

  lims <- range(c(d$gdp, d$y))

  x_title <- ifelse(lang == "de", "Reales BIP pro Kopf", "Real GDP per capita")

  ggplot(d, aes(x = gdp, y = y)) +
    geom_abline(slope = 1, intercept = 0, colour = design_colours()[["ink"]], linewidth = .4) +
    geom_zeroline() +
    geom_vline(xintercept = 0, colour = design_colours()[["ink"]], linewidth = .5) +
    geom_point(data = filter(d, !hl), colour = design_colours()[["mute"]], size = 1.8) +
    geom_point(data = filter(d, hl), aes(colour = gap > 0), size = 2.2,
               show.legend = FALSE) +
    ggrepel::geom_text_repel(data = d_lab, aes(label = name, colour = gap > 0),
                             size = 3, family = font_corporate_design, seed = 1,
                             min.segment.length = .2, box.padding = .3,
                             show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = design_colours()[["red"]],
                                   `FALSE` = design_colours()[["blue"]])) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                       breaks = seq(-.2, .2, .02), limits = lims) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                       breaks = seq(-.2, .2, .02), limits = lims) +
    coord_equal() +
    labs(title = fig_title,
         subtitle = fig_subtitle,
         x = x_title, y = y_title,
         caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
    theme_corporate_design(base_size = 11, grid = "both")
}

share_cap <- mean(avg$gap_cap[avg$iso != "WLD"] > 0)

if (lang == "de") {
  g_con <- scatter_growth(
    "con",
    fig_title = "Konsum und BIP: meist im Gleichschritt",
    fig_subtitle = paste0("Durchschnittliches jährliches Wachstum pro Kopf, ", min_year, "–", max_year,
                          ", ", n_ctry, " Länder.\nÜber der Linie wuchs der Konsum schneller als das BIP."),
    y_title = "Realer Konsum pro Kopf",
    fig_note = paste0("Rohstoffexporteure gewinnen Kaufkraft über Terms of Trade, aufholende ",
                      "Volkswirtschaften investieren mehr.\nIrland: Gewinne multinationaler ",
                      "Konzerne erhöhen das BIP, nicht den Konsum."))
  g_cap <- scatter_growth(
    "cap",
    fig_title = "Der Kapitalstock wächst schneller als das BIP",
    fig_subtitle = paste0("Durchschnittliches jährliches Wachstum pro Kopf, ", min_year, "–", max_year,
                          ", ", n_ctry, " Länder.\nÜber der Linie (", round(share_cap * 100),
                          " % der Länder) wuchs das Kapital schneller als das BIP."),
    y_title = "Realer Kapitalstock pro Kopf")
} else {
  g_con <- scatter_growth(
    "con",
    fig_title = "Consumption and GDP: mostly in step",
    fig_subtitle = paste0("Average annual growth per capita, ", min_year, "–", max_year,
                          ", ", n_ctry, " countries.\nAbove the line, consumption grew faster than GDP."),
    y_title = "Real consumption per capita",
    fig_note = paste0("Commodity exporters gain purchasing power through the terms of trade, ",
                      "catching-up economies invest more.\nIreland: profits of multinationals ",
                      "raise GDP, not consumption."))
  g_cap <- scatter_growth(
    "cap",
    fig_title = "Capital grows faster than GDP",
    fig_subtitle = paste0("Average annual growth per capita, ", min_year, "–", max_year,
                          ", ", n_ctry, " countries.\nAbove the line (", round(share_cap * 100),
                          "% of countries), capital grew faster than GDP."),
    y_title = "Real capital stock per capita")
}

g_con

save_chart(g_con, "gdp-consumption-growth-countries", lang = lang, format = "portrait")

g_cap

save_chart(g_cap, "gdp-capital-growth-countries", lang = lang, format = "portrait")

# Chart 4: decade growth against the decade's average level -----------------
# Convergence check: do countries at a low level grow faster?
# Levels are at constant 2021 national prices converted at 2021 PPPs, so they
# are comparable across countries around 2021 and only approximately earlier.
# Non-overlapping decades 1971-1980, ..., 2011-2020. Within each decade: the
# mean of the year-on-year growth rates against the level in the base year
# (1970, 1980, ...), i.e. before any of that growth. The last complete decade
# ends 2020.
dec_start <- seq(min_year + 1, max_year - 9, 10)
dec_labels <- paste0(dec_start, "–", dec_start + 9)

if (lang == "de") {
  facet_names <- c(gdp = "BIP pro Kopf\n(Tsd. $)", con = "Konsum pro Kopf\n(Tsd. $)",
                   cap = "Kapital pro Kopf\n(Tsd. $)", prod = "BIP je Erwerbstätigen\n(Tsd. $)")
} else {
  facet_names <- c(gdp = "GDP per capita\n(thousand $)", con = "Consumption\nper capita (thousand $)",
                   cap = "Capital per capita\n(thousand $)", prod = "GDP per worker\n(thousand $)")
}

conv <- pwt %>%
  # Labour productivity: real GDP per person employed, from total GDP
  mutate(prod = gdp / emp,
         across(c(gdp, con, cap), ~ .x / pop)) %>%
  select(iso, year, gdp, con, cap, prod) %>%
  pivot_longer(c(gdp, con, cap, prod), names_to = "var", values_to = "value") %>%
  group_by(iso, var) %>%
  arrange(year) %>%
  mutate(growth = value / lag(value) - 1, base = lag(value)) %>%
  ungroup() %>%
  filter(year > min_year) %>%
  mutate(start = min_year + 1 + 10 * ((year - min_year - 1) %/% 10)) %>%
  filter(start %in% dec_start) %>%
  group_by(iso, var, start) %>%
  summarise(level = base[year == min(year)], growth = mean(growth), .groups = "drop") %>%
  # Employment is missing for some countries and years: keep complete decades only
  filter(!is.na(level), !is.na(growth)) %>%
  # PWT money values are in millions of 2021 US$, population and employment in
  # millions: per-capita and per-worker values are in US$, shown in thousands
  mutate(level = level / 1e3,
         decade = factor(paste0(start, "–", start + 9), levels = dec_labels),
         var = factor(facet_names[var], levels = unname(facet_names)))

# Convergence clubs (Phillips & Sul 2007, 2009) ------------------------------
# Log t test on HP-filtered log GDP per capita 1970 to the last year, then the
# core-group-and-sieve clustering. Implemented here rather than via the
# ConvergenceClubs package; club borders are sensitive to the sorting and the
# sieve threshold, so treat marginal members with care.
hp_trend <- function(x, lambda = 400) {
  n <- length(x)
  D <- diff(diag(n), differences = 2)
  as.vector(solve(diag(n) + lambda * crossprod(D), x))
}

# Slope b and HAC t statistic of log(H1/Ht) - 2 log(log t) on log t
log_t <- function(x, r = 1 / 3) {
  h <- x / matrix(colMeans(x), nrow(x), ncol(x), byrow = TRUE)
  H <- colMeans((h - 1)^2)
  tt <- floor(r * length(H)):length(H)
  y <- log(H[1] / H[tt]) - 2 * log(log(tt))
  z <- cbind(1, log(tt))
  m <- lm.fit(z, y)
  u <- m$residuals
  n <- length(u)
  # Newey-West variance, Bartlett kernel
  S <- crossprod(z * u)
  for (l in seq_len(floor(4 * (n / 100)^(2 / 9)))) {
    w <- 1 - l / (floor(4 * (n / 100)^(2 / 9)) + 1)
    G <- crossprod(z[(l + 1):n, , drop = FALSE] * u[(l + 1):n],
                   z[1:(n - l), , drop = FALSE] * u[1:(n - l)])
    S <- S + w * (G + t(G))
  }
  B <- solve(crossprod(z))
  c(b = unname(m$coefficients[2]), t = unname(m$coefficients[2] / sqrt((B %*% S %*% B)[2, 2])))
}

find_clubs <- function(x, crit = -1.65) {
  x <- x[order(x[, ncol(x)], decreasing = TRUE), , drop = FALSE]
  clubs <- list()
  rest <- rownames(x)
  while (length(rest) >= 2) {
    z <- x[rest, , drop = FALSE]
    if (log_t(z)["t"] > crit) {
      clubs[[length(clubs) + 1]] <- rest
      rest <- character()
      break
    }
    # Core group: the first pair that converges, extended while t stays above
    # the critical value, cut where t peaks
    start <- NA
    for (i in seq_len(nrow(z) - 1)) {
      if (log_t(z[i:(i + 1), ])["t"] > crit) { start <- i; break }
    }
    if (is.na(start)) break
    tk <- sapply((start + 1):nrow(z), function(k) log_t(z[start:k, , drop = FALSE])["t"])
    run <- seq_len(which(c(tk <= crit, TRUE))[1] - 1)
    core <- rownames(z)[start:(start + run[which.max(tk[run])])]
    # Sieve: add every other country that keeps t positive
    cand <- setdiff(rownames(z), core)
    club <- c(core, cand[vapply(cand, function(cc) log_t(z[c(core, cc), ])["t"] > 0, logical(1))])
    if (log_t(z[club, ])["t"] <= crit) club <- core
    clubs[[length(clubs) + 1]] <- club
    rest <- setdiff(rest, club)
  }
  list(clubs = clubs, divergent = rest)
}

gdp_pc <- pwt %>%
  mutate(y = log(gdp / pop)) %>%
  select(iso, year, y) %>%
  pivot_wider(names_from = year, values_from = y)
gdp_pc_mat <- as.matrix(gdp_pc[, -1])
rownames(gdp_pc_mat) <- gdp_pc$iso

clubs <- find_clubs(t(apply(gdp_pc_mat, 1, hp_trend)))

club_of <- c(unlist(lapply(seq_along(clubs$clubs), function(i)
                 setNames(rep(paste0("club", i), length(clubs$clubs[[i]])), clubs$clubs[[i]]))),
             setNames(rep("div", length(clubs$divergent)), clubs$divergent))

y_cap_conv <- c(-.08, .12)

# Slope of the linear trend per panel: growth in pp per tenfold higher level
conv_slopes <- conv %>%
  group_by(var, decade) %>%
  summarise(slope = coef(lm(growth ~ log10(level)))[2], .groups = "drop") %>%
  mutate(label = paste0(ifelse(round(slope * 100, 1) >= 0, "+", "−"),
                        formatC(abs(slope) * 100, format = "f", digits = 1,
                                decimal.mark = dec_mark)))

# Club legend: members and median GDP per capita in the last year
club_n <- table(club_of)
club_gdp <- tapply(exp(gdp_pc_mat[names(club_of), ncol(gdp_pc_mat)]) / 1e3, club_of, median)
money_k <- function(x) {
  v <- formatC(x, format = "f", digits = 0)
  if (lang == "de") paste0(v, " Tsd. $") else paste0("$", v, "k")
}
club_names <- vapply(names(club_n), function(k) {
  if (k == "div") {
    paste0(ifelse(lang == "de", "Keinem Club zugeordnet (", "No club ("), club_n[[k]], ")")
  } else {
    paste0("Club ", sub("club", "", k), " (", club_n[[k]], ifelse(lang == "de", " Länder, ", " countries, "),
           money_k(club_gdp[[k]]), ")")
  }
}, character(1))
club_colours <- setNames(c(design_colours()[["blue"]], design_colours()[["amber"]],
                           design_colours()[["red"]], design_colours()[["violet"]],
                           design_colours()[["teal"]])[seq_along(clubs$clubs)],
                         club_names[paste0("club", seq_along(clubs$clubs))])
if (length(clubs$divergent)) club_colours[club_names[["div"]]] <- design_colours()[["rest"]]

conv <- conv %>%
  mutate(club = factor(club_names[club_of[iso]], levels = names(club_colours)))

if (lang == "de") {
  fig_title <- "Wachsen die Kleinen schneller?"
  fig_subtitle <- paste0("Ø jährliches Wachstum je Jahrzehnt (vertikal) gegen das Niveau\n",
                         "zu Beginn des Jahrzehnts (horizontal, log. Skala), ", n_distinct(conv$iso),
                         " Länder.\nLinien: linearer Trend je Club, gestrichelt alle Länder. Zahl: Steigung aller Länder\nin PP je Verzehnfachung des Niveaus.")
  fig_note <- paste0("Werte in US-Dollar zu Preisen und Kaufkraftparitäten von 2021. ",
                     "Achse begrenzt auf −8 bis +12 %.\n",
                     "BIP je Erwerbstätigen: nur Länder mit Beschäftigungsdaten für das ganze Jahrzehnt.\n",
                     "Clubs: Konvergenzclubs nach Phillips und Sul (2009) für BIP pro Kopf ",
                     min_year, "–", max_year, "; in Klammern der Median ", max_year, ".")
} else {
  fig_title <- "Do the small ones grow faster?"
  fig_subtitle <- paste0("Avg. annual growth per decade (vertical) against the level\n",
                         "at the start of the decade (horizontal, log scale), ", n_distinct(conv$iso),
                         " countries.\nLines: linear trend per club, dashed all countries. Number: slope for all countries\nin pp per tenfold higher level.")
  fig_note <- paste0("Values in US dollars at 2021 prices and purchasing power parities. ",
                     "Axis capped at −8 to +12%.\n",
                     "GDP per worker: only countries with employment data for the whole decade.\n",
                     "Clubs: convergence clubs following Phillips and Sul (2009) for GDP per capita ",
                     min_year, "–", max_year, "; in brackets the median in ", max_year, ".")
}

g_conv <- ggplot(conv, aes(x = level, y = growth)) +
  geom_zeroline() +
  # Largest club first, so the small clubs are drawn on top
  geom_point(data = arrange(conv, club), aes(colour = club), size = .8, alpha = .55) +
  # Trend per club (countries without a club are left out), then all countries
  geom_smooth(data = filter(conv, club != club_names[["div"]]), aes(colour = club),
              method = "lm", formula = y ~ x, se = FALSE, linewidth = .8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = .6,
              linetype = "22",
              colour = design_colours()[["ink"]]) +
  geom_text(data = conv_slopes, aes(label = label), x = Inf, y = Inf,
            hjust = 1.1, vjust = 1.4, size = 3, fontface = "bold",
            family = font_corporate_design, colour = design_colours()[["ink"]]) +
  facet_grid(decade ~ var, scales = "free_x") +
  scale_colour_manual(values = club_colours, drop = FALSE) +
  scale_x_log10(labels = scales::label_number(accuracy = NULL, decimal.mark = dec_mark,
                                              big.mark = ifelse(lang == "de", ".", ","),
                                              drop0trailing = TRUE),
                breaks = 10^(-2:3)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = seq(-.05, .1, .05)) +
  coord_cartesian(ylim = y_cap_conv) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(size = 2.5, alpha = 1))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max(dec_start) + 9, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "both") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        strip.text.x = element_text(size = 8.5, hjust = 0),
        strip.text.y = element_text(size = 8.5, angle = 0, hjust = 0),
        panel.spacing = unit(8, "pt"))

g_conv

save_chart(g_conv, "decade-growth-vs-level", lang = lang, format = "portrait")

# Chart 5: rich countries -- consumption, GDP and productivity growth ---------
# Richest third of countries by consumption per capita at the start of each
# decade, oil exporters excluded (population booms and falling GDP per capita
# there would dominate the averages). Same non-overlapping decades as chart 4.
oil_iso <- c("KWT", "ARE", "QAT", "SAU", "BRN", "OMN", "BHR", "TTO", "GAB")

rich <- pwt %>%
  filter(!iso %in% oil_iso) %>%
  group_by(iso) %>%
  arrange(year) %>%
  mutate(con_pc0 = lag(con / pop),
         g_con = (con / pop) / lag(con / pop) - 1,
         g_gdp = (gdp / pop) / lag(gdp / pop) - 1,
         g_prod = (gdp / emp) / lag(gdp / emp) - 1) %>%
  ungroup() %>%
  filter(year > min_year) %>%
  mutate(start = min_year + 1 + 10 * ((year - min_year - 1) %/% 10)) %>%
  filter(start %in% dec_start) %>%
  group_by(iso, start) %>%
  summarise(level = con_pc0[year == min(year)],
            across(c(g_con, g_gdp, g_prod), mean), .groups = "drop") %>%
  group_by(start) %>%
  filter(level >= quantile(level, 2 / 3)) %>%
  ungroup()

n_rich <- round(mean(table(rich$start)))

rich_avg <- rich %>%
  group_by(start) %>%
  # GDP per worker only where employment covers the whole decade
  summarise(across(c(g_con, g_gdp, g_prod), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-start, names_to = "var", values_to = "value")

if (lang == "de") {
  rich_names <- c(g_con = "Konsum pro Kopf", g_gdp = "BIP pro Kopf", g_prod = "BIP je Erwerbstätigen")
  fig_title <- "Reiche Länder: Konsum wächst so langsam wie die Produktion"
  fig_subtitle <- paste0("Ø jährliches Wachstum je Jahrzehnt, reichstes Drittel der Länder nach Konsum\n",
                         "pro Kopf zu Beginn des Jahrzehnts (rund ", n_rich, " Länder), ohne Ölexporteure.")
  fig_note <- paste0("Ungewichteter Durchschnitt der Länder. Konsum: Haushalte und Staat. ",
                     "2011–2020 einschließlich des Pandemiejahres 2020.")
} else {
  rich_names <- c(g_con = "Consumption per capita", g_gdp = "GDP per capita", g_prod = "GDP per worker")
  fig_title <- "Rich countries: consumption slows just like production"
  fig_subtitle <- paste0("Average annual growth per decade, richest third of countries by consumption\n",
                         "per capita at the start of the decade (about ", n_rich, " countries), excl. oil exporters.")
  fig_note <- paste0("Unweighted average across countries. Consumption: households and government. ",
                     "2011–2020 includes the pandemic year 2020.")
}

rich_avg <- rich_avg %>%
  mutate(var = factor(rich_names[var], levels = unname(rich_names)),
         decade = factor(paste0(start, "–", start + 9), levels = dec_labels),
         label = formatC(value * 100, format = "f", digits = 1, decimal.mark = dec_mark))

rich_colours <- setNames(unname(design_colours()[c("red", "blue", "teal")]), unname(rich_names))

g_rich <- ggplot(rich_avg, aes(x = decade, y = value, fill = var)) +
  geom_col(position = position_dodge(width = .8), width = .75) +
  geom_text(aes(label = label, vjust = ifelse(value >= 0, -.5, 1.5)),
            position = position_dodge(width = .8), size = 3,
            family = font_corporate_design, colour = design_colours()[["ink"]]) +
  geom_zeroline() +
  scale_fill_manual(values = rich_colours) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = seq(0, .03, .01),
                     expand = expansion(mult = c(.05, .1))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max(dec_start) + 9, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank())

g_rich

save_chart(g_rich, "rich-countries-consumption-gdp-productivity", lang = lang, format = "portrait")
