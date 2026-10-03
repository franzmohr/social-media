rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

lang <- "de"

# Start of the boom decades used as references. China: the 1978 reforms.
# India: growth only accelerated in 2002 (the 1990s after the 1991 reforms
# averaged under 4 % per head), with capital per worker rising at 7 % a year.
ref_start <- c(CHN = 1978, IND = 2002)
n_years <- 10
# Countries shown per reference
n_match <- 6
min_pop <- 1

# Penn World Table 11.0 -------------------------------------------------------
# dataverse.nl sits behind a bot filter that rejects curl's user agent but lets
# R's own download.file() through.
options(timeout = 600)
pwt_file <- tempfile(fileext = ".xlsx")
download.file("https://dataverse.nl/api/access/datafile/554105",
              pwt_file, mode = "wb", quiet = TRUE)

# rgdpe: expenditure-side real GDP at 2021 PPPs, for levels relative to the US.
# rgdpna, rconna, rnna: GDP, consumption (households and government) and
# capital stock at constant national prices, for growth.
pwt <- read_excel(pwt_file, sheet = "Data") %>%
  select(iso = countrycode, country, year, pop, emp, lvl = rgdpe, gdp = rgdpna,
         con = rconna, cap = rnna, inv = csh_i, hc)

# Economic Complexity Index, Harvard Growth Lab --------------------------------
# SITC classification, the only one reaching back to 1978. The ECI is
# standardised across countries each year, so it measures a country's
# complexity relative to the world at the time.
eci_file <- tempfile(fileext = ".csv")
download.file("https://dataverse.harvard.edu/api/access/datafile/13685143",
              eci_file, mode = "wb", quiet = TRUE)
eci <- read.csv(eci_file) %>%
  select(iso = country_iso3_code, year, eci)

pwt <- left_join(pwt, eci, by = c("iso", "year"))

max_year <- max(pwt$year)
cur_start <- max_year - n_years

us <- pwt %>%
  filter(iso == "USA") %>%
  transmute(year, us = lvl / pop, us_con = con / pop)

pwt <- pwt %>%
  left_join(us, by = "year") %>%
  mutate(rel = lvl / pop / us,
         rel_con = con / pop / us_con)

# Profile of a ten-year window: level, human capital and complexity at the
# start, then growth and investment over the window. Growth in log points per year.
profile <- function(start) {
  pwt %>%
    filter(year >= start, year <= start + n_years) %>%
    group_by(iso, country) %>%
    arrange(year) %>%
    filter(n() == n_years + 1) %>%
    summarise(rel = first(rel),
              g_gdp = (log(last(gdp / pop)) - log(first(gdp / pop))) / n_years,
              # Capital deepening: capital per person employed
              g_cap = (log(last(cap / emp)) - log(first(cap / emp))) / n_years,
              inv = mean(inv),
              hc = first(hc),
              eci = first(eci),
              # Last, so the per-capita terms above still see the population series
              pop = last(pop),
              .groups = "drop") %>%
    mutate(start = start)
}

refs <- bind_rows(lapply(names(ref_start), function(i) filter(profile(ref_start[[i]]), iso == i)))

cur <- profile(cur_start) %>%
  filter(pop >= min_pop) %>%
  drop_na(rel, g_gdp, g_cap, inv, eci)

n_ctry <- nrow(cur)

# Similarity: Euclidean distance over six features, each scaled by its
# standard deviation across countries today. The level enters in logs, so
# 2 % and 4 % of the US are as far apart as 20 % and 40 %. The ECI separates
# a broad export base like China's and India's from capital built on debt or
# a single resource.
#
# Leapfrogging: a country ahead of China or India on output growth, human
# capital or export complexity is not less similar for it. For these three
# features only a shortfall counts. Level, capital growth and investment stay
# two-sided: more capital without matching output points to waste or debt.
#
# The human capital index is missing for some countries (e.g. Uzbekistan).
# Their distance is computed over the features they have and scaled up to six,
# so it stays comparable.
feat <- c("rel", "g_gdp", "g_cap", "inv", "hc", "eci")
one_sided <- c("g_gdp", "hc", "eci")
feat_mat <- function(x) cbind(log(x$rel), as.matrix(x[feat[-1]]))
sds <- apply(feat_mat(cur), 2, sd, na.rm = TRUE)

dist <- sapply(names(ref_start), function(i) {
  r <- feat_mat(filter(refs, iso == i))
  z <- sweep(sweep(feat_mat(cur), 2, r), 2, sds, "/")
  z[, feat %in% one_sided] <- pmin(z[, feat %in% one_sided], 0)
  sqrt(rowSums(z^2, na.rm = TRUE) * length(feat) / rowSums(!is.na(z)))
})
cur <- bind_cols(cur, as_tibble(dist, .name_repair = ~ paste0("d_", .x)))

matches <- lapply(names(ref_start), function(i) {
  cur %>%
    filter(!iso %in% names(ref_start)) %>%
    slice_min(.data[[paste0("d_", i)]], n = n_match) %>%
    pull(iso)
})
names(matches) <- names(ref_start)
match_iso <- unique(unlist(matches))

print(cur %>% filter(iso %in% match_iso) %>% arrange(d_IND) %>%
        select(iso, country, all_of(feat), d_CHN, d_IND), width = 200)

if (lang == "de") {
  names_loc <- c(CHN = "China", IND = "Indien", BGD = "Bangladesch", KHM = "Kambodscha",
                 LAO = "Laos", TZA = "Tansania", VNM = "Vietnam", CIV = "Elfenbeinküste",
                 NPL = "Nepal", ETH = "Äthiopien", SEN = "Senegal", MMR = "Myanmar",
                 BEN = "Benin", IDN = "Indonesien", MAR = "Marokko", UGA = "Uganda",
                 KEN = "Kenia", RWA = "Ruanda", PAK = "Pakistan", PHL = "Philippinen",
                 EGY = "Ägypten", GHA = "Ghana", COD = "DR Kongo", MOZ = "Mosambik",
                 UZB = "Usbekistan", KGZ = "Kirgisistan", TJK = "Tadschikistan", LKA = "Sri Lanka",
                 DJI = "Dschibuti", ROU = "Rumänien", BIH = "Bosnien-Herzegowina")
} else {
  names_loc <- c(TZA = "Tanzania", LAO = "Laos", VNM = "Vietnam", CIV = "Côte d'Ivoire",
                 COD = "DR Congo")
}
ctry_name <- function(iso) {
  pwt_names <- setNames(cur$country, cur$iso)
  ifelse(iso %in% names(names_loc), names_loc[iso], pwt_names[iso])
}

dec_mark <- ifelse(lang == "de", ",", ".")
src <- paste0("Penn World Table 11.0 (rgdpe, rgdpna, rconna, rnna, pop, emp, csh_i, hc);\n",
              "Harvard Growth Lab, Atlas of Economic Complexity (ECI, SITC).")

source("r-corporate-design-functions-ggplot2.R")

ref_colours <- c(CHN = design_colours()[["red"]], IND = design_colours()[["blue"]])

# Chart 1: fingerprint -- the six features side by side --------------------
# One row per country, one panel per feature. Vertical lines: China and India
# at the start of their boom. Rows sorted by similarity to India's start,
# China's and India's own boom decades on top.
ref_label <- function(i) paste0(ctry_name(i), " ", ref_start[i], "–", ref_start[i] + n_years)

if (lang == "de") {
  feat_names <- c(rel = "BIP pro Kopf\nzu Beginn, % der USA",
                  g_gdp = "Wachstum BIP\npro Kopf ↑",
                  g_cap = "Wachstum Kapital\nje Erwerbstätigen",
                  inv = "Investitionen\nin % des BIP",
                  hc = "Humankapital-\nindex zu Beginn ↑",
                  eci = "Export-\nkomplexität (ECI) ↑")
  fig_title <- "Wer heute so startet wie China und Indien"
  fig_subtitle <- paste0("Profil von ", cur_start, "–", max_year, " im Vergleich zum Beginn des Booms in\n",
                         "China (", ref_start[["CHN"]], "–", ref_start[["CHN"]] + n_years, ") und Indien (",
                         ref_start[["IND"]], "–", ref_start[["IND"]] + n_years,
                         "). Wachstum: Ø pro Jahr. Die je ", n_match, " ähnlichsten\nvon ",
                         n_ctry, " Ländern, sortiert nach Ähnlichkeit mit Indien.\n",
                         "Farbe: unter den ähnlichsten zu China (rot), Indien (blau) oder beiden (violett).")
  fig_note <- paste0("Ähnlichkeit: Abstand über alle sechs Merkmale, jeweils geteilt durch die Streuung über die Länder heute;\n",
                     "Niveau logarithmiert. ↑: Überholen erlaubt, nur ein Rückstand auf China bzw. Indien zählt.\n",
                     "In Klammern: Abstand zu China | Indien (kleiner = ähnlicher). Länder ab 1 Mio. Einwohner;\n",
                     "fehlt das Humankapital (z. B. Usbekistan), zählen die übrigen Merkmale. ",
                     "Niveaus zu Kaufkraftparitäten von 2021,\nüber Jahrzehnte nur ungefähr vergleichbar.")
} else {
  feat_names <- c(rel = "GDP per capita\nat start, % of US",
                  g_gdp = "Growth GDP\nper capita ↑",
                  g_cap = "Growth capital\nper worker",
                  inv = "Investment\nin % of GDP",
                  hc = "Human capital\nindex at start ↑",
                  eci = "Export com-\nplexity (ECI) ↑")
  fig_title <- "Who starts today the way China and India did"
  fig_subtitle <- paste0("Profile of ", cur_start, "–", max_year, " compared with the start of the boom in\n",
                         "China (", ref_start[["CHN"]], "–", ref_start[["CHN"]] + n_years, ") and India (",
                         ref_start[["IND"]], "–", ref_start[["IND"]] + n_years,
                         "). Growth: avg. per year. The ", n_match, " most similar\nto each out of ",
                         n_ctry, " countries, sorted by similarity to India.\n",
                         "Colour: among the most similar to China (red), India (blue) or both (violet).")
  fig_note <- paste0("Similarity: distance across all six features, each divided by its spread across countries today;\n",
                     "level in logs. ↑: leapfrogging allowed, only a shortfall against China or India counts.\n",
                     "In brackets: distance to China | India (smaller = more similar). Countries with 1 million people or more;\n",
                     "where human capital is missing (e.g. Uzbekistan), the other features count. ",
                     "Levels at 2021 PPPs, only roughly\ncomparable across decades.")
}

# Row labels: reference decades as they are, today's countries with their
# distance to China | India in brackets
dist_lab <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = dec_mark)
row_lab <- cur %>%
  filter(iso %in% match_iso) %>%
  arrange(d_IND) %>%
  mutate(label = paste0(ctry_name(iso), " (", dist_lab(d_CHN), " | ", dist_lab(d_IND), ")"))
row_lab <- c(setNames(ref_label(names(ref_start)), names(ref_start)),
             setNames(row_lab$label, row_lab$iso))

fp <- bind_rows(refs %>% mutate(grp = iso),
                cur %>% filter(iso %in% match_iso) %>%
                  mutate(grp = ifelse(iso %in% matches$IND & !iso %in% matches$CHN, "IND",
                                      ifelse(iso %in% matches$CHN & !iso %in% matches$IND, "CHN", "both")))) %>%
  pivot_longer(all_of(feat), names_to = "var", values_to = "value") %>%
  mutate(var = factor(feat_names[var], levels = unname(feat_names)),
         name = factor(row_lab[iso], levels = rev(unname(row_lab))),
         is_ref = iso %in% names(ref_start))

fp_ref <- fp %>%
  filter(is_ref) %>%
  select(grp, var, ref_value = value)

grp_colours <- c(ref_colours, both = design_colours()[["violet"]])

g_fp <- ggplot(fp, aes(x = value, y = name)) +
  geom_vline(data = fp_ref, aes(xintercept = ref_value, colour = grp),
             linewidth = .5, linetype = "22", show.legend = FALSE) +
  geom_point(aes(colour = grp, shape = is_ref), size = 2.6, show.legend = FALSE, na.rm = TRUE) +
  facet_wrap(~ var, ncol = 3, scales = "free_x") +
  scale_colour_manual(values = grp_colours) +
  scale_shape_manual(values = c(`TRUE` = 18, `FALSE` = 16)) +
  # The labeller sees one panel's breaks at a time: shares and growth rates lie
  # in [0, 1) and get percent labels; the human capital index (>= 1) and the
  # ECI (standardised, with negative values) get plain numbers
  scale_x_continuous(labels = function(x) {
    if (any(x < 0 | x >= 1, na.rm = TRUE)) {
      formatC(x, format = "f", digits = 1, decimal.mark = dec_mark)
    } else {
      scales::label_percent(accuracy = 1, decimal.mark = dec_mark)(x)
    }
  }, breaks = scales::breaks_extended(n = 4), expand = expansion(mult = .12)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "x") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 7.5),
        axis.text.y = element_text(size = 8.5),
        strip.text = element_text(size = 8.5, hjust = 0),
        panel.spacing.x = unit(14, "pt"),
        panel.spacing.y = unit(16, "pt"))

g_fp

save_chart(g_fp, "catch-up-boom-analogues-profile", lang = lang, format = "portrait")

# Charts 2 to 4: paths since the start -----------------------------------------
# China from 1978 and India from 2002 as long as data run, the closest
# matches since today's window started. Same x axis: years since the start.
# GDP and consumption per capita relative to the US; capital per worker as an
# index, since its dollar level hinges on the capital-goods PPP and is not
# comparable across countries.
path_iso <- c(names(ref_start), match_iso)
start_of <- c(ref_start, setNames(rep(cur_start, length(match_iso)), match_iso))

paths <- pwt %>%
  filter(iso %in% path_iso) %>%
  mutate(t = year - start_of[iso]) %>%
  filter(t >= 0) %>%
  group_by(iso) %>%
  arrange(year) %>%
  mutate(cap_idx = (cap / emp) / first(cap / emp) * 100) %>%
  ungroup() %>%
  mutate(name = ifelse(iso %in% names(ref_start),
                       paste0(ctry_name(iso), " ", start_of[iso]), ctry_name(iso)),
         is_ref = iso %in% names(ref_start))

path_colours <- c(setNames(unname(ref_colours), paste0(ctry_name(names(ref_start)), " ", ref_start)),
                  setNames(unname(design_colours()[c("amber", "teal", "violet", "rose", "olive", "rust",
                                                  "slate", "rest")])[seq_along(match_iso)],
                           ctry_name(match_iso)))

t_max <- 30

if (lang == "de") {
  x_title <- "Jahre seit Beginn"
  sub_start <- paste0("China ab ", ref_start[["CHN"]], ", Indien ab ", ref_start[["IND"]],
                      ", die ähnlichsten Länder ab ", cur_start, ".")
} else {
  x_title <- "Years since start"
  sub_start <- paste0("China from ", ref_start[["CHN"]], ", India from ", ref_start[["IND"]],
                      ", the most similar countries from ", cur_start, ".")
}

path_chart <- function(var, y_labels, y_breaks, fig_title, fig_subtitle, fig_note) {
  d <- paths %>%
    filter(t <= t_max, !is.na(.data[[var]])) %>%
    mutate(y = .data[[var]])

  d_end <- d %>%
    group_by(iso) %>%
    filter(t == max(t)) %>%
    ungroup()

  ggplot(d, aes(x = t, y = y, group = iso, colour = name)) +
    geom_line(data = ~ filter(.x, !is_ref), linewidth = .6) +
    geom_line(data = ~ filter(.x, is_ref), linewidth = 1.3) +
    ggrepel::geom_text_repel(data = d_end, aes(label = name),
                             hjust = 0, direction = "y", nudge_x = .8,
                             segment.colour = design_colours()[["grid"]], segment.size = .3,
                             min.segment.length = 0, box.padding = .25, force = 2, size = 2.8,
                             fontface = "bold", family = font_corporate_design, seed = 1) +
    geom_vline(xintercept = n_years, colour = design_colours()[["ink_soft"]],
               linewidth = .3, linetype = "22") +
    scale_colour_manual(values = path_colours) +
    scale_x_continuous(breaks = seq(0, t_max, 5), limits = c(0, t_max + 6),
                       expand = expansion(mult = c(.01, 0))) +
    scale_y_log10(labels = y_labels, breaks = y_breaks) +
    labs(title = fig_title,
         subtitle = fig_subtitle,
         x = x_title,
         caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
    theme_corporate_design(base_size = 11, grid = "y") +
    theme(axis.title.y = element_blank(),
          legend.position = "none")
}

pct_breaks <- c(.01, .02, .03, .05, .1, .15, .2, .3, .5)
pct_labels <- scales::label_percent(accuracy = 1, decimal.mark = dec_mark)
idx_labels <- scales::label_number(big.mark = ifelse(lang == "de", ".", ","), decimal.mark = dec_mark)

if (lang == "de") {
  g_path <- path_chart(
    "rel", pct_labels, pct_breaks,
    fig_title = "Auf demselben Pfad?",
    fig_subtitle = paste0("BIP pro Kopf in % der USA (log. Skala) nach Jahren seit Beginn.\n", sub_start),
    fig_note = "Kaufkraftparitäten von 2021 (rgdpe). Über Jahrzehnte nur ungefähr vergleichbar.")
  g_path_con <- path_chart(
    "rel_con", pct_labels, pct_breaks,
    fig_title = "Kommt das Wachstum beim Konsum an?",
    fig_subtitle = paste0("Realer Konsum pro Kopf in % der USA (log. Skala) nach Jahren seit Beginn.\n", sub_start),
    fig_note = paste0("Konsum: Haushalte und Staat, konstante nationale Preise umgerechnet mit Kaufkraftparitäten von 2021 (rconna).\n",
                      "Über Jahrzehnte nur ungefähr vergleichbar."))
  g_path_cap <- path_chart(
    "cap_idx", idx_labels, c(100, 150, 200, 300, 500, 1000, 2000),
    fig_title = "Wie schnell der Kapitalstock wächst",
    fig_subtitle = paste0("Realer Kapitalstock je Erwerbstätigen, Index Beginn = 100 (log. Skala),\n",
                          "nach Jahren seit Beginn.\n", sub_start),
    fig_note = paste0("Konstante nationale Preise (rnna). Als Index, weil das Niveau in Dollar von der Kaufkraftparität\n",
                      "für Investitionsgüter abhängt und zwischen Ländern kaum vergleichbar ist."))
} else {
  g_path <- path_chart(
    "rel", pct_labels, pct_breaks,
    fig_title = "On the same path?",
    fig_subtitle = paste0("GDP per capita in % of the US (log scale) by years since the start.\n", sub_start),
    fig_note = "2021 PPPs (rgdpe). Only roughly comparable across decades.")
  g_path_con <- path_chart(
    "rel_con", pct_labels, pct_breaks,
    fig_title = "Does growth reach consumption?",
    fig_subtitle = paste0("Real consumption per capita in % of the US (log scale) by years since the start.\n", sub_start),
    fig_note = paste0("Consumption: households and government, constant national prices converted at 2021 PPPs (rconna).\n",
                      "Only roughly comparable across decades."))
  g_path_cap <- path_chart(
    "cap_idx", idx_labels, c(100, 150, 200, 300, 500, 1000, 2000),
    fig_title = "How fast the capital stock grows",
    fig_subtitle = paste0("Real capital stock per worker, index start = 100 (log scale),\n",
                          "by years since the start.\n", sub_start),
    fig_note = paste0("Constant national prices (rnna). As an index because the dollar level hinges on the PPP for\n",
                      "capital goods and is hardly comparable across countries."))
}

g_path

save_chart(g_path, "catch-up-boom-analogues-paths", lang = lang, format = "portrait")

g_path_con

save_chart(g_path_con, "catch-up-boom-analogues-paths-consumption", lang = lang, format = "portrait")

g_path_cap

save_chart(g_path_cap, "catch-up-boom-analogues-paths-capital", lang = lang, format = "portrait")
