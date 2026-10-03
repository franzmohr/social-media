rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

lang <- "de"

# Window for "at the moment": the last ten years of PWT
n_years <- 10
# Candidates: below this share of US GDP per capita and at least 1 million people
max_rel <- .5
min_pop <- 1

# Penn World Table 11.0 -------------------------------------------------------
# dataverse.nl sits behind a bot filter that rejects curl's user agent but lets
# R's own download.file() through.
options(timeout = 600)
pwt_file <- tempfile(fileext = ".xlsx")
download.file("https://dataverse.nl/api/access/datafile/554105",
              pwt_file, mode = "wb", quiet = TRUE)

# rgdpe: expenditure-side real GDP at 2021 PPPs, for comparing levels across
# countries. rgdpna, rnna: GDP and capital stock at constant national prices,
# the PWT series meant for growth rates.
pwt <- read_excel(pwt_file, sheet = "Data") %>%
  select(iso = countrycode, country, year, pop, emp, lvl = rgdpe, gdp = rgdpna,
         cap = rnna, inv = csh_i)

max_year <- max(pwt$year)
min_year <- max_year - n_years

cagr <- function(x) (last(x) / first(x))^(1 / n_years) - 1

d <- pwt %>%
  filter(year %in% c(min_year, max_year)) %>%
  group_by(iso, country) %>%
  arrange(year) %>%
  filter(n() == 2) %>%
  summarise(lvl = last(lvl / pop),
            g_gdp = cagr(gdp / pop),
            # Capital deepening: capital per person employed
            g_cap = cagr(cap / emp),
            # Last, so the per-capita terms above still see the population series
            pop = last(pop),
            .groups = "drop") %>%
  filter(!is.na(lvl), !is.na(g_gdp), !is.na(g_cap), pop >= min_pop)

# Average investment share of GDP over the window
inv <- pwt %>%
  filter(year > min_year) %>%
  group_by(iso) %>%
  summarise(inv = mean(inv, na.rm = TRUE), .groups = "drop")

us <- filter(d, iso == "USA")

d <- d %>%
  left_join(inv, by = "iso") %>%
  mutate(rel = lvl / us$lvl,
         # Growth lead over the US in log points; the gap to the US shrinks
         # when positive
         lead = log(1 + g_gdp) - log(1 + us$g_gdp),
         cand = rel < max_rel,
         # Years to reach half of the US level if the lead of the last ten
         # years were kept
         yrs_half = ifelse(cand & lead > 0, log(max_rel / rel) / lead, NA))

# Catch-up score for the candidates: mean z-score of capital deepening and
# GDP per capita growth. Both matter: capital without output growth points to
# wasteful investment, output without capital to commodity booms.
d <- d %>%
  group_by(cand) %>%
  mutate(score = (as.vector(scale(g_cap)) + as.vector(scale(g_gdp))) / 2) %>%
  ungroup() %>%
  mutate(score = ifelse(cand, score, NA))

n_ctry <- nrow(d)
n_cand <- sum(d$cand)

top <- d %>%
  filter(cand) %>%
  slice_max(score, n = 15)

print(select(top, iso, country, rel, g_gdp, g_cap, inv, yrs_half, score), n = 15, width = 200)

if (lang == "de") {
  names_de <- c(MMR = "Myanmar", UZB = "Usbekistan", CHN = "China", ETH = "Äthiopien",
                KHM = "Kambodscha", VNM = "Vietnam", BGD = "Bangladesch",
                TZA = "Tansania", CIV = "Elfenbeinküste", IND = "Indien", RWA = "Ruanda",
                LAO = "Laos", ROU = "Rumänien", TUR = "Türkei", GEO = "Georgien",
                ARM = "Armenien", TJK = "Tadschikistan", NPL = "Nepal", EGY = "Ägypten",
                DOM = "Dominikanische Rep.", BIH = "Bosnien-Herzegowina", UGA = "Uganda",
                KEN = "Kenia", IDN = "Indonesien", PHL = "Philippinen", PAK = "Pakistan",
                NGA = "Nigeria", MNG = "Mongolei", SEN = "Senegal", DJI = "Dschibuti",
                BEN = "Benin", GIN = "Guinea", MOZ = "Mosambik", BFA = "Burkina Faso",
                USA = "USA", SLE = "Sierra Leone", GHA = "Ghana", MAR = "Marokko",
                SRB = "Serbien", MKD = "Nordmazedonien", ALB = "Albanien", KGZ = "Kirgisistan",
                LKA = "Sri Lanka", ZMB = "Sambia", TGO = "Togo", MLI = "Mali", NER = "Niger",
                BGR = "Bulgarien", KAZ = "Kasachstan", COL = "Kolumbien", PER = "Peru",
                BRA = "Brasilien", MEX = "Mexiko", ZAF = "Südafrika", MDA = "Moldau",
                UKR = "Ukraine", TUN = "Tunesien", JOR = "Jordanien")
  src <- "Penn World Table 11.0 (rgdpe, rgdpna, rnna, pop, emp, csh_i)."
} else {
  names_de <- c(TZA = "Tanzania", LAO = "Laos", VNM = "Vietnam", TUR = "Türkiye",
                USA = "United States", CIV = "Côte d'Ivoire", DOM = "Dominican Rep.",
                MDA = "Moldova")
  src <- "Penn World Table 11.0 (rgdpe, rgdpna, rnna, pop, emp, csh_i)."
}
ctry_name <- function(iso) {
  pwt_names <- setNames(d$country, d$iso)
  ifelse(iso %in% names(names_de), names_de[iso], pwt_names[iso])
}

dec_mark <- ifelse(lang == "de", ",", ".")
pct <- function(x, digits = 1) {
  paste0(ifelse(x >= 0, "+", "−"),
         formatC(abs(x) * 100, format = "f", digits = digits, decimal.mark = dec_mark))
}

source("r-corporate-design-functions-ggplot2.R")

# Chart 1: capital deepening against income level ----------------------------
# Every country with at least 1 million people. Upper left: poor, but
# building up capital fast. Colour: GDP per capita grew faster than in the US,
# i.e. the income gap shrank.
d1 <- d %>%
  mutate(conv = factor(ifelse(lead > 0, "yes", "no"), levels = c("yes", "no")),
         lab = iso %in% c(top$iso[1:10], "USA", "IND", "CHN"))

conv_names <- if (lang == "de") {
  c(yes = "Schließt zur USA auf (BIP pro Kopf wächst schneller)", no = "Fällt zurück")
} else {
  c(yes = "Closing the gap to the US (GDP per capita grows faster)", no = "Falling behind")
}

y_rng <- c(-.04, .12)

if (lang == "de") {
  fig_title <- "Arm, aber mit viel neuem Kapital"
  fig_subtitle <- paste0("Wachstum des Kapitalstocks je Erwerbstätigen, Ø pro Jahr ", min_year, "–", max_year,
                         " (vertikal),\ngegen das BIP pro Kopf ", max_year,
                         " in % der USA (horizontal, log. Skala). ", n_ctry, " Länder.\n",
                         "Kreisgröße: Bevölkerung.")
  fig_note <- paste0("BIP pro Kopf zu Kaufkraftparitäten von 2021 (rgdpe). Länder mit mindestens 1 Mio. Einwohnern.\n",
                     "Beschriftet: die zehn Länder mit dem höchsten Aufhol-Score (siehe nächste Grafik) sowie USA, China, Indien.")
  x_title <- "BIP pro Kopf in % der USA"
  y_title <- "Kapital je Erwerbstätigen"
} else {
  fig_title <- "Poor, but building up capital fast"
  fig_subtitle <- paste0("Growth of the capital stock per worker, avg. per year ", min_year, "–", max_year,
                         " (vertical),\nagainst GDP per capita ", max_year,
                         " in % of the US (horizontal, log scale). ", n_ctry, " countries.\n",
                         "Circle size: population.")
  fig_note <- paste0("GDP per capita at 2021 PPPs (rgdpe). Countries with at least 1 million people.\n",
                     "Labelled: the ten countries with the highest catch-up score (see next chart) plus US, China, India.")
  x_title <- "GDP per capita in % of the US"
  y_title <- "Capital per worker"
}

g_scatter <- ggplot(d1, aes(x = rel, y = g_cap)) +
  annotate("rect", xmin = -Inf, xmax = max_rel, ymin = median(d1$g_cap), ymax = Inf,
           fill = design_colours()[["amber"]], alpha = .08) +
  geom_zeroline() +
  geom_vline(xintercept = max_rel, colour = design_colours()[["ink_soft"]],
             linewidth = .3, linetype = "22") +
  geom_hline(yintercept = median(d1$g_cap), colour = design_colours()[["ink_soft"]],
             linewidth = .3, linetype = "22") +
  geom_point(aes(size = pop, colour = conv), alpha = .6, stroke = 0) +
  ggrepel::geom_text_repel(data = filter(d1, lab), aes(label = ctry_name(iso), colour = conv),
                           size = 3, family = font_corporate_design, fontface = "bold", seed = 1,
                           min.segment.length = .2, box.padding = .35,
                           segment.colour = design_colours()[["ink_soft"]], segment.size = .3,
                           show.legend = FALSE) +
  scale_colour_manual(values = c(yes = design_colours()[["red"]], no = design_colours()[["rest"]]),
                      labels = conv_names) +
  scale_size_area(max_size = 16, guide = "none") +
  scale_x_log10(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                breaks = c(.02, .05, .1, .2, .5, 1, 2)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = seq(-.04, .12, .02)) +
  coord_cartesian(ylim = y_rng) +
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 1))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       x = x_title, y = y_title,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "both")

g_scatter

save_chart(g_scatter, "catch-up-capital-vs-income", lang = lang, format = "portrait")

# Chart 2: ranking of the catch-up candidates --------------------------------
# Top 15 countries below half the US level by the catch-up score. Dots: GDP
# per capita and capital per worker growth. Right: level today and, at the
# pace of the last ten years, years until half of the US level.
if (lang == "de") {
  fig_title <- "Wer gerade am schnellsten aufholt"
  fig_subtitle <- paste0("Länder unter ", max_rel * 100, " % des US-BIP pro Kopf (", n_cand,
                         "), die 15 mit dem höchsten Aufhol-Score.\n",
                         "Ø jährliches Wachstum ", min_year, "–", max_year, ".")
  fig_note <- paste0("Aufhol-Score: Mittelwert der standardisierten Wachstumsraten von Kapital je Erwerbstätigen und BIP pro Kopf.\n",
                     "Niveau: BIP pro Kopf ", max_year, " in % der USA. Jahre: bis 50 % der USA erreicht sind, ",
                     "wenn der Wachstums-\nvorsprung gegenüber den USA (", pct(us$g_gdp), " % p. a.) ",
                     "der letzten zehn Jahre anhält.")
  var_names <- c(g_cap = "Kapital je Erwerbstätigen", g_gdp = "BIP pro Kopf")
  col_head <- c("Niveau", "Jahre")
} else {
  fig_title <- "Who is catching up fastest right now"
  fig_subtitle <- paste0("Countries below ", max_rel * 100, "% of US GDP per capita (", n_cand,
                         "), the 15 with the highest catch-up score.\n",
                         "Avg. annual growth ", min_year, "–", max_year, ".")
  fig_note <- paste0("Catch-up score: mean of the standardised growth rates of capital per worker and GDP per capita.\n",
                     "Level: GDP per capita ", max_year, " in % of the US. Years: until 50% of the US is reached ",
                     "if the growth\nlead over the US (", pct(us$g_gdp), "% p.a.) of the last ten years persists.")
  var_names <- c(g_cap = "Capital per worker", g_gdp = "GDP per capita")
  col_head <- c("Level", "Years")
}

d2 <- top %>%
  mutate(name = factor(ctry_name(iso), levels = rev(ctry_name(iso))))

d2_long <- d2 %>%
  pivot_longer(c(g_cap, g_gdp), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var_names[var], levels = unname(var_names)))

x_max <- max(d2_long$value)
x_lvl <- x_max * 1.18
x_yrs <- x_max * 1.38

d2_txt <- d2 %>%
  mutate(lvl_txt = paste0(formatC(rel * 100, format = "f", digits = 0), " %"),
         yrs_txt = ifelse(is.na(yrs_half), "–",
                          ifelse(yrs_half > 100, ">100", formatC(yrs_half, format = "f", digits = 0))))
if (lang == "en") d2_txt$lvl_txt <- sub(" %", "%", d2_txt$lvl_txt)

g_rank <- ggplot(d2, aes(y = name)) +
  geom_vline(xintercept = 0, colour = design_colours()[["ink"]], linewidth = .5) +
  geom_segment(aes(x = pmin(g_cap, g_gdp), xend = pmax(g_cap, g_gdp), yend = name),
               colour = design_colours()[["grid"]], linewidth = 1.6) +
  geom_point(data = d2_long, aes(x = value, colour = var), size = 3) +
  geom_text(data = d2_txt, aes(x = x_lvl, label = lvl_txt), size = 3, hjust = 1,
            family = font_corporate_design, colour = design_colours()[["ink"]]) +
  geom_text(data = d2_txt, aes(x = x_yrs, label = yrs_txt), size = 3, hjust = 1,
            family = font_corporate_design, colour = design_colours()[["ink"]]) +
  annotate("text", x = c(x_lvl, x_yrs), y = nrow(d2) + .9, label = col_head,
           size = 3, hjust = 1, fontface = "bold", family = font_corporate_design,
           colour = design_colours()[["ink_soft"]]) +
  scale_colour_manual(values = setNames(c(design_colours()[["blue"]], design_colours()[["red"]]),
                                        unname(var_names))) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1, decimal.mark = dec_mark),
                     breaks = seq(0, .12, .02),
                     expand = expansion(mult = c(.02, .01))) +
  scale_y_discrete(expand = expansion(add = c(.6, 1.3))) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_year, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "x") +
  theme(axis.title = element_blank())

g_rank

save_chart(g_rank, "catch-up-ranking", lang = lang, format = "portrait")
