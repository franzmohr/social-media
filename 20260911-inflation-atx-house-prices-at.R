rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(oenb)
library(zoo)

lang <- "de"

base_qtr <- "2010 Q1"

# Wiener Boerse index downloads ---------------------------------------------
# The export mixes "1812,54" and "16.219,48" in the same column, so parse the
# German number format explicitly rather than trusting read.csv2()'s guess.
num_de <- function(x) {
  as.numeric(gsub(",", ".", gsub(".", "", as.character(x), fixed = TRUE), fixed = TRUE))
}

wb_index <- function(slug, name, to = Sys.Date()) {
  url <- paste0("https://www.wienerborse.at/index/", slug,
                "/historische-daten/?c7012%5BDOWNLOAD%5D=csv",
                "&c7012%5BDATETIME_TZ_END_RANGE%5D=", format(to, "%d.%m.%Y"),
                "&c7012%5BDATETIME_TZ_START_RANGE%5D=01.01.2009")
  tmp <- tempfile()
  on.exit(unlink(tmp))
  download.file(url, destfile = tmp, quiet = TRUE)
  read.csv2(tmp, colClasses = "character") %>%
    transmute(date = as.Date(Datum, "%d.%m.%Y"), value = num_de(Schlusspreis)) %>%
    filter(!is.na(date), !is.na(value)) %>%
    arrange(date) %>%
    # The export carries the odd corrupt record (a turnover figure landing in
    # the close column, e.g. 26,348,349 for the ATX on 31.03.2010). An index
    # cannot move by a factor of three in a day, so drop closes that sit far
    # off the local level instead of letting one of them rebase the series.
    mutate(ref = zoo::rollapply(value, 21, stats::median, partial = TRUE,
                                align = "center")) %>%
    filter(value > ref / 3, value < ref * 3) %>%
    mutate(qtr = as.yearqtr(date)) %>%
    filter(qtr != as.yearqtr(max(date))) %>%   # drop the incomplete quarter
    arrange(date) %>%
    group_by(qtr) %>%
    slice(n()) %>%
    ungroup() %>%
    transmute(date = qtr, value, name = name)
}

atx    <- wb_index("atx-AT0000999982", "atx")
atx_tr <- wb_index("atx-tr-AT0000A09FJ6", "atx_tr")

# Consumer prices -----------------------------------------------------------
inflation <- get_eurostat(id = "prc_hicp_midx",
                          filters = list(geo = "AT", unit = "I15", coicop = "CP00"),
                          cache = FALSE) %>%
  mutate(date = as.yearqtr(time)) %>%
  group_by(date) %>%
  filter(n() == 3) %>%                          # complete quarters only
  summarise(value = mean(values), name = "infl", .groups = "drop")

# Residential property prices ----------------------------------------------
hp <- oenb_data(id = "6", pos = "VDBPLIMOPATGEZBN", freq = "Q") %>%
  filter(!is.na(value)) %>%
  transmute(date = as.yearqtr(period, "%Y-Q%q"), value, name = "immo")

if (lang == "de") {
  fig_title <- "Aktien, Wohnungen, Preise seit 2010"
  fig_subtitle <- paste0("Index, ", base_qtr, " = 100. Ein Wert von 200 bedeutet\n",
                         "eine Verdoppelung gegenüber dem Basisquartal.")
  lvls <- c(atx_tr = "ATX Total Return", immo = "Immobilienpreise",
            atx = "ATX (Kursindex)", infl = "Verbraucherpreise")
  src <- "Eurostat, OeNB, Wiener Börse."
  note <- "ATX-Werte sind Quartals-Endkurse. Total Return inklusive Dividenden."
} else {
  fig_title <- "Stocks, homes and prices since 2010"
  fig_subtitle <- paste0("Index, ", base_qtr, " = 100. A value of 200 means\n",
                         "a doubling relative to the base quarter.")
  lvls <- c(atx_tr = "ATX Total Return", immo = "House prices",
            atx = "ATX (price index)", infl = "Consumer prices")
  src <- "Eurostat, OeNB, Vienna Stock Exchange."
  note <- "ATX values are quarter-end closing prices. Total return includes dividends."
}

temp <- bind_rows(inflation, hp, atx, atx_tr) %>%
  filter(date >= base_qtr) %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(idx = value / value[1] * 100) %>%
  ungroup() %>%
  # order the factor by where the lines end, so legend/colour follow the chart
  mutate(name = factor(name, levels = names(lvls), labels = unname(lvls)))

ends <- temp %>%
  group_by(name) %>%
  filter(date == max(date)) %>%
  ungroup()

max_date <- format(max(temp$date), "%YQ%q")

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = idx, colour = name)) +
  geom_hline(yintercept = 100, colour = franz_colours[["ink_soft"]],
             linewidth = .5, linetype = "22") +
  geom_line(linewidth = 1.2) +
  geom_point(data = ends, size = 2) +
  # value plus name at the end of each line: no legend, no colour matching
  ggrepel::geom_text_repel(
    data = ends,
    aes(label = paste0(name, "  ", round(idx))),
    hjust = 0, nudge_x = .8, direction = "y", size = 4,
    fontface = "bold", family = franz_font,
    segment.colour = NA, min.segment.length = Inf, box.padding = .15, seed = 1) +
  scale_x_yearqtr(breaks = seq(as.yearqtr(base_qtr), max(temp$date), by = 3),
                  format = "%Y",
                  expand = expansion(mult = c(.02, .40))) +
  scale_y_continuous(breaks = scales::breaks_width(50)) +
  scale_colour_manual(values = franz_pal("cat", 4), guide = "none") +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = franz_caption(src, last = max_date, note = note, lang = lang)) +
  theme_franz(base_size = 13, grid = "y") +
  theme(axis.title = element_blank())

g

save_post(g, "inflation-atx-house-prices-at", lang = lang, format = "portrait")
