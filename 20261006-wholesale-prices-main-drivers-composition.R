
rm(list = ls())


lang <- "de"
options(corporate_design.mode = "dark")  # "light" for the standard look

library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(tidyr)

# Index of wholesale prices (GHPI 2020 = 100), Statistik Austria -------------
# STATcube export in long format, one row per product group and month:
#   date   - first day of the month (or "2025-09" style month)
#   code   - hierarchical product group code; a group's subgroups extend its
#            code (e.g. "47" -> "471" -> "4711"), the total is "GHPI"
#   name   - German title of the product group
#   weight - weight in the GHPI 2020 basket, in per mille of the total
#   index  - index value, 2020 = 100
ghpi_file <- "D:/Data/20261006-statistik-austria-ghpi.xlsx"
total_code <- "GHPI"

# Groups split into their own subgroups, applied in this order. Use it where a
# single subgroup hides very different price developments (like fuels).
split_codes <- c()

n_top <- 7   # subgroups shown in colour; the remainder collapses into "Andere"

raw <- read_excel(ghpi_file) %>%
  mutate(date = floor_date(as.Date(paste0(substring(as.character(date), 1, 7), "-01")), "month"),
         code = as.character(code)) %>%
  filter(!is.na(index))

# Parent of a code: the longest other code that is a prefix of it
codes <- setdiff(unique(raw$code), total_code)
parent_of <- function(x) {
  cand <- codes[nchar(codes) < nchar(x) & startsWith(x, codes)]
  if (length(cand) == 0) total_code else cand[which.max(nchar(cand))]
}
hierarchy <- tibble(code = codes) %>%
  mutate(parent = vapply(code, parent_of, character(1)))

labels <- raw %>%
  distinct(code, name)

# Contributions to the annual rate -----------------------------------------
# The GHPI is a fixed-base Laspeyres index, so a group's contribution in
# percentage points is w_i * (I_i,t - I_i,t-12) / I_total,t-12 * 100, with w_i
# the group's share in the total basket.
used_date <- max(raw$date)
prev_date <- used_date - months(12)

total_prev <- raw %>%
  filter(code == total_code, date == prev_date) %>%
  pull("index")

main_weight <- raw %>%
  filter(date == used_date, code %in% hierarchy$code[hierarchy$parent == total_code]) %>%
  pull("weight") %>%
  sum()

comp <- raw %>%
  filter(code != total_code, date %in% c(used_date, prev_date)) %>%
  select(date, code, weight, index) %>%
  pivot_wider(names_from = date, values_from = index) %>%
  rename(index_now = !!as.character(used_date), index_prev = !!as.character(prev_date)) %>%
  mutate(value = weight / main_weight * (index_now - index_prev) / total_prev * 100) %>%
  left_join(hierarchy, by = "code") %>%
  filter(!is.na(value))

# Points: contribution of the main groups
temp_line <- comp %>%
  filter(parent == total_code) %>%
  select(var_x = code, value)

order_x <- temp_line %>%
  arrange(value) %>%
  left_join(labels, by = c("var_x" = "code"))

var_x_levels <- pull(order_x, "var_x")
var_x_labels <- pull(order_x, "name")

temp_line <- temp_line %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels))

# Bars: subgroups of each main group, stacked
main_of <- function(x) {
  while (TRUE) {
    p <- hierarchy$parent[hierarchy$code == x]
    if (p == total_code) return(x)
    x <- p
  }
}

bar_codes <- comp %>%
  filter(parent %in% var_x_levels) %>%
  pull("code")
# A main group without subgroups stands for itself
bar_codes <- c(bar_codes, setdiff(var_x_levels, comp$parent))

for (diff_var in split_codes) {
  sub_codes <- comp$code[comp$parent == diff_var]
  if (diff_var %in% bar_codes && length(sub_codes) > 0) {
    bar_codes <- c(setdiff(bar_codes, diff_var), sub_codes)
  }
}

temp_col <- comp %>%
  filter(code %in% bar_codes) %>%
  mutate(var_x = vapply(code, main_of, character(1))) %>%
  select(var_x, var_fill = code, value)

top_var_fill <- temp_col %>%
  arrange(desc(abs(value))) %>%
  slice(seq_len(n_top)) %>%
  left_join(labels, by = c("var_fill" = "code"))

var_fill_levels <- pull(top_var_fill, "var_fill")
var_fill_labels <- pull(top_var_fill, "name")

temp_col <- temp_col %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels),
         var_fill = ifelse(var_fill %in% var_fill_levels, var_fill, "Andere"),
         var_fill = factor(var_fill, levels = c(var_fill_levels, "Andere"),
                           labels = c(var_fill_labels, "Andere")))

total_rate <- raw %>%
  filter(code == total_code, date %in% c(used_date, prev_date)) %>%
  arrange(date) %>%
  pull("index")
total_rate <- (total_rate[2] / total_rate[1] - 1) * 100

source("r-corporate-design-functions-ggplot2.R")

if (lang == "de") {
  months_de <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                 "August", "September", "Oktober", "November", "Dezember")
  used_month <- paste(months_de[month(used_date)], year(used_date))
  dec_mark <- ","
  fig_title <- "Was die Großhandelspreise in Österreich treibt"
  fig_subtitle <- paste0("Beiträge zur Veränderung der Großhandelspreise gegenüber dem Vorjahr\n",
                         "im ", used_month, " (insgesamt ",
                         format(round(total_rate, 1), nsmall = 1, decimal.mark = dec_mark),
                         " %), in Prozentpunkten\n",
                         "Punkte: Beitrag der gesamten Warengruppe")
  fig_caption <- caption_corporate_design("Statistik Austria (Großhandelspreisindex 2020).",
                                          lang = lang)
}

# Long product group titles: wrap them so neither axis nor legend squeezes the panel
levels(temp_line$var_x) <- wrap_labels(levels(temp_line$var_x), width = 40)
levels(temp_col$var_x) <- wrap_labels(levels(temp_col$var_x), width = 40)
levels(temp_col$var_fill) <- wrap_labels(levels(temp_col$var_fill), width = 36)
fill_highlight <- head(levels(temp_col$var_fill), -1)

g <- ggplot(temp_line, aes(x = value, y = var_x)) +
  geom_zeroline(x = 0) +
  # reverse = TRUE puts the named drivers next to the zero line, "Andere" outside
  geom_col(data = temp_col, aes(fill = var_fill), width = .72,
           position = position_stack(reverse = TRUE)) +
  geom_point(shape = 21, size = 2.6, stroke = 1,
             fill = design_colours()[["paper"]], colour = design_colours()[["ink"]]) +
  scale_fill_highlight(highlight = fill_highlight, rest = "Andere") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = dec_mark),
                     expand = expansion(mult = c(.02, .04))) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_corporate_design(grid = "x") +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(colour = design_colours()[["ink"]], hjust = 1,
                                   lineheight = .95),
        legend.text = element_text(lineheight = .95),
        legend.key.spacing.y = unit(5, "pt"),
        legend.margin = margin(b = 14))

g

save_chart(g, "wholesale-prices-main-drivers-composition", lang = lang, format = "portrait")
