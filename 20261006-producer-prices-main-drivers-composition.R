
rm(list = ls())


ctry <- "AT"
lang <- "de"
options(corporate_design.mode = "dark")  # "light" for the standard look

# Eurostat has no wholesale price index; domestic producer prices in industry
# are the closest harmonised series.
min_date <- "2021-01-01"   # base year of the index, start of the weight estimation

library(dplyr)
library(eurostat)
library(ggplot2)
library(lubridate)
library(nnls)
library(tidyr)

n_top <- 7   # divisions shown in colour; the remainder collapses into "Andere"

# Producer prices on the domestic market, 2021 = 100 ------------------------
raw <- get_eurostat(id = "sts_inppd_m",
                    filters = list(geo = ctry,
                                   unit = "I21",
                                   s_adj = "NSA",
                                   sinceTimePeriod = substring(min_date, 1, 7)),
                    cache = FALSE) %>%
  mutate(time = as.Date(paste0(substring(as.character(time), 1, 7), "-01"))) %>%
  filter(!is.na(values)) %>%
  rename(code = nace_r2, index = values)

if (length(unique(raw$indic_bt)) != 1) stop("more than one indicator in sts_inppd_m")

total_code <- "B-E36"
section_codes <- c("B", "C", "D", "E36")

# Divisions (B05, C10, ...) and the section each belongs to
division <- raw %>%
  distinct(code) %>%
  filter(grepl("^[B-E][0-9]{2}$", code)) %>%
  mutate(section = ifelse(code == "E36", "E36", substring(code, 1, 1))) %>%
  filter(section %in% section_codes)

idx <- raw %>%
  select(time, code, index) %>%
  pivot_wider(names_from = code, values_from = index) %>%
  arrange(time)

# Weights --------------------------------------------------------------------
# Eurostat publishes no weights. The index is a fixed-base Laspeyres index, so
# a parent index is the weighted sum of its components' indices; regressing
# the parent on its components (non-negative, no intercept) recovers the
# weights. Divisions suppressed for confidentiality simply drop out.
estimate_weights <- function(parent, children) {
  children <- intersect(children, names(idx))
  if (length(children) == 1) return(setNames(1, children))
  # Only components with a complete series can enter the regression
  children <- children[colSums(!is.na(idx[, children])) == nrow(idx)]
  d <- idx[, c(parent, children)] %>% na.omit()
  fit <- nnls(as.matrix(d[, children]), d[[parent]])
  r2 <- 1 - sum(fit$residuals^2) / sum((d[[parent]] - mean(d[[parent]]))^2)
  message(parent, ": weights fitted on ", nrow(d), " months, R2 = ", round(r2, 4))
  setNames(fit$x, children)
}

w_section <- estimate_weights(total_code, section_codes)

weights <- bind_rows(lapply(section_codes, function(s) {
  w <- estimate_weights(s, division$code[division$section == s])
  tibble(code = names(w), section = s, weight = w * w_section[[s]])
})) %>%
  filter(weight > 0)

# Contributions to the annual rate -----------------------------------------
# Contribution of division i in percentage points:
# w_i * (I_i,t - I_i,t-12) / I_total,t-12 * 100
used_date <- max(idx$time[!is.na(idx[[total_code]])])
prev_date <- used_date - months(12)

now <- idx %>% filter(time == used_date)
prev <- idx %>% filter(time == prev_date)

comp <- weights %>%
  mutate(value = weight * (unlist(now[code]) - unlist(prev[code])) /
           prev[[total_code]] * 100) %>%
  filter(!is.na(value))

total_rate <- (now[[total_code]] / prev[[total_code]] - 1) * 100
message("Annual rate: ", round(total_rate, 2), " %, sum of contributions: ",
        round(sum(comp$value), 2), " pp")

# Labels ---------------------------------------------------------------------
label_de <- function(x) {
  lab <- label_eurostat(x, dic = "nace_r2", lang = lang)
  lab <- sub("^Herstellung von ", "", lab)
  paste0(toupper(substring(lab, 1, 1)), substring(lab, 2))
}

# Points: contribution of the sections
temp_line <- comp %>%
  group_by(var_x = section) %>%
  summarise(value = sum(value), .groups = "drop")

order_x <- temp_line %>%
  arrange(value)

var_x_levels <- pull(order_x, "var_x")
var_x_labels <- label_de(var_x_levels)

temp_line <- temp_line %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels))

# Bars: divisions, stacked within their section
temp_col <- comp %>%
  select(var_x = section, var_fill = code, value)

var_fill_levels <- temp_col %>%
  arrange(desc(abs(value))) %>%
  slice(seq_len(n_top)) %>%
  pull("var_fill")
var_fill_labels <- label_de(var_fill_levels)

temp_col <- temp_col %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels),
         var_fill = ifelse(var_fill %in% var_fill_levels, var_fill, "Andere"),
         var_fill = factor(var_fill, levels = c(var_fill_levels, "Andere"),
                           labels = c(var_fill_labels, "Andere")))

source("r-corporate-design-functions-ggplot2.R")

if (lang == "de") {
  months_de <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                 "August", "September", "Oktober", "November", "Dezember")
  used_month <- paste(months_de[month(used_date)], year(used_date))
  dec_mark <- ","
  fig_title <- "Was die Erzeugerpreise in Österreich treibt"
  fig_subtitle <- paste0("Beiträge zur Veränderung der Erzeugerpreise im Inlandsabsatz gegenüber\n",
                         "dem Vorjahr im ", used_month, " (insgesamt ",
                         format(round(total_rate, 1), nsmall = 1, decimal.mark = dec_mark),
                         " %), in Prozentpunkten\n",
                         "Punkte: Beitrag des gesamten Wirtschaftsabschnitts")
  fig_caption <- caption_corporate_design(
    "Eurostat (sts_inppd_m).", lang = lang,
    note = "Gewichte aus den Indexreihen geschätzt.")
}

# Long NACE titles: wrap them so neither axis nor legend squeezes the panel
levels(temp_line$var_x) <- wrap_labels(levels(temp_line$var_x), width = 30)
levels(temp_col$var_x) <- wrap_labels(levels(temp_col$var_x), width = 30)
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

save_chart(g, "producer-prices-main-drivers-composition", lang = lang, format = "portrait")
