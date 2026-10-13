
rm(list = ls())


ctry <- "AT"
lang <- "de"
options(corporate_design.mode = "dark")  # "light" for the standard look


library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

# Mapping of COICOP code and its title
coicop <- read.csv("mapping-coicop-2.csv")

# Load VPI data from Statistik Austria open data ----
# VPI 2025 by COICOP 2018 down to 5 digits. Besides the indices it carries the
# official contributions to the annual rate ("Einfluss Vorjahresmonat"), which
# add up to the headline rate, so no weights have to be applied here.
ogd_id <- "OGD_vpi25c18_VPI_2025COICOP18_1"
vpi <- read.csv2(paste0("https://data.statistik.gv.at/data/", ogd_id, ".csv"),
                 colClasses = "character")

comp <- vpi %>%
  select(time = C.VPIZR.0, coicop = C.VPICOICOP18_5.0, value = F.VPIEFVJM) %>%
  mutate(coicop = sub("VPICOICOP18-", "", coicop)) %>%
  # Monthly values only, COICOP codes only (drops annual averages, the total
  # and special aggregates such as "SKERN")
  filter(grepl("^VPIZR-[0-9]{6}$", time),
         grepl("^[0-9]{2,5}$", coicop)) %>%
  mutate(time = as.Date(paste0(substring(time, 7, 10), "-",
                               substring(time, 11, 12), "-01")),
         value = as.numeric(sub(",", ".", value)),
         geo = ctry,
         coicop = paste0("CP", coicop)) %>%
  filter(time == max(time)) %>%
  mutate(lvl = nchar(coicop)) %>%
  left_join(coicop, by = "coicop") %>%
  rename(var = var_de) %>%
  select(time, geo, coicop, lvl, value, var)

## HVPI contributions of the main groups ----
# Neither Statistik Austria nor Eurostat publish HVPI contributions for
# Austria, so they are computed from Eurostat's indices and item weights with
# the formula for chain-linked indices (Ribe). Each year's weights apply to
# price relatives against the previous December, which splits the annual rate
# into the part up to December of last year and the part since then.
library(eurostat)
hvpi_groups <- sprintf("CP%02d", 1:13)
hvpi_date <- max(comp$time)
hvpi_year <- year(hvpi_date)
hvpi_dates <- c(hvpi_date, hvpi_date - years(1),
                make_date(hvpi_year - 1, 12), make_date(hvpi_year - 2, 12))

hvpi_index <- get_eurostat(id = "prc_hicp_minr",
                           filters = list(geo = ctry, unit = "I25",
                                          coicop18 = hvpi_groups,
                                          sinceTimePeriod = format(min(hvpi_dates), "%Y-%m")),
                           cache = FALSE) %>%
  mutate(time = as.Date(time)) %>%
  filter(time %in% hvpi_dates) %>%
  mutate(period = case_when(time == hvpi_dates[1] ~ "now",
                            time == hvpi_dates[2] ~ "last",
                            time == hvpi_dates[3] ~ "dec_last",
                            time == hvpi_dates[4] ~ "dec_prev")) %>%
  select(coicop = coicop18, period, values) %>%
  pivot_wider(names_from = period, values_from = values)

hvpi_weights <- get_eurostat(id = "prc_hicp_iw",
                             filters = list(geo = ctry, coicop18 = hvpi_groups,
                                            sinceTimePeriod = hvpi_year - 1),
                             cache = FALSE) %>%
  mutate(year = year(as.Date(time))) %>%
  group_by(year) %>%
  mutate(values = values / sum(values)) %>%
  ungroup() %>%
  mutate(year = ifelse(year == hvpi_year, "w_now", "w_last")) %>%
  select(coicop = coicop18, year, values) %>%
  pivot_wider(names_from = year, values_from = values)

hvpi <- hvpi_index %>%
  left_join(hvpi_weights, by = "coicop") %>%
  mutate(rel_dec = dec_last / dec_prev,   # December to December, last year
         rel_last = last / dec_prev,      # same month last year
         rel_now = now / dec_last)        # since December
agg_dec <- sum(hvpi$w_last * hvpi$rel_dec)
agg_last <- sum(hvpi$w_last * hvpi$rel_last)
hvpi <- hvpi %>%
  mutate(value = 100 * (w_last * (rel_dec - rel_last) +
                          agg_dec * w_now * (rel_now - 1)) / agg_last) %>%
  select(coicop, value)

# Check against the published HVPI rate (Statistik Austria open data)
hvpi_total <- read.csv2("https://data.statistik.gv.at/data/OGD_hvpi25_HVD_HVPI_2025_1.csv",
                        colClasses = "character") %>%
  filter(C.HVPICOICOP18_5.0 == "HVPI-0",
         C.VPIZR.0 %in% paste0("VPIZR-", format(hvpi_dates[1:2], "%Y%m"))) %>%
  arrange(C.VPIZR.0) %>%
  pull(F.VPIMZBM) %>%
  sub(",", ".", x = .) %>%
  as.numeric()
hvpi_rate <- round(100 * (hvpi_total[2] / hvpi_total[1] - 1), 1)
if (abs(sum(hvpi$value) - hvpi_rate) > .1) {
  warning("HVPI contributions add up to ", round(sum(hvpi$value), 2),
          ", published rate is ", hvpi_rate)
}

# Points
temp_line <- comp %>%
  filter(lvl == 4) %>%
  mutate(var_x = coicop) %>%
  select(var_x, value)

order_x <- temp_line %>%
  arrange(value) %>%
  left_join(coicop, by = c("var_x" = "coicop")) %>%
  select(var_x_levels = var_x, var_x_labels = var_de)

var_x_levels <- pull(order_x, "var_x_levels")
var_x_labels <- pull(order_x, "var_x_labels")

temp_line <- temp_line %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels))

temp_hvpi <- hvpi %>%
  mutate(var_x = factor(coicop, levels = var_x_levels, labels = var_x_labels)) %>%
  select(var_x, value)

# Bar plots
temp_col <- comp %>%
  filter(lvl == 5) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)

# Differentiate "CP072"
diff_var <- "CP072"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)


# Differentiate "CP0722"
diff_var <- "CP0722"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)

# Differentiate "CP111"
diff_var <- "CP111"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)

# Differentiate "CP1111"
diff_var <- "CP1111"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)

# Differentiate "CP044"
diff_var <- "CP044"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)

# Differentiate "CP094"
diff_var <- "CP094"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)

# Differentiate "CP0946"
diff_var <- "CP0946"
var_length <- nchar(diff_var)
temp_col <- temp_col %>% filter(var_fill != diff_var)
temp_sub <- comp %>%
  filter(lvl == (var_length + 1) & substring(coicop, 1, var_length) == diff_var) %>%
  mutate(var_x = substring(coicop, 1, 4),
         var_fill = coicop) %>%
  select(var_x, var_fill, value)
temp_col <- bind_rows(temp_col, temp_sub)
rm(temp_sub)



top_var_fill <- temp_col %>%
  mutate(value = abs(value)) %>%
  arrange(desc(value)) %>%
  slice(1:7) %>%
  select(var_fill) %>%
  left_join(coicop, by = c("var_fill" = "coicop")) %>%
  select(var_fill_levels = var_fill, var_fill_labels = var_de)

var_fill_levels <- pull(top_var_fill, "var_fill_levels")
var_fill_labels <- pull(top_var_fill, "var_fill_labels")

temp_col <- temp_col %>%
  mutate(var_x = factor(var_x, levels = var_x_levels, labels = var_x_labels),
         var_fill = ifelse(var_fill %in% var_fill_levels, var_fill, "Andere"),
         var_fill = factor(var_fill, levels = c(var_fill_levels, "Andere"),
                           labels = c(var_fill_labels, "Andere")))

used_date <- as.Date(unique(comp$time))

source("r-corporate-design-functions-ggplot2.R")

if (lang == "de") {
  months_de <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                 "August", "September", "Oktober", "November", "Dezember")
  used_month <- paste(months_de[month(used_date)], year(used_date))
  fig_title <- "Was die Inflation in Österreich treibt"
  fig_subtitle <- paste0("Beiträge zur VPI-Inflationsrate im ", used_month,
                         ", in Prozentpunkten\n",
                         "Hauptgruppe gesamt: Kreise VPI, Rauten HVPI")
  fig_caption <- caption_corporate_design("Statistik Austria (VPI 2025, Open Data), Eurostat (HVPI).",
                                          lang = lang)
  dec_mark <- ","
}

# Long COICOP titles: wrap them so neither axis nor legend squeezes the panel
levels(temp_line$var_x) <- wrap_labels(levels(temp_line$var_x), width = 40)
levels(temp_hvpi$var_x) <- wrap_labels(levels(temp_hvpi$var_x), width = 40)
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
  geom_point(data = temp_hvpi, shape = 23, size = 2.6, stroke = .8,
             fill = design_colours()[["ink"]], colour = design_colours()[["paper"]]) +
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

save_chart(g, "inflation-main-drivers-composition-vpi", lang = lang, format = "portrait")

