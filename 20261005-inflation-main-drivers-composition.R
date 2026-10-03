
rm(list = ls())


ctry <- "AT"
lang <- "de"


library(lubridate)
min_date <- floor_date(Sys.Date() - months(4), "month")
min_year <- as.numeric(substring(floor_date(min_date, "year"), 1, 4))
min_date <- as.character(min_date)

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

# Mapping of COICOP code and its title
coicop <- read.csv("mapping-coicop-2.csv")

# Load data on all COICOPs and selected countries ----

## Weights ----
weights <- get_eurostat(id = "prc_hicp_iw",
                        filters = list(geo = ctry,
                                       sinceTimePeriod = min_year),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  filter(!is.na(values)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  # Rescale
  mutate(weight = weight / 1000)

## Growth prc_hicp_manr ----
index <- get_eurostat(id = "prc_hicp_minr",
                      filters = list(geo = ctry,
                                     sinceTimePeriod = min_date,
                                     unit = "RCH_A"),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(!is.na(values)) %>%
  select(-freq)

comp <- index %>%
  filter(substring(coicop18, 1, 2) == "CP") %>%
  # Merge with weight data
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop18")) %>%
  # Calculate contribution
  mutate(values = values * weight) %>%
  # # Apply filters based on try and error
  # # If a COICOP with one sublevel is excluded, its sub-sub-levels MUST be included!
  # mutate(cond = case_when(nchar(coicop) == 5 & !coicop %in% c("CP045", "CP094", "CP111") ~ TRUE,
  #                         nchar(coicop) == 6 & substring(coicop, 1, 5) %in% c("CP045", "CP094", "CP111") ~ TRUE,
  #                         TRUE ~ FALSE)) %>%
  #filter(cond) %>%
  # Drop weight information
  select(-weight) %>%
  filter(time == max(time)) %>%
  mutate(lvl = nchar(coicop18)) %>%
  rename(coicop = coicop18,
         value = values) %>%
  left_join(coicop, by = "coicop") %>%
  rename(var = var_de) %>%
  select(time, geo, coicop, lvl, value, var)

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

source("theme_franz.R")

if (lang == "de") {
  months_de <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                 "August", "September", "Oktober", "November", "Dezember")
  used_month <- paste(months_de[month(used_date)], year(used_date))
  fig_title <- "Was die Inflation in Österreich treibt"
  fig_subtitle <- paste0("Beiträge zur Inflationsrate im ", used_month,
                         ", in Prozentpunkten\n",
                         "Punkte: Beitrag der gesamten Hauptgruppe")
  fig_caption <- franz_caption("Eurostat (HVPI).", lang = lang)
  dec_mark <- ","
}

# Long COICOP titles: wrap them so neither axis nor legend squeezes the panel
levels(temp_line$var_x) <- wrap_labels(levels(temp_line$var_x), width = 40)
levels(temp_col$var_x) <- wrap_labels(levels(temp_col$var_x), width = 40)
levels(temp_col$var_fill) <- wrap_labels(levels(temp_col$var_fill), width = 36)
fill_highlight <- head(levels(temp_col$var_fill), -1)

g <- ggplot(temp_line, aes(x = value, y = var_x)) +
  geom_vline(xintercept = 0, colour = franz_colours[["ink"]], linewidth = .5) +
  # reverse = TRUE puts the named drivers next to the zero line, "Andere" outside
  geom_col(data = temp_col, aes(fill = var_fill), width = .72,
           position = position_stack(reverse = TRUE)) +
  geom_point(shape = 21, size = 2.6, stroke = 1,
             fill = franz_colours[["paper"]], colour = franz_colours[["ink"]]) +
  scale_fill_highlight(highlight = fill_highlight, rest = "Andere") +
  scale_x_continuous(labels = scales::label_number(decimal.mark = dec_mark),
                     expand = expansion(mult = c(.02, .04))) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_franz(grid = "x") +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(colour = franz_colours[["ink"]], hjust = 1,
                                   lineheight = .95),
        legend.text = element_text(lineheight = .95),
        legend.key.spacing.y = unit(5, "pt"),
        legend.margin = margin(b = 14))

g

save_post(g, "inflation-main-drivers-composition", lang = lang, format = "portrait")

