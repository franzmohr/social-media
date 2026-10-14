
rm(list = ls())


lang <- "de"
options(corporate_design.mode = "dark")  # "light" for the standard look

min_date <- as.Date("2024-01-01")
n_top <- 7     # number of named drivers, the rest is shown as "Andere"
n_digits <- 3  # COICOP level of the drivers, e.g. 045 "Elektrizität, Gas ..."


library(lubridate)
library(dplyr)
library(ggplot2)

# Mapping of COICOP code and its title
coicop <- read.csv("mapping-coicop-2.csv")

# Load VPI data from Statistik Austria open data ----
# Both datasets are by COICOP 2018 down to 5 digits and carry the official
# contributions to the annual rate ("Einfluss Vorjahresmonat"). Up to 2025 the
# VPI 2020 is used, from 2026 the VPI 2025, which is the official index since
# then. The groups of each COICOP level add up to the published rate (up to
# rounding).
read_vpi <- function(ogd_id) {
  read.csv2(paste0("https://data.statistik.gv.at/data/", ogd_id, ".csv"),
            colClasses = "character") %>%
    select(time = C.VPIZR.0, coicop = C.VPICOICOP18_5.0,
           rate = F.VPIPZVJM, value = F.VPIEFVJM) %>%
    # Monthly values only (drops annual averages)
    filter(grepl("^VPIZR-[0-9]{6}$", time)) %>%
    mutate(time = as.Date(paste0(substring(time, 7, 10), "-",
                                 substring(time, 11, 12), "-01")),
           coicop = sub("VPICOICOP18-", "", coicop),
           rate = as.numeric(sub(",", ".", rate)),
           value = as.numeric(sub(",", ".", value)))
}

vpi <- bind_rows(
  read_vpi("OGD_vpi20c18_VPI_2020COICOP18_1") %>% filter(year(time) <= 2025),
  read_vpi("OGD_vpi25c18_VPI_2025COICOP18_1") %>% filter(year(time) >= 2026)
) %>%
  filter(time >= min_date)

# Published inflation rate
total <- vpi %>%
  filter(coicop == "0") %>%
  select(time, value = rate)

# Contributions of the groups at the chosen COICOP level
comp <- vpi %>%
  filter(grepl(paste0("^[0-9]{", n_digits, "}$"), coicop)) %>%
  mutate(coicop = paste0("CP", coicop)) %>%
  select(time, coicop, value)

# Largest drivers: highest average absolute contribution over the period
top_var_fill <- comp %>%
  group_by(coicop) %>%
  summarise(size = mean(abs(value)), .groups = "drop") %>%
  arrange(desc(size)) %>%
  slice(1:n_top) %>%
  left_join(coicop, by = "coicop") %>%
  select(var_fill_levels = coicop, var_fill_labels = var_de)

var_fill_levels <- pull(top_var_fill, "var_fill_levels")
var_fill_labels <- pull(top_var_fill, "var_fill_labels")

temp_col <- comp %>%
  mutate(var_fill = ifelse(coicop %in% var_fill_levels, coicop, "Andere")) %>%
  group_by(time, var_fill) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(var_fill = factor(var_fill, levels = c(var_fill_levels, "Andere"),
                           labels = c(var_fill_labels, "Andere")))

source("r-corporate-design-functions-ggplot2.R")

if (lang == "de") {
  months_de <- c("Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli",
                 "August", "September", "Oktober", "November", "Dezember")
  max_date <- max(total$time)
  used_month <- paste(months_de[month(max_date)], year(max_date))
  fig_title <- "Was die Inflation in Österreich treibt"
  fig_subtitle <- paste0("Beiträge zur VPI-Inflationsrate, in Prozentpunkten\n",
                         "Linie: Inflationsrate in %")
  fig_caption <- caption_corporate_design("Statistik Austria (VPI 2020 und VPI 2025, Open Data).",
                                          last = used_month, lang = lang, own = FALSE)
  label_total <- "Inflation"
  dec_mark <- ","
}

levels(temp_col$var_fill) <- wrap_labels(levels(temp_col$var_fill), width = 36)
fill_highlight <- head(levels(temp_col$var_fill), -1)

last_total <- filter(total, time == max(time))

g <- ggplot(temp_col, aes(x = time, y = value)) +
  # reverse = TRUE puts the named drivers next to the zero line, "Andere" outside
  geom_col(aes(fill = var_fill), width = 25,
           position = position_stack(reverse = TRUE)) +
  geom_zeroline() +
  geom_line(data = total, colour = design_colours()[["ink"]], linewidth = .8) +
  geom_point(data = last_total, colour = design_colours()[["ink"]], size = 2) +
  geom_text(data = last_total,
            aes(label = paste0(label_total, ": ",
                               format(value, decimal.mark = dec_mark), " %")),
            hjust = 0, nudge_x = 25, size = 3.6, fontface = "bold",
            colour = design_colours()[["ink"]], family = font_corporate_design) +
  scale_x_date(breaks = seq(min_date, max_date, by = "6 months"),
               labels = function(x) paste0(substring(months_de[month(x)], 1, 3), " ",
                                           format(x, "%y")),
               expand = expansion(mult = c(.02, .2))) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = dec_mark),
                     breaks = scales::breaks_width(1)) +
  scale_fill_highlight(highlight = fill_highlight, rest = "Andere") +
  guides(fill = guide_legend(ncol = 2)) +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  theme_corporate_design(grid = "y") +
  theme(axis.title = element_blank(),
        legend.text = element_text(lineheight = .95),
        legend.key.spacing.y = unit(5, "pt"),
        legend.margin = margin(b = 14))

g

save_chart(g, "inflation-main-drivers-vpi-over-time", lang = lang, format = "portrait")

# Annual rates of the highlighted groups ----
# Small multiples on a shared y axis: the overall rate first, then each
# driver in its colour from the chart above, with the overall rate in grey
# for reference.

if (lang == "de") {
  label_all <- "Inflation insgesamt"
  fig_subtitle_rate <- paste0("Veränderung gegenüber dem Vorjahresmonat, in %\n",
                              "Graue Linie: Inflation insgesamt")
}

panel_levels <- c(label_all, fill_highlight)

temp_rate <- vpi %>%
  filter(paste0("CP", coicop) %in% var_fill_levels) %>%
  mutate(var_panel = factor(paste0("CP", coicop), levels = var_fill_levels,
                            labels = fill_highlight)) %>%
  select(time, var_panel, value = rate) %>%
  bind_rows(mutate(total, var_panel = label_all)) %>%
  mutate(var_panel = factor(var_panel, levels = panel_levels))

# Overall rate without a panel variable, so it is drawn in every panel
temp_ref <- total

last_rate <- label_last(temp_rate, time, var_panel)

g_rate <- ggplot(temp_rate, aes(x = time, y = value)) +
  geom_zeroline() +
  geom_line(data = temp_ref, colour = design_colours()[["rest"]], linewidth = .6) +
  geom_line(aes(colour = var_panel), linewidth = .9) +
  geom_point(data = last_rate, aes(colour = var_panel), size = 1.8) +
  # Label on a canvas-coloured patch so values near 0 stay legible on the zero line
  geom_label(data = last_rate,
             aes(label = format(value, decimal.mark = dec_mark, nsmall = 1),
                 colour = var_panel),
             hjust = 0, nudge_x = 60, size = 3.4, fontface = "bold",
             family = font_corporate_design, fill = design_colours()[["paper"]],
             border.colour = NA, label.padding = unit(1.5, "pt")) +
  facet_wrap(~ var_panel, ncol = 2) +
  scale_colour_highlight(highlight = fill_highlight, rest = label_all,
                         rest_colour = design_colours()[["ink"]]) +
  scale_x_date(breaks = seq(min_date, max_date, by = "1 year"),
               labels = function(x) format(x, "%Y"),
               expand = expansion(mult = c(.02, .14))) +
  scale_y_continuous(labels = scales::label_number(decimal.mark = dec_mark),
                     breaks = scales::breaks_width(10)) +
  coord_cartesian(clip = "off") +
  labs(title = "Wie stark die Preise der Treiber steigen",
       subtitle = fig_subtitle_rate,
       caption = fig_caption) +
  theme_corporate_design(grid = "y") +
  theme(axis.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(hjust = 0, lineheight = .95),
        panel.spacing.x = unit(18, "pt"),
        panel.spacing.y = unit(10, "pt"))

g_rate

save_chart(g_rate, "inflation-main-drivers-vpi-rates", lang = lang, format = "portrait")
