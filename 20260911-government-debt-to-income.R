rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)

lang <- "de"

# Highlighted countries. Everything else is drawn as grey context.
hl <- c("EL", "IE", "ES", "EA19", "AT", "DE")

min_date <- "2000-01-01"

# Gross government debt -----------------------------------------------------
debt <- get_eurostat(id = "gov_10q_ggdebt",
                     filters = list(sector = "S13",
                                    unit = "MIO_EUR",
                                    na_item = "GD"),
                     cache = FALSE) %>%
  filter(substring(time, 6, 7) == "10") %>%
  mutate(time = substring(time, 1, 4)) %>%
  select(time, geo, values) %>%
  mutate(var = "debt")

# Total government revenue --------------------------------------------------
inc <- get_eurostat(id = "gov_10a_main",
                    filters = list(sector = "S13",
                                   unit = "MIO_EUR",
                                   na_item = "TR"),
                    cache = FALSE) %>%
  mutate(time = substring(time, 1, 4)) %>%
  select(time, geo, values) %>%
  mutate(var = "inc")

temp <- bind_rows(debt, inc) %>%
  na.omit() %>%
  pivot_wider(names_from = "var", values_from = "values") %>%
  mutate(value = debt / inc,
         date = as.Date(paste0(time, "-01-01"))) %>%
  na.omit() %>%
  filter(date >= min_date)

if (lang == "de") {
  fig_title <- "Wie viele Jahreseinnahmen schuldet der Staat?"
  fig_subtitle <- paste0("Staatsschulden in Prozent der jährlichen Staatseinnahmen.\n",
                         "Jede graue Linie ist ein weiteres EU-Land.")
  ctry <- c(AT = "Österreich", DE = "Deutschland", EA19 = "Euroraum",
            EL = "Griechenland", IE = "Irland", ES = "Spanien")
  src <- "Eurostat."
} else {
  fig_title <- "How many years of revenue does the state owe?"
  fig_subtitle <- paste0("Government debt as a percent of annual government revenue.\n",
                         "Each grey line is another EU country.")
  ctry <- c(AT = "Austria", DE = "Germany", EA19 = "Euro area",
            EL = "Greece", IE = "Ireland", ES = "Spain")
  src <- "Eurostat."
}

max_date <- max(temp$time)

# Split into context and story so each gets its own layer -------------------
back <- filter(temp, !geo %in% hl)
front <- temp %>%
  filter(geo %in% hl) %>%
  mutate(name = factor(ctry[geo], levels = unname(ctry[hl])))

ends <- front %>%
  group_by(name) %>%
  filter(date == max(date)) %>%
  ungroup()

source("r-corporate-design-functions-ggplot2.R")

g <- ggplot(mapping = aes(x = date, y = value)) +
  # context: every other EU country, readable as a cloud but never as a series
  geom_line(data = back, aes(group = geo),
            colour = design_colours()[["mute"]], linewidth = .5) +
  # story: the six series that carry the point
  geom_line(data = front, aes(colour = name), linewidth = 1.1) +
  # direct labels replace the legend
  geom_point(data = ends, aes(colour = name), size = 1.8) +
  ggrepel::geom_text_repel(data = ends, aes(colour = name, label = name),
                           hjust = 0, nudge_x = 200, direction = "y",
                           size = 4.1, fontface = "bold", family = font_corporate_design,
                           segment.colour = NA, min.segment.length = Inf,
                           box.padding = .1, seed = 1) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), as.Date(paste0(max_date, "-01-01")),
                            by = "5 years"),
               date_labels = "%Y",
               expand = expansion(mult = c(.02, .26))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(0, 4, 1)) +
  scale_colour_manual(values = palette_corporate_design("cat", length(hl)), guide = "none") +
  coord_cartesian(clip = "off") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = max_date, lang = lang)) +
  theme_corporate_design(base_size = 13, grid = "y") +
  theme(axis.title = element_blank())

g

save_chart(g, "government-debt-to-income", lang = lang, format = "portrait")
