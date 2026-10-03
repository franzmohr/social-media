rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

lang <- "de"

hl <- c("AT", "DE", "EA19", "EL", "IE", "ES")

min_date <- "2000-01-01"

# Gross debt
debt <- get_eurostat(id = "gov_10q_ggdebt",
                          filters = list(sector = "S13",
                                         unit = "MIO_EUR",
                                         na_item = "GD"),
                          cache = FALSE) %>%
  filter(substring(time, 6, 7) == "10") %>%
  mutate(time = substring(time, 1, 4)) %>%
  arrange(time) %>%
  select(time, geo, values) %>%
  mutate(var = "debt")


inc <- get_eurostat(id = "gov_10a_main",
                     filters = list(sector = "S13",
                                    unit = "MIO_EUR",
                                    na_item = "TR"),
                     cache = FALSE) %>%
  mutate(time = substring(time, 1, 4)) %>%
  arrange(time) %>%
  select(time, geo, values) %>%
  mutate(var = "inc")

temp <- bind_rows(debt, inc) %>%
  na.omit() %>%
  pivot_wider(names_from = "var", values_from = "values") %>%
  mutate(value = debt / inc)

if (lang == "de") {
  fig_title <- "Öffentliche Verschuldung und Einnahmen des Staates"
  fig_subtitle <- "Prozent"
  fig_caption <- "Quelle: Eurostat. Eigene Berechnungen. Letzter Wert: "
}

max_date <- max(temp$time)
fig_caption <- paste0(fig_caption, max_date, ".")

temp <- temp %>%
  mutate(date = as.Date(paste0(time, "-01-01")),
         var_group = geo,
         var_colour = ifelse(geo %in% hl, geo, "Other"),
         var_alpha = ifelse(geo %in%hl, 1, .2)) %>%
  na.omit() %>%
  filter(date >= min_date)

source("r-corporate-design-functions-ggplot2.R")


g <- ggplot(temp, aes(x = date, y = value, colour = var_colour,
                 group = var_group, alpha = var_alpha)) +
  geom_line(linewidth = 1.2) +
  guides(colour = guide_legend(nrow = 1), alpha = "none") +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = c(.01, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_colour_insta +
  theme_instagram +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-government-debt-to-income-", lang, ".jpeg"), height = 5, width = 5)
