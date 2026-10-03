rm(list = ls())

library(dplyr)
library(ecb)
library(eurostat)
library(oenb)
library(ggplot2)
library(tidyr)
library(zoo)

lang <- "de"

# ATX

tmp <- tempfile()
download.file("https://www.wienerborse.at/index/atx-AT0000999982/historische-daten/?c7012%5BDOWNLOAD%5D=csv&c7012%5BDATETIME_TZ_END_RANGE%5D=12.04.2026&c7012%5BDATETIME_TZ_START_RANGE%5D=12.03.2002",
              destfile = tmp)


atx <- read.csv2(tmp) %>%
  select(date = Datum, value = Schlusspreis) %>%
  mutate(date = as.Date(date, "%d.%m.%Y"),
         qtr = as.yearqtr(date),
         max_date = max(date),
         qtr_max = as.yearqtr(max_date)) %>%
  filter(qtr != qtr_max) %>%
  arrange(date) %>%
  group_by(qtr) %>% slice(n()) %>% ungroup() %>%
  mutate(name = "atx") %>%
  select(date = qtr, value, name)

file.remove(tmp)
rm(tmp)

# ATX Total Retrun

tmp <- tempfile()
download.file("https://www.wienerborse.at/index/atx-tr-AT0000A09FJ6/historische-daten/?c7012%5BDOWNLOAD%5D=csv&c7012%5BDATETIME_TZ_END_RANGE%5D=12.04.2026&c7012%5BDATETIME_TZ_START_RANGE%5D=12.03.2002",
              destfile = tmp)

atx_tr <- read.csv2(tmp) %>%
  select(date = Datum, value = Schlusspreis) %>%
  mutate(date = as.Date(date, "%d.%m.%Y"),
         qtr = as.yearqtr(date),
         max_date = max(date),
         qtr_max = as.yearqtr(max_date)) %>%
  filter(qtr != qtr_max) %>%
  arrange(date) %>%
  group_by(qtr) %>% slice(n()) %>% ungroup() %>%
  mutate(name = "atx_tr") %>%
  select(date = qtr, value, name)

file.remove(tmp)
rm(tmp)


# Inflation
inflation <- get_eurostat(id = "prc_hicp_midx",
                          filters = list(geo = "AT",
                                         unit = "I15",
                                         coicop = "CP00"),
                          cache = FALSE) %>%
  mutate(date = as.yearqtr(time)) %>%
  arrange(date) %>%#
  rename(ctry = geo) %>%
  group_by(date) %>%
  filter(n() == 3) %>%
  summarise(value = mean(values),
            name = "infl",
            .groups = "drop")

# House prices
hp <- oenb_data(id = "6", pos = c("VDBPLIMOPATGEZBN"), freq = "Q") %>%
  arrange(period) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.yearqtr(period, "%Y-Q%q")) %>%
  mutate(name = case_when(pos == "VDBPLIMOPATGEZBN" ~ "immo",
                          pos == "VDBPLIMOPAT00" ~ "wo_wien",
                          pos == "VDBPLIMOPWIEN00" ~ "wien")) %>%
  select(date, value, name)

temp <- bind_rows(inflation, hp, atx, atx_tr) %>%
  filter(date >= "2010 Q1") %>%
  arrange(date) %>%
  group_by(name) %>%
  mutate(idx = value / value[1] * 100) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = c("infl", "immo", "atx", "atx_tr"),
                       labels = c("Verbraucherpreisindex", "Immobilienpreise", "ATX", "ATX Total Return")))

if (lang == "de") {
  fig_title <- "ATX, Immobilienpreise und Inflation in Österreich"
  fig_subtitle <- "Index (2010Q1 = 100)"
  fig_caption <- "Quelle: Eurostat, OeNB, Wiener Börse. Eigene Berechnungen.\nATX-Werte beziehen sich auf Quartals-Endkurse. Letzter Wert: "
}

max_date <- format(max(temp$date), "%YQ%q")
fig_caption <- paste0(fig_caption, max_date, ".")

source("r-corporate-design-functions-ggplot2.R")


g <- ggplot(temp, aes(x = date, y = idx, colour = name)) +
  geom_hline(yintercept = 100, colour = "black") +
  geom_line(linewidth = 1.2) +
  scale_x_yearqtr(expand = c(.01, 0), format = "%YQ%q", n = 10) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption) +
  scale_colour_insta +
  theme_instagram +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-inflation-atx-house-prices-in-at-", lang, ".jpeg"), height = 5, width = 5)
