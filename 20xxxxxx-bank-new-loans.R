

rm(list = ls())

ctry <- c("AT", "DE", "FR", "IT")

lang <- "de"

library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

raw <- get_data(paste0("MIR.M.", paste0(ctry, collapse = "+"), ".B.A2A+A2B+A2C+A2D.A.B.A..EUR.P"))

temp <- raw %>%
  rename(date = obstime,
         geo = ref_area,
         value = obsvalue) %>%
  mutate(var = paste0(bs_item, "_", bs_count_sector)) %>%
  filter(!is.na(value)) %>%
  filter(var != "A2A_2250") %>%
  mutate(date = as.Date(paste0(date, "-01")),
         value = value / 1000) %>%
  select(date, geo, var, value)

max_date <- format(max(temp$date), "%YM%m")

var_levels <- c("A2A_2240", "A2C_2250", "A2B_2250", "A2D_2250")
if (lang == "de") {
  var_labels <- c("Nichtfinanzielle\nKaptialgesellschaften", "Private Haushalte\nWohnbau", "Private Haushalte\nKonsum", "Private Haushalte\nSonstige")
  fig_title <- "Neukredite nach Verwendung"
  fig_subtitle <- "Mrd EUR pro Monat"
  fig_caption <- paste0("Quelle: EZB-MIR. Kredite an Gegenparteien im Euroraum. Reine Neuvergabe. Letzter Wert: ", max_date, ".")
}

temp <- temp %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels))

max_value <- max(temp$value)

g <- ggplot(temp, aes(x = date, y = value, fill = var)) +
  geom_col(show.legend = FALSE) +
  facet_grid(geo~var, scales = "free_y") +
  scale_x_date(expand = c(.01, 1)) +
  #scale_y_continuous(limits = c(0, max_value * 1.06), expand = c(0,  0)) +
  scale_fill_corporate_design() +
  theme_corporate_design(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption)

g
save_chart(g, "bank-new-loans", lang = lang, format = "portrait")
