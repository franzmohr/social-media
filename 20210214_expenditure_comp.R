rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)


items <- c("CP00", "CP01", "CP02", "CP03", "CP04", "CP05",
           "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12")

item_de <- c("Nahrungsmittel und alkoholfreie Getränke",
             "Alkoholische Getränke, Tabak und Narkotikum",
             "Bekleidung und Schuhe",
             "Wohnung, Wasser, Elektrizität, Gas und andere Brennstoffe",
             "Hausrat und laufende Instandhaltung des Hauses",
             "Gesundheit", "Verkehr", "Nachrichtenübermittlung", "Freizeit und Kultur",
             "Bildungswesen", "Restaurants und Hotels","Verschiedene Waren und Dienstleistungen")

raw <- get_eurostat(id = "prc_hicp_inw")

temp <- raw %>%
  rename(var = coicop) %>%
  filter(geo == "AT") %>%
  filter(var != "CP00") %>%
  filter(substring(var, 1, 1) == "C") %>%
  filter(!is.na(values)) %>%
  filter(time == max(time)) %>%
  mutate(time = substring(time, 1, 4),
         values = values / 1000) %>%
  arrange(desc(values)) %>%
  mutate(subgroup = substring(var, 1, 4),
         tot = ifelse(var == subgroup, values, NA)) %>%
  group_by(subgroup) %>%
  mutate(tot = mean(tot, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(tshare = tot,
         gshare = values / tot)

ggplot(temp, aes(x = tshare, y = gshare)) +
  geom_point()


temp <- temp %>%
  mutate(var = factor(var, levels = temp$var))

source("theme_instagram.R")

ggplot(temp, aes(x = var, y = values)) +
  geom_col(aes(fill = time), alpha = 1, position = "dodge") +
  #scale_x_date(expand = c(.01, 0), date_breaks = "2 months", date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Beitrag zur Inflation (Österreich)", 
       subtitle = "Anteil am Warenkorb in %",
       caption = "Quelle: Eurostat. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

ggsave(g, filename = "pics/20210214_infl_comp_de.jpeg", height = 5, width = 5)


g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var_en), alpha = 1) +
  geom_line(data = line, aes(linetype = line_en), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "2 months", date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Contribution to inflation (Austria)", 
       subtitle = "Inflation in %; Contribution of component in percentage points",
       caption = "Source: Eurostat. Idea: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich\n2020 bis 2023. Code available at https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

ggsave(g, filename = "pics/20200207_infl_comp_en.jpeg", height = 5, width = 5)

