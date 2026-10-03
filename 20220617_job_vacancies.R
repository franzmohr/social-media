rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

rate <- get_eurostat(id = "jvs_q_nace2",
                    filters = list(geo = "AT",
                                   indic_em = "JOBRATE",
                                   sizeclas = "TOTAL",
                                   s_adj = "NSA"))

occu <- get_eurostat(id = "jvs_q_nace2",
                     filters = list(geo = "AT",
                                    indic_em = "JOBOCC",
                                    sizeclas = "TOTAL",
                                    s_adj = "NSA"))

raw <- bind_rows(rate, occu) %>%
  filter(!is.na(values)) %>%
  #        nace_r2 %in% c("B-F", "G-N", "O-S")) %>%
  select(time, geo, var = indic_em, sector = nace_r2, values) %>%
  filter(nchar(sector) == 1) %>%
  pivot_wider(names_from = "var", values_from = "values")

ggplot(raw, aes(x = time, y = JOBRATE)) +
  geom_line() +
  facet_wrap(~ sector)


raw <- bind_rows(rate, occu) %>%
  filter(!is.na(values)) %>%
#        nace_r2 %in% c("B-F", "G-N", "O-S")) %>%
  select(time, geo, var = indic_em, sector = nace_r2, values) %>%
  filter(nchar(sector) == 1) %>%
  pivot_wider(names_from = "var", values_from = "values") %>%
  #na.omit() #%>%
  group_by(sector) %>% filter(time == max(time)) %>% ungroup() %>%
  mutate(sector = reorder(sector, -JOBRATE, sum))

ggplot(raw, aes(x = sector, y = JOBRATE, group = as.character(time))) +
  geom_col(position = "dodge")


raw <- bind_rows(rate, occu) %>%
  filter(!is.na(values)) %>%
#          nace_r2 %in% c("B-F", "G-N", "O-S")) %>%
  select(time, geo, var = indic_em, sector = nace_r2, values) %>%
  pivot_wider(names_from = "var", values_from = "values") %>%
  na.omit() %>%
  group_by(time) %>%# filter(time == max(time)) %>% ungroup() %>%
  mutate(occu = JOBOCC / sum(JOBOCC) * 2)

ggplot(raw, aes(x = sector, y = JOBRATE)) +
  geom_col(aes(width = occu), position = position_dodge2())




source("theme_instagram.R")

g <- ggplot(comp, aes(x = time, y = values)) +
  geom_col(aes(fill = var_de), alpha = 1) +
  geom_line(data = line, aes(linetype = line_de), size = 1.2) +
  scale_x_date(expand = c(.01, 0), date_breaks = "2 months", date_labels = "%YM%m") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_insta +
  guides(fill = guide_legend(ncol = 2),
         linetype = guide_legend(ncol = 2, keywidth = 1.5)) +
  labs(title = "Beitrag zur Inflation (Österreich)", 
       subtitle = "Inflationsraten in %; Beiträge der Komponenten in Prozentpunkten",
       caption = "Quelle: Eurostat. Idee: OeNB (2020). Gesamtwirtschaftliche Prognose der OeNB für Österreich\n2020 bis 2023. Code unter https://github.com/franzmohr/instagram.") +
  theme_instagram +
  theme(legend.position="bottom", legend.box = "vertical")

ggsave(g, filename = "pics/20200207_infl_comp_de.jpeg", height = 5, width = 5)


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

