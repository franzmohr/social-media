rm(list = ls())


lang <- "en"

ctry <- "AT+U2"

min_date <- "2019-01-01"


library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

konzepte <- c(paste0("BSI.M.", ctry, ".N.A.A20.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A30.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A42.A.1.U2.1000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A50.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.AXG.A.1.U4.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A60.X.1.Z5.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.T00.A.1.Z5.0000.Z01.E"))


raw <- c()
for (i in 1:length(konzepte)) {
  raw <- bind_rows(raw, get_data(konzepte[i]))
}



temp <- raw %>%
  rename(date = obstime,
         geo = ref_area,
         var = bs_item,
         value = obsvalue) %>%
  select(date, geo, var, value) %>%
  mutate(date = as.Date(paste0(date, "-01")),
         geo = case_when(geo == "U2" ~ "EA",
                         TRUE ~ geo),
         value = value / 1000) %>%
  pivot_wider(names_from = "var") %>%
  mutate(A99 = T00 - A20 - A30 - A42 - A50 - AXG - A60,
         B50 = A42 + A50) %>%
  select(-A42, -A50) %>%
  pivot_longer(cols = -c("date", "geo", "T00")) %>%
  arrange(geo, date, name) %>%
  group_by(geo, name) %>%
  mutate(value = value - lag(value, 12)) %>%
  ungroup() %>%
  filter(!is.na(value)) %>%
  filter(date >= min_date)


max_date <- format(max(temp$date), "%YM%m")


var_levels <- c("A20", "A30", "B50", "AXG", "A60", "A99")

if (lang == "de") {
  var_labels <- c("Kredite", "Schuldverschreibungen", "Beteiligungen",
                  "Externe Aktiva", "Nichtfinanzielle Aktiva", "Sonstige Aktiva")
  fig_title <- "Bilanz österreichischer MFIs (Aktivseite)"
  fig_subtitle <- "Veränderung zum Vorjahreswert in Mrd EUR"
  fig_caption <- paste0("Quelle: EZB-BSI. Letzter Wert: ", max_date, ".")
}

if (lang == "en") {
  var_labels <- c("Loans", "Debt securities", "Equity",
                  "External assets", "Nonfinancial assets", "Other assets")
  fig_title <- "Bank balance sheet composition (assets)"
  fig_subtitle <- "Change of component in bn EUR (y-o-y, unconsolidated)"
  fig_caption <- paste0("Source: ECB-BSI. Last observation: ", max_date, ".")
}



temp <- temp %>%
  mutate(name = factor(name, levels = var_levels, labels = var_labels))


g <- ggplot(temp, aes(x = date, y = value, fill = name)) +
  geom_col() +
  guides(fill = guide_legend(nrow = 2)) +
  facet_wrap(~ geo, ncol = 2, scales = "free_y") +
  scale_x_date(expand = c(.01, 1)) +
  scale_fill_insta +
  #scale_fill_viridis_d() +
  theme_instagram +
  theme(axis.title = element_blank()) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption)

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-bank-balance-sheet-composition-change-", lang, ".jpeg"), height = 7, width = 7)
