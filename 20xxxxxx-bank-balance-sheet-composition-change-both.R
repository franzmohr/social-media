rm(list = ls())

lang <- "en"
ctry <- "AT+U2"
min_date <- "2022-01-01"


library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

source("theme_franz.R")

# Asset side data ----
var_levels <- c("A20", "A30", "B50", "AXG", "A60", "A99")
if (lang == "de") {
  var_labels <- c("Kredite", "Schuldverschreibungen", "Beteiligungen",
                  "Externe Aktiva", "Nichtfinanzielle Aktiva", "Sonstige Aktiva")
  asset_fig_title <- "Aktivseite"
  asset_fig_subtitle <- "Veränderung zum Vorjahreswert in Mrd EUR"
  fig_caption <- "Quelle: EZB-BSI. Unkonsolidiert. Letzter Wert: "
}

if (lang == "en") {
  var_labels <- c("Loans", "Debt securities", "Equity",
                  "External assets", "Nonfinancial assets", "Other assets")
  asset_fig_title <- "Assets"
  asset_fig_y <- "Change of component in bn EUR (y-o-y)"
  fig_caption <- "Source: ECB-BSI. Unconsolidated. Last value: "
}

asst_konzepte <- c(paste0("BSI.M.", ctry, ".N.A.A20.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A30.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A42.A.1.U2.1000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A50.A.1.U2.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.AXG.A.1.U4.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.A60.X.1.Z5.0000.Z01.E"),
              paste0("BSI.M.", ctry, ".N.A.T00.A.1.Z5.0000.Z01.E"))


asset <- c()
for (i in 1:length(asst_konzepte)) {
  asset <- bind_rows(asset, get_data(asst_konzepte[i]))
}

asset <- asset %>%
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
  filter(date >= min_date) %>%
  mutate(name = factor(name, levels = var_levels, labels = var_labels))


# Liability-side data ----

var_levels <- c("L20", "L30", "L40", "LXG", "L70", "L60")
if (lang == "de") {
  var_labels <- c("Einlagen", "Anteile an Geldmarktfonds", "Schuldverschreibungen",
                  "Externe Passiva", "Sonstige Passiva", "Kapital und Reserven")
  
  liab_fig_title <- "Passivseite"
}

if (lang == "en") {
  var_labels <- c("Deposits", "Money market fund shares", "Debt securities issued",
                  "External liabilities", "Remaining liabilities", "Capital and reserves")
  liab_fig_title <- "Liabilities"
}

liab_konzepte <- c(paste0("BSI.M.", ctry,".N.A.L20.A.1.U2.0000.Z01.E"),
                   paste0("BSI.M.", ctry,".N.A.L30.A.1.U2.0000.Z01.E"),
                   paste0("BSI.M.", ctry,".N.A.L40.A.1.U2.0000.Z01.E"),
                   paste0("BSI.M.", ctry,".N.A.L60.X.1.Z5.0000.Z01.E"),
                   paste0("BSI.M.", ctry,".N.A.LXG.A.1.U4.0000.Z01.E"),
                   paste0("BSI.M.", ctry,".N.A.L70.X.1.Z5.0000.Z01.E"))

liab <- NULL
for (i in 1:length(liab_konzepte)) {
  liab <- bind_rows(liab, get_data(liab_konzepte[i]))
}

liab <- liab %>%
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
  pivot_longer(cols = -c("date", "geo")) %>%
  arrange(geo, date, name) %>%
  group_by(geo, name) %>%
  mutate(value = value - lag(value, 12)) %>%
  ungroup() %>%
  mutate(name = factor(name, levels = var_levels, labels = var_labels)) %>%
  filter(!is.na(value)) %>%
  filter(date >= min_date)



# Graphs ----

g_asset <- ggplot(asset, aes(x = date, y = value, fill = name)) +
  geom_col() +
  guides(fill = guide_legend(ncol = 1)) +
  facet_wrap(~ geo, ncol = 1, scales = "free_y") +
  scale_x_date(expand = c(.01, 1)) +
  scale_fill_franz() +
  theme_franz(base_size = 13) +
  theme(axis.title.x = element_blank()) +
  labs(title = asset_fig_title,
       y = asset_fig_y)

g_liab <- ggplot(liab, aes(x = date, y = value, fill = name)) +
  geom_col() +
  guides(fill = guide_legend(ncol = 1)) +
  facet_wrap(~ geo, ncol = 1, scales = "free_y") +
  scale_x_date(expand = c(.01, 1)) +
  scale_fill_franz() +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank()) +
  labs(title = liab_fig_title)


temp <- bind_rows(asset, liab)
max_date <- format(max(temp$date), "%YM%m")
fig_caption <- paste0(fig_caption, max_date, ".")

g <- cowplot::plot_grid(g_asset, g_liab, align = "h", axis = "b")

g <- cowplot::ggdraw(cowplot::add_sub(g, label = fig_caption,
                                      x = .05, y = 0.5, hjust = 0, vjust = 0, size = 9))

g
save_post(g, "bank-balance-sheet-composition-change-both", lang = lang, format = "portrait")
