
rm(list = ls())

# Choose language
lang <- "en" # "de" or "en"

# Choose country
ctry <- c("")

# Load packages
library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)
library(zoo)

if (lang == "de") {
  temp_var_labels <- c("Monetäre Finanz-\ninstitute (MFIs): Gesamt",
                       "Nichtfinanzielle\nUnternehmen", "Private\nHaushalte",
                       "MFIs: Davon\nZentralbanken", "MFIs: Davon\nandere MFIs")
  temp_title <- "Ausstehende Kredite von Banken im Euroraum"
  temp_subtitle <- "Mrd EUR"
  temp_caption <- "Quelle: EZB-BSI. Gegenparteien im Euroraum. Unkonsolidiert."
}
if (lang == "en") {
  temp_var_labels <- c("Monetary financial\ninstitutions (MFIs): Total",
                       "Non-financial\ncorporations", "Householdes",
                       "MFIs: Of which\ncentral banks", "MFIs: Of which\nother MFIs")
  temp_title <- "Outstanding loans by sector (euro area)"
  temp_subtitle <- "Bn EUR"
  temp_caption <- "Source: ECB-BSI. Counterparties within euro area. Unconsolidated data."
}

ctry <- paste0(ctry, collapse = "+")

#raw <- get_data(paste0("BSI.M.", ctry,".N.N...1...Z01.E"))

assets <- bind_rows(get_data(paste0("BSI.M.", ctry,".N.N.A20+A30+A50.A.1.U2.0000.Z01.E")),
                    get_data(paste0("BSI.M.", ctry,".N.N.A60+A7C.X.1.Z5.0000.Z01.E")),
                    get_data(paste0("BSI.M.", ctry,".N.N.A42.A.1.U2.1000.Z01.E")),
                    get_data(paste0("BSI.M.", ctry,".N.N.AXG.A.1.U4.0000.Z01.E"))) %>%
  rename(date = obstime,
         geo = ref_area)


liabs <- bind_rows(get_data(paste0("BSI.M.", ctry,".N.N.L40.A.1.Z5.0000.Z01.E")),
                   get_data(paste0("BSI.M.", ctry,".N.N.L20.A.1.U2.0000.Z01.E")),
                   get_data(paste0("BSI.M.", ctry,".N.N.L10+L60+L70.X.1.Z5.0000.Z01.E")),
                   get_data(paste0("BSI.M.", ctry,".N.N.LXG.A.1.U4.0000.Z01.E")),
                   get_data(paste0("BSI.M.", ctry,".N.N.T00.A.1.Z5.0000.Z01.E"))) %>%
  rename(date = obstime,
         geo = ref_area)


temp <- bind_rows(assets, liabs) %>%
  rename(name = bs_item,
         value = obsvalue) %>%
  filter(!is.na(value)) %>%
  select(date, geo, name, value) %>%
  pivot_wider(values_fill = 0) %>%
  arrange(desc(date)) %>%
  mutate(summe_asst = A20 + A30 + A42 + A50 + A60 + A7C + AXG,
         summe_liab = L10 + L20 + L40 + L60 + L70 + LXG,
         check_liab = summe_liab == T00,
         date = as.Date(paste0(date, "-01"))) %>%
  filter(date >= "2005-01-01")

check <- temp %>% filter(!check_liab)
if (nrow(check) > 0) {
  warning("Balance sheet numbers to not aggregate up.")
}

temp <- temp %>%
  select(-T00, -summe_asst, -summe_liab, -check_liab) %>%
  pivot_longer(cols = -c("date", "geo")) %>%
  filter(!is.na(value)) %>%
  mutate(value = value / 1000,
         side = ifelse(substring(name, 1, 1) == "A", "asset", "liability"))

ggplot(temp, aes(x = date, y = value, fill = name)) +
  geom_col() +
  facet_grid(geo~side, scales = "free_y") +
  labs(subtitle = temp_subtitle)

%>%
  
  select(obstime, count_area, bs_count_sector, obsvalue) %>%
  pivot_wider(names_from = "bs_count_sector", values_from = "obsvalue") %>%
  mutate(date = as.Date(as.yearmon(obstime, "%Y-%m"))) %>%
  select(-obstime, -count_area) %>%
  pivot_longer(cols = -c("date"), names_to = "var", values_to = "value") %>%
  filter(!is.na(value)) %>%
  arrange(date) %>%
  mutate(value = value  / 1000,
         alph = var %in% c("1100", "1200"),
         var = factor(var, levels = c("1000", "2240", "2250", "1100", "1200"),
                      labels = temp_var_labels))

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = value, colour = var, alpha = alph)) +
  geom_line(linewidth = .7) +
  guides(colour = guide_legend(nrow = 2), alpha = "none") +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_x_date(expand = c(.01, 0), date_label = "%Y", date_breaks = "2 years") +
  scale_alpha_manual(values = c(1, .3)) +
  scale_colour_franz() +
  theme_franz(base_size = 13) +
  coord_cartesian(ylim = c(0, max(temp$value) * 1.06), expand = FALSE)
save_post(g, "ncb-balance-sheet-structure", lang = lang, format = "portrait")
