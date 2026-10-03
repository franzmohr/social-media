rm(list = ls())


lang <- "en"

ctry <- "AT+DE+U2"

min_date <- "2005-01-01"


library(dplyr)
library(ecb)
library(ggplot2)
library(tidyr)

source("r-corporate-design-functions-ggplot2.R")

konzepte <- paste0("BSI.M.", ctry, ".N.A.A30.A.1.U2.1000+2100+2200.Z01.E")

raw <- get_data(konzepte)


temp <- raw %>%
  rename(date = obstime,
         geo = ref_area,
         var = bs_count_sector,
         value = obsvalue) %>%
  select(date, geo, var, value) %>%
  mutate(date = as.Date(paste0(date, "-01")),
         geo = case_when(geo == "U2" ~ "EA",
                         TRUE ~ geo),
         value = value / 1000) %>%
  #pivot_wider(names_from = "var")
  #pivot_longer(cols = -c("date", "geo", "T00")) %>%
  #arrange(geo, date, var) %>%
  #group_by(geo, var) %>%
  #mutate(value = value - lag(value, 12)) %>%
  #ungroup() %>%
  filter(!is.na(value)) %>%
  filter(date >= min_date)

max_date <- format(max(temp$date), "%YM%m")

var_levels <- c("1000", "2100", "2200")

if (lang == "en") {
  var_labels <- c("Monetary financial institutions (MFI)", "Non-MFIs\nGeneral government", "Non-MFIs\nExcluding general goverment")
  fig_title <- "Debt securities held by banks"
  fig_subtitle <- "Outstanding amounts in bn EUR (unconsolidated)"
  fig_caption <- paste0("Source: ECB-BSI. Counterparties in the euro area. Last observation: ", max_date, ".")
}


temp <- temp %>%
  mutate(var = factor(var, levels = var_levels, labels = var_labels))

g <- ggplot(temp, aes(x = date, y = value, fill = var)) +
  geom_col(show.legend = FALSE) +
  guides(fill = guide_legend(nrow = 2)) +
  facet_grid(geo ~ var, scales = "free_y") +
  scale_x_date(expand = c(.01, 1)) +
  scale_fill_insta +
  #scale_fill_viridis_d() +
  theme_instagram +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = fig_caption)

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-bank-debt-securities-", lang, ".jpeg"), height = 7, width = 7)
