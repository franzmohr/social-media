rm(list = ls())

lang <- "de"

library(dplyr)
library(oenb)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

source("r-corporate-design-functions-ggplot2.R")

min_date <- "2015-01-01"

if (lang == "de") {
  var_labels <- c("Bruttoanlageinvestitionen in Wohnbauten",
                  "Neue Wohnbaukredite an private Haushalte")
  temp_title <- "Wohnbau: Investitionen und Neukreditvolumen (Österreich)"
  temp_subtitle <- "Mrd EUR (Jeweilige Preise, Quartalswerte)"
  temp_caption <- "Quelle: Eurostat, OeNB. Eigene Berechnungen."
}

invest <- get_eurostat(id = "namq_10_an6", filters = list(geo = "AT",
                                                       asset10 = "N111G",
                                                       unit = "CP_MEUR",
                                                       s_adj = "NSA"),
                    cache = FALSE) %>%
  filter(!is.na(values)) %>%
  mutate(value = values / 1000,
         var = "invest") %>%
  select(date = time, var, value)

#oenb_toc()
#oenb_dataset("100140002")

loans <- oenb_data("100140002", "VDBMSKNWOHNBAU", freq = "M") %>%
  mutate(value = value / 1000,
         date = as.Date(paste0(period, "-01")),
         qtr = as.yearqtr(date)) %>%
  group_by(qtr) %>%  filter(n() == 3) %>%  ungroup() %>%
  group_by(qtr) %>%
  summarise(value = sum(value),
            .groups = "drop") %>%
  mutate(date = as.Date(qtr),
         var = "newloans") %>%
  select(date, var, value)


temp <- bind_rows(invest, loans) %>%
  pivot_wider(names_from = "var") %>%
  na.omit() %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(name = factor(name, levels = c("invest", "newloans"),
                       labels = var_labels))


last_value <- format(as.yearqtr(max(temp$date)), "%YQ%q")
if (lang == "de") {
  temp_caption <- paste0(temp_caption, " Letzter Wert: ", last_value, ".")
}
if (lang == "en") {
  temp_caption <- paste0(temp_caption, " Last value: ", last_value, ".")
}

max_value <- max(temp$value)

g <- ggplot(temp, aes(x = date, y = value)) +
  geom_line(aes(colour = name), linewidth = 1.2) +
  scale_x_date(expand = c(0.01, 0), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max_value * 1.06), expand = c(0, 0)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  theme_instagram +
  scale_colour_insta

g

date_title <- format(Sys.Date(), "%Y%m%d")
ggsave(g, filename = paste0("pics/", date_title, "-rre-investment-and-new-loans-", lang, ".jpeg"), height = 7, width = 7)

