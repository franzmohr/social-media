rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)

temp <- tempfile()
download.file("http://hdr.undp.org/sites/default/files/hdro_statistical_data_table_1.xlsx",
              destfile = temp, mode = "wb")
data <- read_xlsx(temp, range = "A9:C189",
                  col_names = c("rank", "ctry", "value"),
                  col_types = c("text", "text", "numeric")) %>%
  filter(!is.na(value))
unlink(temp)

temp <- data %>%
  arrange(value)

lowest_5 <- temp %>%
  top_n(5, -value)

top_5 <- temp %>%
  top_n(5, value)

final <- temp %>%
  filter(ctry %in% c(lowest_5$ctry, top_5$ctry) | 
           ctry %in% c("Austria", "Belgium", "Canada", "Libanon",
                       "Cuba",
                       "United States", "United Kingdom", "Liechtenstein")) %>%
  arrange(value) %>%
  mutate(rank_de = paste0("Rang\n", rank))

final$ctry <- factor(final$ctry, levels = final$ctry)

source("theme_instagram.R")

g <- ggplot(final, aes(x = ctry, y = value, fill = "a")) +
  geom_col(show.legend = FALSE) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE) +
  scale_fill_insta +
  theme_instagram +
  theme(axis.text.x = element_text(hjust = 1))

g_temp <- g +
  geom_text(aes(label = rank), nudge_y = -.03, size = 3, colour = "white") +
  labs(title = "Kennst du die ärmsten und reichsten Länder der Welt?",
       subtitle = "Humand Development Index der UN",
       caption = "Quelle: http://hdr.undp.org/. Zahl in den Balken entspricht dem Rang.\nCode unter https://github.com/franzmohr/instagram.")

ggsave(g_temp, filename = "pics/20200920_world_dev_index_de.jpeg", height = 5, width = 5)

g <- ggplot(temp, aes(x = date, y = value, colour = names_en)) +
  geom_line(size = 1.2) +
  scale_x_date(date_breaks = "2 weeks", expand = c(.01, 0)) +
  labs(title = "New cases of Covid-19-infections",
       subtitle = "New cases per million (smoothed)",
       caption = "Source: https://ourworldindata.org/coronavirus.\nCode available at https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200918_corona_en.jpeg", height = 5, width = 5)



