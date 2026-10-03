rm(list = ls())

library(dplyr)
library(eurostat)
library(ggplot2)
library(tidyr)
library(zoo)

years <- 2016:2020

url_path <- "https://www.ams.at/content/dam/download/arbeitsmarktdaten/%c3%b6sterreich/berichte-auswertungen/001_amd-nuts3_monate_"

col_names <- c("region", "w_unselb", "w_al", "w_quote", "m_unselb", "m_al", "m_quote", "g_unselb", "g_al", "g_quote")

months <- c("")

down_file <- tempfile()
result <- NULL
for (i in years) {
  #store_name <- paste("D:/data/at_unemp_", i, ".xls", sep = "")
  download.file(paste(url_path, i, ".xls", sep = ""),
                destfile = down_file, #store_name,
                mode = "wb")
  #sheets <- readxl::excel_sheets(store_name)
  sheets <- readxl::excel_sheets(down_file)
  sheets <- sheets[!grepl("jahr", tolower(sheets))]
  
  for (j in sheets) {
    #temp <- readxl::read_excel(store_name, sheet = j, skip = 9,
    temp <- readxl::read_excel(down_file, sheet = j, skip = 9,
                               col_names = col_names) %>%
      mutate(date = j,
             year = i) %>%
      na.omit()
    
    result <- bind_rows(result, temp)
  }
}

national <- result %>%
  filter(region == "Österreich") %>%
  mutate(month = case_when(grepl("jän", date) ~ "01",
                           grepl("feb", date) ~ "02",
                           grepl("mär", date) ~ "03",
                           grepl("apr", date) ~ "04",
                           grepl("mai", date) ~ "05",
                           grepl("jun", date) ~ "06",
                           grepl("jul", date) ~ "07",
                           grepl("aug", date) ~ "08",
                           grepl("sep", date) ~ "09",
                           grepl("okt", date) ~ "10",
                           grepl("nov", date) ~ "11",
                           grepl("dez", date) ~ "12"),
         date = as.yearmon(paste(year, "-", month, sep = ""), "%Y-%m"),
         type = "national") %>%
  rename(values = g_quote) %>%
  select(date, type, values)

# Download data
ilo <- get_eurostat(id = "une_rt_m", filters = list(geo = "AT",
                                                    age = c("TOTAL"),
                                                    sex = "T",
                                                    s_adj = "NSA",
                                                    unit = "PC_ACT")) %>%
  mutate(type = "int",
         time = as.yearmon(time),
         values = values / 100) %>%
  rename(date = time) %>%
  select(date, type, values)

temp <- bind_rows(national, ilo) %>%
  filter(!is.na(values)) %>%
  pivot_wider(names_from = "type", values_from = "values") %>%
  na.omit() %>%
  pivot_longer(cols = -c("date")) %>%
  mutate(name_de = factor(name, levels = c("int", "national"),
                          labels = c("Internationle Definition",
                                     "Nationale Definition")))

source("theme_instagram.R")

g <- ggplot(temp, aes(x = date, y = value, colour = name_de)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_line(size = 1.2) +
  scale_x_yearmon(expand = c(.01, 0), format = "%YM%m", n = 10) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Arbeitslosenquote (Österreich)",
       subtitle = "Anteil arbeitsloser Personen an der Erwerbsbevölkerung*",
       caption = "Quelle: AMS, Eurostat. Unbereinigte Daten. *Die nationale und internationale Definition der Ar-\nbeitslosenquote unterscheiden sich sowohl in ihrem Verständnis der als arbeitslos geltenden\nPersonen (im Zähler) als auch dem Kreis der Erwerbsbevölkerung (im Nenner). Mehr Informa-\ntionen unter shorturl.at/jsGNX. Code unter https://github.com/franzmohr/instagram.") +
  scale_colour_insta +
  theme_instagram

ggsave(g, filename = "pics/20200719_unemp_de.jpeg", height = 5, width = 5)
