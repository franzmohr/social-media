
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

# Nationale AL-Quote für AT

# Quelle: https://www.ams.at/arbeitsmarktdaten-und-medien/arbeitsmarkt-daten-und-arbeitsmarkt-forschung/berichte-und-auswertungen

# Alternative Quelle: https://www.data.gv.at/2023/07/24/daten-fuer-alle-ams-veroeffentlicht-11-neue-offene-datensaetze-zu-arbeitslosigkeit-und-offenen-stellen/ 

years <- 2016:as.numeric(substring(Sys.Date(), 1, 4))

url_path <- "https://www.ams.at/content/dam/download/arbeitsmarktdaten/%c3%b6sterreich/berichte-auswertungen/001_amd-nuts3_monate_"

col_names <- c("region", "w_unselb", "w_al", "w_quote", "m_unselb", "m_al", "m_quote", "g_unselb", "g_al", "g_quote")

months <- c("")

result <- NULL
for (i in years) {
  
  temp_file <- tempfile()
  file_type <- ifelse(i >= 2022, ".xlsx", ".xls")
  try_down <- try(download.file(paste(url_path, i, file_type, sep = ""),
                                destfile = temp_file,
                                mode = "wb"))
  
  if (!inherits(try_down, "try-error")) {
    sheets <- readxl::excel_sheets(temp_file)
    sheets <- sheets[!grepl("jahr", tolower(sheets))]
    
    for (j in sheets) {
      temp <- readxl::read_excel(temp_file, sheet = j, skip = 9,
                                 col_names = col_names) %>%
        mutate(date = j,
               year = i) %>%
        na.omit()
      
      result <- dplyr::bind_rows(result, temp)
    }
  }
  
  file.remove(temp_file)
}

arbeit_nat <- result %>%
  mutate(region_name = case_when(region == "Österreich" ~ "Österreich",
                                 TRUE ~ substring(region, 9, nchar(region))),
         region = case_when(region == "Österreich" ~ "Österreich",
                            TRUE ~ substring(region, 1, 5))) %>%
  mutate(month = dplyr::case_when(grepl("jän", date) ~ "01",
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
         date = zoo::as.Date.yearmon(zoo::as.yearmon(paste(year, "-", month, sep = ""), "%Y-%m")),
         date = lubridate::ceiling_date(date, "month") - 1) %>%
  dplyr::select(-year, -month) %>%
  tidyr::pivot_longer(cols = -c("date", "region", "region_name"), names_to = "var", values_to = "value") %>%
  dplyr::mutate(variable = paste0("unemp_nat_", var),
                variable = ifelse(variable == "unemp_nat_g_quote", "unemp_nat", variable),
                value = dplyr::case_when(variable %in% c("unemp_nat", "unemp_nat_w_quote", "unemp_nat_m_quote") ~ value * 100,
                                         TRUE ~ value),
                unit = dplyr::case_when(variable %in% c("unemp_nat", "unemp_nat_w_quote", "unemp_nat_m_quote") ~ "Prozent",
                                        TRUE ~ "Personen")) %>%
  filter(!is.na(value)) %>%
  select(date, region, region_name, variable, value, unit) %>%
  filter(!is.na(value)) %>%
  arrange(date)

temp <- arbeit_nat %>%
  filter(grepl("_al", variable) | grepl("_unselb", variable)) %>%
  mutate(var = substring(variable, 11, nchar(variable)),
         sex = substring(var, 1, 1),
         sex = factor(sex, levels = c("w", "m", "g"), labels = c("Frauen", "Männer", "Gesamt")),
         var = substring(var, 3, nchar(var)),
         var = factor(var, levels = c("al", "unselb"),
                      labels = c("Arbeitslos", "Unselbständig erwerbstätig")),
         value = value / 10^3) %>%
  filter(sex == "Gesamt",
         region != "Österreich")

fig_caption <- "Quelle: AMS. Nicht saisonell bereinigt. Letzter Wert: "
max_date <- format(max(temp$date), "%YM%m")
fig_caption <- paste0(fig_caption, max_date, ".")

source("theme_franz.R")

g <- ggplot(temp, aes(x = date, y = value, fill = var)) +
  geom_col() +
  scale_x_date(expand = c(.01, 0), date_labels = "%Y", date_breaks = "1 year") +
  facet_wrap(~region_name, ncol = 6, scales = "free_y") +
  scale_fill_franz() +
  labs(title = "Beschäftigungslage in Österreich nach NUTS-3-Region",
       subtitle = "Tausend Personen (unterschiedliche Skalierung der y-Axen)",
       caption = fig_caption) +
  theme_franz(base_size = 13) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

g
save_post(g, "austria-employment-by-nuts-3-levels", format = "portrait")
