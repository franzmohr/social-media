# inflation, components

rm(list = ls())

library(dplyr)
library(eurostat)
library(tidyr)
library(ggplot2)

# Countries of interest
ctry <- c("AT", "EA20")
ctry_labels <- c("Österreich", "Euroraum (20)")

# Language
lang <- "de"

# Starting date of the plot
min_date_plot <- "2019-01-01"

# Country used to obtain the most important drivers of inflation
ctry_for_selection <- "AT"
# Window used to obtain the most important drivers of inflation.
# NULL means "the three most recent months available", which is what you want
# for a recurring post -- a hard-coded window silently selects nothing once the
# data no longer reaches it.
min_date_for_selection <- NULL
max_date_for_selection <- NULL
n_months_for_selection <- 3

# Labels for plot
if (lang == "de") {
  temp_title <- "Kerninflation: Beitrag der Komponenten"
  temp_subtitle <- "Gewichtete Preisveränderung in Prozentpunkten"
  temp_caption <- paste0("Quelle: Eurostat. Eigene Berechnungen. COICOP Einheiten wurden auf Basis der Summe\n",
                         "der quadrierten gewichteten Preisänderungen der jeweiligen Komponente\n",
                         "zwischen %s ausgewählt.")
}
if (lang == "en") {
  temp_title <- "Core inflation: contribution by component"
  temp_subtitle <- "Weighted change of growth in percentage points"
  temp_caption <- paste0("Source: Eurostat. Own calculations. COICOP items were selected based on the sum of\n",
                         "squared weighted price changes of the respective component\n",
                         "between %s.")
}

# ******************************************************************************
# ******************************************************************************
# No manual input should be necessary from here on
# ******************************************************************************
# ******************************************************************************



# Main contributors to core inflation
# All items excluding energy food alcohol and tobacco

# Weights
weights <- get_eurostat(id = "prc_hicp_inw",
                        filters = list(geo = ctry),
                        cache = FALSE) %>%
  mutate(year = substring(time, 1, 4)) %>%
  select(-time, -freq) %>%
  rename(weight = values) %>%
  mutate(weight = weight / 1000)

# Growth prc_hicp_manr
index <- get_eurostat(id = "prc_hicp_manr",
                      filters = list(geo = ctry),
                      cache = FALSE) %>%
  mutate(time = as.Date(paste0(time, "-01"))) %>%
  filter(time >= min_date_plot,
         !is.na(values)) %>%
  select(-freq)

# Mapping from
# https://ec.europa.eu/eurostat/documents/3859598/18594110/KS-GQ-24-003-EN-N.pdf/8490b532-f9e2-5b63-16fa-d88cda14c7b4?version=3.0&t=1709119338351

comp <- index %>%
  # Only consider "raw data"
  filter(substring(coicop, 1, 2) == "CP",# coicop == "TOT_X_NRG_FOOD",
         # Entirely omitted
         !substring(coicop, 1, 4) %in% c("CP02"),
         !substring(coicop, 1, 5) %in% c("CP011", "CP012", "CP045"),
         # w/o NRG (ELC_GAS, FUEL)
         !coicop %in% c("CP0451", "CP0452", "CP0454", "CP0455"), # ELC_GAS
         !coicop %in% c("CP0453", "CP072", "CP0722", "CP07221", "CP07222", "CP07223"), # FUEL
         # w/o FOOD (FOOD_P, FOOD_NP)
         # FOOD_P
         !coicop %in% c("CP0111",
                        "CP01127", "CP01128",
                        "CP01132", "CP01134", "CP01135", "CP01136",
                        "CP01141", "CP01142", "CP01143", "CP01144", "CP01145", "CP01146",
                        "CP0115",
                        "CP01162", "CP01163", "CP01164",
                        "CP01172", "CP01173", "CP01174", "CP01175", "CP01176",
                        "CP0118",
                        "CP0119",
                        "CP012",
                        "CP02"),
         # FOOD_NP
         !coicop %in% c("01121", "CP01122", "CP01123", "CP01124", "CP01125", "CP01126",
                        "CP01131", "CP01133",
                        "CP01147",
                        "CP01161",
                        "CP01171"),
         nchar(coicop) > 4) %>% # coicop == "TOT_X_NRG_FOOD") %>%
  #nchar(coicop) <= 6) %>%
  mutate(year = substring(time, 1, 4)) %>%
  left_join(weights, by = c("year", "geo", "coicop")) %>%
  mutate(values = values * weight) %>%
  mutate(cond = case_when(coicop == "TOT_X_NRG_FOOD" ~ TRUE,
                          nchar(coicop) == 5 & !coicop %in% c("CP041", "CP111", "CP125") ~ TRUE,
                          nchar(coicop) == 6 & substring(coicop, 1, 5) %in% c("CP041", "CP111", "CP125") ~ TRUE,
                          TRUE ~ FALSE)) %>%
  filter(cond) %>%
  select(-weight)

# Resolve the selection window against what the data actually reaches
avail <- sort(unique(comp$time[comp$geo == ctry_for_selection]))
if (is.null(max_date_for_selection)) max_date_for_selection <- max(avail)
if (is.null(min_date_for_selection)) {
  min_date_for_selection <- avail[max(1, length(avail) - n_months_for_selection + 1)]
}

# The caption must name the window the selection was actually made over
temp_caption <- sprintf(temp_caption,
                        paste(format(min_date_for_selection, "%Y-%m"),
                              if (lang == "de") "und" else "and",
                              format(max_date_for_selection, "%Y-%m")))

# Get indicators with highest contribution to inflation in period
top_comp <- comp %>%
  filter(time >= min_date_for_selection,
         time <= max_date_for_selection,
         geo == ctry_for_selection) %>%
  group_by(coicop) %>%
  summarise(value = sum(values^2),
            .groups = "drop") %>%
  arrange(desc(value)) %>%
  slice(1:12) %>%
  pull("coicop") %>%
  as.character()

source("r-corporate-design-functions-ggplot2.R")

# Helper file with mapping of COICOP code and its title
coicop <- read.csv("mapping-coicop-1.csv") %>%
  filter(coicop %in% top_comp)

# A COICOP without a row in the mapping keeps its own code as its label, so a
# gap shows up as "CP123" and not as a facet titled NA. CP123 is missing today.
unmapped <- setdiff(top_comp, coicop$coicop)
if (length(unmapped) > 0) {
  warning("no mapping row for: ", paste(unmapped, collapse = ", "))
  coicop <- bind_rows(coicop,
                      data.frame(coicop = unmapped, var_de = unmapped,
                                 var_en = unmapped))
}

coicop <- coicop %>%
  mutate(coicop = factor(coicop, levels = top_comp)) %>%
  arrange(coicop) %>%
  as.data.frame()

if (lang == "de") {
  coicop[, "var"] <- gsub("\\n", "\n", coicop[, "var_de"], fixed = TRUE)
}
if (lang == "en") {
  coicop[, "var"] <- gsub("\\n", "\n", coicop[, "var_en"], fixed = TRUE)
}


coicop_levels <- pull(coicop, "coicop")
coicop_labels <- wrap_labels(pull(coicop, "var"), width = 16)

# comp %>%
#   select(time, values, geo, coicop) %>%
#   pivot_wider(names_from = "coicop", values_from = "values") %>%
#   pivot_longer(cols = -c("time", "geo", "TOT_X_NRG_FOOD")) %>%
#   group_by(time, geo) %>%
#   summarise(value = sum(value, na.rm = TRUE),
#             tot = mean(TOT_X_NRG_FOOD),
#             .groups = "drop") %>%
#   tail()

temp <- comp %>%
  filter(coicop %in% top_comp) %>%
  mutate(coicop = factor(coicop, levels = coicop_levels, labels = coicop_labels),
         geo = factor(geo, levels = ctry, labels = ctry_labels))

show_legend <- length(ctry) > 1

g <- ggplot(temp, aes(x = time, y = values)) +
  geom_zeroline() +
  geom_line(aes(colour = geo), alpha = 1, show.legend = show_legend) +
  facet_wrap(~coicop, nrow = 3) +
  labs(title = temp_title,
       subtitle = temp_subtitle,
       caption = temp_caption) +
  scale_x_date(expand = c(.01, 0), date_breaks = "2 years", date_labels = "%Y") +
  scale_colour_corporate_design() +
  theme_corporate_design(base_size = 9) +
  theme(axis.title = element_blank()) +
  theme(legend.box = "vertical")

g

save_chart(g, "core-inflation-main-drivers", lang = lang, format = "portrait")

