rm(list = ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)

lang <- "de"

# Penn World Table 11.0 -------------------------------------------------------
# Same local copy as 20260926-gdp-consumption-capital-growth.R: PWT 11.0 is a
# fixed release and dataverse.nl rate-limits repeated downloads.
pwt_cache <- "D:/projects/MacroData/pwt/pwt110.xlsx"
if (!file.exists(pwt_cache)) {
  dir.create(dirname(pwt_cache), recursive = TRUE, showWarnings = FALSE)
  download.file("https://dataverse.nl/api/access/datafile/554105", pwt_cache,
                mode = "wb", quiet = TRUE)
}

# Hours worked from OECD and ILO instead of PWT's avh, see hours_worked.R
source("hours_worked.R")
hrs <- load_hours()

# rgdpna: real GDP, rconna: real consumption, both at constant 2021 national
# prices converted with 2021 PPPs, so levels are comparable across countries.
temp <- read_excel(pwt_cache, sheet = "Data") %>%
  transmute(iso = countrycode, year, pop, emp, gdp = rgdpna, c_pc = rconna / pop) %>%
  inner_join(select(hrs, iso, year, hours, series), by = c("iso", "year")) %>%
  filter(year >= 1970, year <= 2020,
         !is.na(gdp), !is.na(c_pc), !is.na(emp), !is.na(hours))

# Growth of GDP per head splits exactly into three log growth rates:
#   GDP / head = GDP / hour  x  hours / worker  x  workers / head
# Growth is only taken between consecutive years of the same hours series,
# never across a break in the ILO survey data.
growth <- temp %>%
  group_by(iso) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(ok = year - lag(year) == 1 & series == lag(series),
         total = log(gdp / pop) - lag(log(gdp / pop)),
         per_hour = log(gdp / (emp * hours)) - lag(log(gdp / (emp * hours))),
         hours_worker = log(hours) - lag(log(hours)),
         workers_head = log(emp / pop) - lag(log(emp / pop))) %>%
  ungroup() %>%
  filter(ok) %>%
  # Growth 1971-1980 belongs to the decade that starts in 1970, and so on
  mutate(decade = 1970 + 10 * ((year - 1971) %/% 10))

# Countries are split into thirds by consumption per head at the start of
# each decade, among the countries with hours data in that year
groups <- temp %>%
  filter(year %% 10 == 0, year <= 2010) %>%
  group_by(year) %>%
  mutate(grp = ntile(c_pc, 3)) %>%
  ungroup() %>%
  select(iso, decade = year, grp)

growth <- inner_join(growth, groups, by = c("iso", "decade"))

# Average annual growth over all country-years in a group, in percent
decomp <- function(d) {
  d %>%
    group_by(period, grp) %>%
    summarise(n_ctry = n_distinct(iso),
              across(c(total, per_hour, hours_worker, workers_head), ~ mean(.x) * 100),
              .groups = "drop")
}

res <- bind_rows(
  decomp(mutate(growth, period = "all")),
  decomp(mutate(growth, period = as.character(decade)))
)

print(as.data.frame(res), digits = 2)

n_all <- n_distinct(growth$iso)

if (lang == "de") {
  period_names <- c(all = "1971–2020", `1970` = "1971–1980", `1980` = "1981–1990",
                    `1990` = "1991–2000", `2000` = "2001–2010", `2010` = "2011–2020")
  grp_names <- c("Niedrig", "Mittel", "Hoch")
  comp_names <- c(per_hour = "BIP je Arbeitsstunde", hours_worker = "Stunden je Erwerbstätigen",
                  workers_head = "Erwerbstätige je Einwohner")
  fig_title <- "Reiche Länder wachsen langsamer,\nvor allem weil die Produktivität stockt"
  fig_subtitle <- paste0("Wachstum des realen BIP je Einwohner und seine Komponenten, in % pro Jahr.\n",
                         "Länder nach Konsum je Einwohner zu Beginn jedes Jahrzehnts in Drittel geteilt.\n",
                         "Punkt: BIP je Einwohner. n: Zahl der Länder.")
  src <- "Penn World Table 11.0, OECD, ILO."
  fig_note <- paste0(n_all, " Länder. Arbeitsstunden: OECD, übrige Länder ILO-Arbeitskräfteerhebungen ",
                     "(ab 1985).\nVor 1990 fast nur OECD-Länder, das untere Drittel sind dort ärmere OECD-Länder.")
  dec_mark <- ","
} else {
  period_names <- c(all = "1971–2020", `1970` = "1971–1980", `1980` = "1981–1990",
                    `1990` = "1991–2000", `2000` = "2001–2010", `2010` = "2011–2020")
  grp_names <- c("Low", "Middle", "High")
  comp_names <- c(per_hour = "GDP per hour worked", hours_worker = "Hours per worker",
                  workers_head = "Workers per head")
  fig_title <- "Rich countries grow more slowly,\nmainly because productivity stalls"
  fig_subtitle <- paste0("Growth of real GDP per head and its components, % per year.\n",
                         "Countries split into thirds by consumption per head at the start of each decade.\n",
                         "Dot: GDP per head. n: number of countries.")
  src <- "Penn World Table 11.0, OECD, ILO."
  fig_note <- paste0(n_all, " countries. Hours: OECD, other countries ILO labour force surveys ",
                     "(from 1985).\nBefore 1990 almost only OECD members, so the lower third are poorer OECD members.")
  dec_mark <- "."
}

res <- res %>%
  mutate(period = factor(period_names[period], levels = unname(period_names)),
         grp = factor(grp_names[grp], levels = grp_names))

bars <- res %>%
  pivot_longer(c(per_hour, hours_worker, workers_head), names_to = "comp", values_to = "value") %>%
  mutate(comp = factor(comp_names[comp], levels = unname(comp_names)))

source("r-corporate-design-functions-ggplot2.R")

comp_colours <- setNames(unname(design_colours()[c("blue", "amber", "teal")]), unname(comp_names))

fmt <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = dec_mark)

# Totals are labelled just beyond the stacked bar: above it when growth is
# positive, below it when negative
lab <- bars %>%
  group_by(period, grp) %>%
  summarise(up = sum(pmax(value, 0)), down = sum(pmin(value, 0)), .groups = "drop") %>%
  inner_join(res, by = c("period", "grp")) %>%
  mutate(y = ifelse(total >= 0, pmax(up, total) + .2, pmin(down, total) - .2))

g <- ggplot(bars, aes(x = grp)) +
  geom_col(aes(y = value, fill = comp), width = .7) +
  geom_hline(yintercept = 0, colour = design_colours()[["ink"]], linewidth = .4) +
  geom_point(data = res, aes(y = total), shape = 21, size = 2.6, stroke = .9,
             fill = design_colours()[["paper"]], colour = design_colours()[["ink"]]) +
  geom_text(data = lab, aes(y = y, label = fmt(total), vjust = ifelse(total >= 0, 0, 1)),
            size = 3, fontface = "bold", family = font_corporate_design, colour = design_colours()[["ink"]]) +
  # Group sizes: the early decades rest on few countries
  geom_text(data = res, aes(y = -Inf, label = paste0("n = ", n_ctry)), vjust = -.6,
            size = 2.6, family = font_corporate_design, colour = design_colours()[["ink_soft"]]) +
  facet_wrap(~ period, ncol = 3) +
  scale_fill_manual(values = comp_colours) +
  scale_y_continuous(labels = function(x) fmt(x), breaks = seq(-2, 5, 1),
                     expand = expansion(mult = c(.14, .1))) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = fig_title,
       subtitle = fig_subtitle,
       caption = caption_corporate_design(src, last = 2020, note = fig_note, lang = lang)) +
  theme_corporate_design(base_size = 11, grid = "y") +
  theme(axis.title = element_blank(),
        legend.text = element_text(size = 8.5),
        panel.spacing.x = unit(14, "pt"),
        panel.spacing.y = unit(16, "pt"))

g

save_chart(g, "growth-decomposition-consumption-level", lang = lang, format = "portrait")
