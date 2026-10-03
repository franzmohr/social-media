

rm(list = ls())


library(dplyr)
library(rsdmx)
library(tidyr)


dataset <- as.data.frame(readSDMX(providerId = 'IMF_DATA',
                                  resource = 'data',
                                  flowRef = "IMF.STA,GFS_BS",
                                  key = "AUT.S13...XDC.A"))


temp <- dataset %>%
  filter(!is.na(OBS_VALUE)) %>%
  #filter(TIME_PERIOD == max(TIME_PERIOD)) %>%
  filter(grepl("_A_SP", INDICATOR)) %>%
  select(TIME_PERIOD, INDICATOR, OBS_VALUE) %>%
  pivot_wider(names_from = "INDICATOR", values_from = "OBS_VALUE") %>%
  arrange(TIME_PERIOD)

unique(temp$TYPE_OF_TRANSFORMATION)


c("F1_A_SP", )



temp %>%
  select(N113_A_SP,
         N114_A_SP,
         N11P_A_SP,
         N11_A_SP) %>%
  mutate(summe = N113_A_SP + N114_A_SP + N11P_A_SP)
  
