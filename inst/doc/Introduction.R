## ----message=F,warning=F-------------------------------------------------
library(EarthQuakeCapstone)
library(dplyr)
library(ggplot2)
library(lubridate)

df = eq_ReadQuake()

## ------------------------------------------------------------------------
df_tidy =  eq_CleanData(df)

## ----fig.height=5,fig.width=6--------------------------------------------
df_tidy %>%
filter_at(c("COUNTRY"),all_vars(.%in% c("TURKEY","ITALY"))) %>%
  filter(.data$DATE > lubridate::origin) %>%
  eq_GenLocationName() %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, fill = DEATHS, size = MAGNITUDE)) +
  geom_TimeLine()

## ----fig.height=5,fig.width=6--------------------------------------------
df_tidy %>%
dplyr::filter_at(c("COUNTRY"),all_vars(.%in% c("TURKEY","ITALY"))) %>%
  dplyr::filter(.data$DATE > lubridate::origin) %>%
  eq_GenLocationName() %>%
  ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, fill = DEATHS, size = MAGNITUDE)) +
  geom_TimeLine() +
  geom_TimeLineLable(aes(label=LOCATION),check.overlap=T)


## ------------------------------------------------------------------------
df_tidy %>%
  eq_GenLocationName() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
  eq_Map(label = "DATE")

## ------------------------------------------------------------------------
df_tidy %>%
  eq_GenLocationName() %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
  eq_CreateLabel() %>%
  eq_Map(label = "popup_text")

