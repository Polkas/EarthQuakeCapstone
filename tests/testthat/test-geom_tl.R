
context("geom-tl")


test_that("Is ggplot null",
          {
            df = eq_ReadQuake()

            df_tidy = eq_GenLocationName(eq_CleanData(df))

            gg = NULL

            gg = df_tidy %>%
              dplyr::filter_at(c("COUNTRY"),dplyr::all_vars(.%in% c("TURKEY","ITALY"))) %>%
              dplyr::filter(.data$DATE > lubridate::origin) %>%
              ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, fill = DEATHS, size = MAGNITUDE)) +
              geom_TimeLine()


            expect_failure(expect_null(gg))

          })

