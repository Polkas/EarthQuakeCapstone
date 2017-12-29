
context("leaflet_map_label")


test_that("Is leaflet null",
          {
            df = eq_ReadQuake()

            df_tidy = eq_GenLocationName(eq_CleanData(df))

            gg = NULL

            gg = df_tidy %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 1980) %>%
              eq_CreateLabel() %>%
              eq_Map(label = "DATE")

            expect_failure(expect_null(gg))

          })

