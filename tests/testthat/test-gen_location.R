
context("file-gen_location")


test_that("check existence of country names",
          {
            df = eq_ReadQuake()

            df_loc = eq_GenLocationName(df)

            expect_true(any(grepl(paste(unique(tolower(df_loc$COUNTRY)),collapse="|"),tolower(df_loc$LOCATION))))

          })

