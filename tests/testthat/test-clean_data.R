
context("file-clean_data")


test_that("clean data - new vars",
          {
            df = eq_ReadQuake()
            df_tidy = eq_CleanData(df)
            expect_true(all(c("MAGNITUDE","DATE") %in% colnames(df_tidy)))

          })

