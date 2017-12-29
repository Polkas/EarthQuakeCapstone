
context("file-existence")

test_that("files exist",
{

expect_true(file.exists(system.file("exdata","EarthQuakeData.txt",package = "EarthQuakeCapstone")))

})

