test_that("LaTeX-klassefila vert kopiert til rett mappe", {
  tempmappe = tempdir()
  kopier_latex_klassefil(tempmappe)
  expect_true(file.exists(paste0(tempmappe, "kvalreg-rapport.cls")))
})
