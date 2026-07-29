tempmappe = tempdir()
klassefil_adresse = file.path(tempmappe, "kvalreg-rapport.cls")

test_that("Fila kvalreg-rapport.cls finst", {
  adresse = system.file("extdata", "kvalreg-rapport.cls", package = "rapwhale")
  expect_true(file.exists(adresse))
})

test_that("Gjev ut adressa til mappa det kopierast til (usynleg)", {
  adresse = expect_invisible(kopier_latex_klassefil(tempmappe))
  expect_identical(adresse, tempmappe)
})

test_that("LaTeX-klassefila vert kopiert til rett mappe", {
  expect_true(file.exists(klassefil_adresse))
})

test_that("Den kopierte fila har storleik større enn 0", {
  klassefil_info = file.info(klassefil_adresse)
  expect_gt(klassefil_info$size, 0)
})
