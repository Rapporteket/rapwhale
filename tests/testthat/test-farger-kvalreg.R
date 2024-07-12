test_that("utdata er en liste som inneholder 3 tekstvektorer: farger_hoved,
          farger_noyt, farger_kontr", {
  expect_length(purrr::map_lgl(farger_kvalreg(), is.character), 3)

  navn_vektorer = c("farger_hoved", "farger_noyt", "farger_kontr")
  expect_named(farger_kvalreg(), navn_vektorer)
})

test_that("farge_morkare gir ut forventede fargekoder ved ulike grader", {
  farger = c("#000059", "#084594", "#2171b5")

  forventet_grad_5 = c("#00004D", "#003A87", "#0065A7")
  forventet_grad_minus_5 = c("#170965", "#2450A1", "#367DC3")

  expect_identical(farge_morkare(farger, grad = 5), forventet_grad_5)
  expect_identical(farge_morkare(farger, grad = -5), forventet_grad_minus_5)
})
