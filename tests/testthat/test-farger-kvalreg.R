test_that("utdata er en liste som inneholder 4 tekstvektorer: farger_hoved,
          farger_noyt, farger_kontr og farger_tillegg", {
  expect_length(purrr:::map_lgl(farger_kvalreg(), is.character), 4)

  navn_vektorer = c(
    "farger_hoved", "farger_noyt", "farger_kontr", "farger_tillegg"
  )
  navn_vektorer_funksjon = purrr::imap_chr(farger_kvalreg(), \(x, navn) navn) |>
    unname()
  expect_identical(navn_vektorer_funksjon, navn_vektorer)
})

test_that("farge_morkare gir ut forventede fargekoder ved ulike grader", {
  farger = c("#000059", "#084594", "#2171b5")

  forventet_grad_5 = c("#00004D", "#003A87", "#0065A7")
  forventet_grad_minus_5 = c("#170965", "#2450A1", "#367DC3")

  expect_identical(farge_morkare(farger, grad = 5), forventet_grad_5)
  expect_identical(farge_morkare(farger, grad = -5), forventet_grad_minus_5)
})
