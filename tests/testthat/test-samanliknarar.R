test_that("Utdata skal seia (elementvis) om verdi1 er lik verdi2", {
  expect_identical(
    samanlikn_identisk(
      varnamn = rep("vekt", 5),
      verdi1 = c(74, 74, 74, NA, NA),
      verdi2 = c(74, 80, NA, 74, NA)
    ),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = NULL,
      verdi1 = c(74, 74, 74, NA, NA),
      verdi2 = c(74, 80, NA, 74, NA)
    ),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("dato", 5),
      verdi1 = c(74, 74, 74, NA, NA) + Sys.Date(),
      verdi2 = c(74, 80, NA, 74, NA) + Sys.Date()
    ),
    c(TRUE, FALSE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("vekt", 7),
      verdi1 = c("foo", "foo", "", "", "NA", NA, NA),
      verdi2 = c("foo", "bar", NA, "", NA, "", NA)
    ),
    c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )

  expect_identical(
    samanlikn_identisk(
      varnamn = rep("janei", 8),
      verdi1 = c(TRUE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA),
      verdi2 = c(TRUE, FALSE, TRUE, NA, NA, TRUE, FALSE, NA)
    ),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("samanlikn_identisk() fungerer òg for vektorar med 1 eller 0 element", {
  # Eitt element
  expect_true(samanlikn_identisk(
    varnamn = "vekt",
    verdi1 = 74,
    verdi2 = 74
  ))
  expect_false(samanlikn_identisk(
    varnamn = "vekt",
    verdi1 = 74,
    verdi2 = 80
  ))

  # Null element
  expect_identical(samanlikn_identisk(
    varnamn = character(),
    verdi1 = numeric(),
    verdi2 = numeric()
  ), logical())
})

test_that("samanlikn_identisk() gjev feilmelding viss argumenta har ulik lengd", {
  expect_error(samanlikn_identisk(verdi1 = 74, verdi2 = 74, varnamn = c("foo", "bar")))
  expect_error(samanlikn_identisk(verdi1 = 74, verdi2 = c(74, 75), varnamn = "foo"))
  expect_error(samanlikn_identisk(verdi1 = c(74, 75), verdi2 = c(74, 75), varnamn = "foo"))
})

test_that("samanlikn_identisk() gjev feilmelding viss verdiane ikkje er av same type", {
  expect_error(samanlikn_identisk(verdi1 = 74, verdi2 = "74", varnamn = "foo"))
  expect_error(samanlikn_identisk(verdi1 = "74", verdi2 = 74, varnamn = "foo"))
})

test_that("samanlikn_identisk() gjev feilmelding dersom «varnamn» ikkje er tekst eller NULL", {
  expect_error(samanlikn_identisk(verdi1 = 74, verdi2 = 74, varnamn = 123))
  expect_error(samanlikn_identisk(verdi1 = 74, verdi2 = 74, varnamn = function(x) {
    x
  }))
})

test_that("samanlikn_identisk() fungerer òg dersom «varnamn» er NULL", {
  expect_no_error(samanlikn_identisk(verdi1 = 74, verdi2 = 74, varnamn = NULL))
})
