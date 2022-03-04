test_that("Gir ut riktig resultat når inndata har lengde 0", {
  expect_identical(
    erstatt_0(numeric(), "ingen"),
    numeric(0)
  )
})

test_that("Gir ut riktig resultat når inndata har lengde 1", {
  expect_identical(
    erstatt_0(0, "ingen"),
    "ingen"
  )
  expect_identical(
    erstatt_0(0L, "ingen"),
    "ingen"
  )
})

test_that("Gir ut riktig resultat når inndata har lengde > 1", {
  expect_identical(
    erstatt_0(c(2, 0, 1, 0), "ingen"),
    c("2", "ingen", "1", "ingen")
  )
})

test_that("Gjør ikke resultatet om til tekst med mindre det er nødvendig", {
  expect_identical(
    erstatt_0(1.23, "ingen"),
    1.23
  )
  expect_identical(
    erstatt_0(NA_real_, "ingen"),
    NA_real_
  )
  expect_identical(
    erstatt_0(0, -99),
    -99
  )
  expect_identical(
    erstatt_0(0, NA),
    NA
  )
  expect_identical(
    erstatt_0(c(2, 1, 3), "ingen"),
    c(2, 1, 3)
  )
})

test_that("Gir feilmelding ved ugyldig type for «x»-argumentet", {
  feilmelding_inndata = "Inndata «x» må være tall, men er:"
  expect_error(
    erstatt_0("null", "ingen"),
    paste(feilmelding_inndata, class("null")),
    fixed = TRUE
  )
  expect_error(
    erstatt_0(factor(0), "ingen"),
    paste(feilmelding_inndata, class(factor(0))),
    fixed = TRUE
  )
})

test_that("Gir feilmelding ved hvis «nullverdi» ikke har nøyaktig 1 element", {
  feilmelding_nullverdi = "«nullverdi» må ha nøyaktig 1 element"
  expect_error(
    erstatt_0(0, c("ingen", "null")),
    feilmelding_nullverdi
  )
})
