test_that("Gir ut riktig resultat for tom inndata", {
  expect_identical(
    erstatt_0(numeric(), "ingen"),
    numeric(0)
  )
})

test_that("Gir ut riktig resultat for enkelt-verdi inndata", {
  expect_identical(
    erstatt_0(0, "ingen"),
    "ingen"
  )
  expect_identical(
    erstatt_0(0L, "ingen"),
    "ingen"
  )
  expect_identical(
    erstatt_0(1.23, "ingen"),
    "1.23"
  )
  expect_identical(
    erstatt_0(NA_real_, "ingen"),
    "NA"
  )
  expect_identical(
    erstatt_0(0, -99),
    -99
  )
  expect_identical(
    erstatt_0(0, NA),
    NA
  )
})

test_that("Gir ut riktig resultat for vektor-verdi inndata", {
  expect_identical(
    erstatt_0(c(0, 1), "ingen"),
    c("ingen", "1")
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
    erstatt(factor(0), "ingen"),
    paste(feilmelding_inndata, class(factor(0))),
    fixed = TRUE
  )
})

test_that("Gir feilmelding ved ugyldig type for «nullverdi»-argumentet", {
  feilmelding_argument = paste0(
    "«nullverdi»-argumentet må være av klasse ",
    "«numeric» eller «character» og ha lengde 1"
  )
  expect_error(
    entall_flertall(0, factor(0)),
    feilmelding_argument
  )
  expect_error(
    entall_flertall(0, c("ingen", "null")),
    feilmelding_argument
  )
})
