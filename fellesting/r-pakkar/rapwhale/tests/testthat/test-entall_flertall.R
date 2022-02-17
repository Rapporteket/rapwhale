test_that("Gir feilmelding ved ingen inndata", {
  feilmelding_ingen_inndata = "Ingen inndata"
  expect_error(
    entall_flertall(NULL, "operasjon", "operasjoner"),
    feilmelding_ingen_inndata
  )
  expect_error(
    entall_flertall(c(), "operasjon", "operasjoner"),
    feilmelding_ingen_inndata
  )
})

test_that("Gir ut riktig resultat for enkelt-verdi inndata", {
  expect_identical(
    entall_flertall(0, "operasjon", "operasjoner"),
    "0 operasjoner"
  )
  expect_identical(
    entall_flertall(1, "operasjon", "operasjoner"),
    "1 operasjon"
  )
  expect_identical(
    entall_flertall(123, "operasjon", "operasjoner"),
    "123 operasjoner"
  )
  expect_identical(
    entall_flertall(0, "nytt", "nye", "ingen"),
    "ingen nye"
  )
  expect_identical(
    entall_flertall(1L, "nytt", "nye", "ingen"),
    "1 nytt"
  )
})

test_that("Gir ut riktig resultat for vektor-verdi inndata", {
  expect_identical(
    entall_flertall(c(1, 123, 0), "operasjon", "operasjoner"),
    c("1 operasjon", "123 operasjoner", "0 operasjoner")
  )
  expect_identical(
    entall_flertall(c(1, 123, 0), "nytt", "nye", "ingen"),
    c("1 nytt", "123 nye", "ingen nye")
  )
})

test_that(
  "Gir ut riktig resultat for inndata som inneholder desimaltall",
  expect_identical(
    entall_flertall(
      c(1.1, 4.0, 1.0, 1.001, 1000.99999),
      "million", "millioner"
    ),
    c(
      "1.1 millioner", "4 millioner", "1 million", "1.001 millioner",
      "1000.99999 millioner"
    )
  )
)

test_that("Gir feilmelding ved inndata av negativ verdi", {
  feilmelding_negative_verdi = "Inndata inneholder minst en negativ verdi"
  expect_error(
    entall_flertall(-1, "operasjon", "operasjoner"),
    feilmelding_negative_verdi
  )
  expect_error(
    entall_flertall(c(1, -1), "operasjon", "operasjoner"),
    feilmelding_negative_verdi
  )
})

test_that("Gir feilmelding ved inndata av NA-verdi", {
  feilmelding_NA_verdi = "Inndata inneholder minst en NA-verdi"
  expect_error(
    entall_flertall(NA_integer_, "operasjon", "operasjoner"),
    feilmelding_NA_verdi
  )
  expect_error(
    entall_flertall(c(1, NA), "operasjon", "operasjoner"),
    feilmelding_NA_verdi
  )
})

test_that("Gir feilmelding ved ugyldig type inndata", {
  feilmelding_inndata = "Inndata må være tall (heltall/flyttall), men er:"
  expect_error(entall_flertall("en", "operasjon", "operasjoner"),
    paste(feilmelding_inndata, class("en")),
    fixed = TRUE
  )
  expect_error(entall_flertall(factor(1), "operasjon", "operasjoner"),
    paste(feilmelding_inndata, class(factor(1))),
    fixed = TRUE
  )
  expect_error(entall_flertall(Sys.Date(), "dato", "datoer"),
    paste(feilmelding_inndata, class(Sys.Date())),
    fixed = TRUE
  )
})

test_that("Gir feilmelding ved ugyldig type argument-verdi for 'entall'-,
          'flertall'- og 'nulltekst'-argumentene", {
  feilmelding_argument = "Argumentene 'entall', 'flertall' og 'nulltekst' må alle være av klasse character og av lengde 1"
  expect_error(
    entall_flertall(1, 1, "operasjoner"),
    feilmelding_argument
  )
  expect_error(
    entall_flertall(123, "operasjon", 123),
    feilmelding_argument
  )
  expect_error(
    entall_flertall(0, "nytt", "nye", factor(0)),
    feilmelding_argument
  )
  expect_error(
    entall_flertall(
      c(1, 5), c("sykepleier", "lege"),
      c("sykepleiere", "leger")
    ),
    feilmelding_argument
  )
})
