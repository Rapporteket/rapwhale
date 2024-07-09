test_that("Gir feilmelding ved ingen inndata", {
  feilmelding_ingen_inndata = "Inndata «x» inneholder ingen verdier"
  expect_error(
    boy_enhet(NULL, "operasjon", "operasjoner"),
    feilmelding_ingen_inndata
  )
  expect_error(
    boy_enhet(numeric(), "operasjon", "operasjoner"),
    feilmelding_ingen_inndata
  )
})

test_that("Gir ut riktig resultat for enkelt-verdi inndata", {
  expect_identical(
    boy_enhet(0, "operasjon", "operasjoner"),
    "0 operasjoner"
  )
  expect_identical(
    boy_enhet(1, "operasjon", "operasjoner"),
    "1 operasjon"
  )
  expect_identical(
    boy_enhet(123, "operasjon", "operasjoner"),
    "123 operasjoner"
  )
  expect_identical(
    boy_enhet(0, "nytt", "nye", nullverdi = "ingen"),
    "ingen nye"
  )
  expect_identical(
    boy_enhet(1L, "nytt", "nye", nullverdi = "ingen"),
    "1 nytt"
  )
})

test_that("Gir ut riktig resultat for ikke-standard formateringsfunksjon", {
  expect_identical(
    boy_enhet(-1.23, "million", "millioner", round, digits = 1),
    "-1.2 millioner"
  )
  expect_identical(
    boy_enhet(1, "pasient", "pasienter", \(x) ifelse(x == 1, "én", x)),
    "én pasient"
  )
})

test_that("Gir ut riktig resultat for vektor-verdi inndata", {
  expect_identical(
    boy_enhet(c(1, 123, 0), "operasjon", "operasjoner"),
    c("1 operasjon", "123 operasjoner", "0 operasjoner")
  )
  expect_identical(
    boy_enhet(c(1, 123, 0), "nytt", "nye", nullverdi = "ingen"),
    c("1 nytt", "123 nye", "ingen nye")
  )
})

test_that(
  "Gir ut riktig resultat for inndata som inneholder desimaltall",
  expect_identical(
    boy_enhet(
      c(1.1, 4.0, 1.0, 1.001, 1000.99999),
      "million", "millioner"
    ),
    c(
      "1.1 millioner", "4 millioner", "1 million", "1.001 millioner",
      "1000.99999 millioner"
    )
  )
)

test_that("Gir feilmelding ved inndata av NA-verdi", {
  feilmelding_na_verdi = "Inndata «x» inneholder minst én NA-verdi"
  expect_error(
    boy_enhet(NA_integer_, "operasjon", "operasjoner"),
    feilmelding_na_verdi
  )
  expect_error(
    boy_enhet(c(1, NA), "operasjon", "operasjoner"),
    feilmelding_na_verdi
  )
})

test_that("Gir feilmelding ved ugyldig type inndata", {
  feilmelding_inndata = "Inndata «x» må være tall, men er:"
  expect_error(boy_enhet("en", "operasjon", "operasjoner"),
    paste(feilmelding_inndata, class("en")),
    fixed = TRUE
  )
  expect_error(boy_enhet(factor(1), "operasjon", "operasjoner"),
    paste(feilmelding_inndata, class(factor(1))),
    fixed = TRUE
  )
  expect_error(boy_enhet(Sys.Date(), "dato", "datoer"),
    paste(feilmelding_inndata, class(Sys.Date())),
    fixed = TRUE
  )
})

test_that("Gir feilmelding hvis utdata fra formatering() ikke har rett lengde", {
  expect_error(
    object = boy_enhet(1:3,
      entall = "operasjon",
      flertall = "operasjoner",
      formatering = \(x) x[1]
    ),
    regexp = paste0(
      "Utdata fra formatering() må ha samme lengde som «x» ",
      "(har hhv. lengde 1 og 3)"
    ),
    fixed = TRUE
  )
})

test_that("Gir feilmelding ved ugyldig type argument-verdi for 'entall'- og
          'flertall'-argumentene", {
  feilmelding_argument = paste0(
    "Argumentene «entall» og «flertall» må begge være av ",
    "klasse «character» og lengde 1"
  )
  expect_error(boy_enhet(1, 1, "operasjoner"), feilmelding_argument)
  expect_error(boy_enhet(123, "operasjon", 123), feilmelding_argument)
  expect_error(
    object = boy_enhet(c(1, 5),
      entall = c("sykepleier", "lege"),
      flertall = c("sykepleiere", "leger")
    ),
    regexp = feilmelding_argument
  )
})
