context("er_syntaktisk_fnr()")

test_that("er_syntaktisk_fnr() gjev forventa resultat", {
  nummer = c(
    "123456789", "123456789ab", "12345612345", "123456123456",
    "abcdefghijk"
  )
  forventa = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  expect_identical(er_syntaktisk_fnr(nummer), forventa)
})



context("er_gyldig_fnr_dato()")

test_that("er_gyldig_fnr_dato() gjev forventa resultat", {
  datoar = c(
    "010101", "170527", "311299",
    "290200", # Skotårsdag, gyldig i år 2000, men ikkje i 1900 ...
    "320101", "011382", "999999",
    "290225", # «Skotårsdag» som aldri er gyldig ...
    "241224"
  )
  dato_gyldig = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  expect_identical(er_gyldig_fnr_dato(datoar), dato_gyldig)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_fnr_dato("010101"), TRUE)
  expect_identical(er_gyldig_fnr_dato("320101"), FALSE)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_fnr_dato(character()), logical())
})




context("er_gyldig_f_nummer()")

# Dei gyldige fødselsnummera er ikkje reelle,
# men henta frå dokumentet «Testaktører»
# versjon 3.7, datert 19.11.2013.

test_that("er_gyldig_f_nummer() gjev forventa resultat", {
  fnr_gyldige = c(
    "15076500565", "21016400952",
    "12057900499", "13116900216",
    "14019800513", "05073500186"
  )
  fnr_ugyldige = c(
    "15076500561", "21016400992",
    "13057900499", "13126900216",
    "98019800547"
  ) # Siste nummer har gyldige sjekksiffer
  expect_identical(
    er_gyldig_f_nummer(fnr_gyldige),
    rep(TRUE, length(fnr_gyldige))
  )
  expect_identical(
    er_gyldig_f_nummer(fnr_ugyldige),
    rep(FALSE, length(fnr_ugyldige))
  )
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 1", {
  expect_identical(er_gyldig_f_nummer("15076500565"), TRUE)
  expect_identical(er_gyldig_f_nummer("15076500561"), FALSE)
})

test_that("er_gyldig_f_nummer() fungerer òg med vektorar av lengd 0", {
  expect_identical(er_gyldig_f_nummer(character()), logical())
})



context("er_fnr_sjekksum_korrekt()")

test_that("er_fnr_sjekksum_korrekt() gjev forventa resultat", {
  nummer_gyldige = c("12345678911", "12345678922", "15076500565")
  nummer_ugyldige = c("12345678922", "15076500511")
  expect_identical(
    er_fnr_sjekksum_korrekt(nummer_gyldige),
    rep(TRUE, length(nummer_gyldige))
  )
  expect_identical(
    er_fnr_sjekksum_korrekt(nummer_ugyldige),
    rep(FALSE, length(nummer_ugyldige))
  )
})
