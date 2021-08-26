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
