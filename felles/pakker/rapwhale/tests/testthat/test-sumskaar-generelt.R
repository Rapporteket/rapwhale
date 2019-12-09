# Testing av generelle sumskårfunksjonar ----------------------------------

# Eksempel på gyldig skåringstabell
skaaringstabell_eks = tibble::tribble(
  ~delskala, ~variabel, ~verdi, ~koeffisient,
  "total", "gen", 1, 0.2,
  "total", "gen", 2, 0.4,
  "total", "gen", 3, 0.8,
  "total", "fys1", 1, 0,
  "total", "fys1", 2, 0.3,
  "total", "fys2", 1, 0,
  "total", "fys2", 2, 0.35,
  "total", "psyk1", NA, -0.01,
  "total", "psyk1", 10, 0,
  "total", "psyk1", 20, 0.025,
  "total", "psyk2", 10, 0,
  "total", "psyk2", 20, 0.018,
  "psykisk", "gen", 1, -2,
  "psykisk", "gen", 2, 0,
  "psykisk", "gen", 3, 2,
  "psykisk", "psyk1", NA, -0.5,
  "psykisk", "psyk1", 10, -5,
  "psykisk", "psyk1", 20, 5,
  "psykisk", "psyk2", 10, -8,
  "psykisk", "psyk2", 20, 8
)


context("sjekk_variabelnavn")

test_that("sjekk_variabelnavn() gjev inga feilmelding for gyldige datasett", {
  d = datasets::iris
  expect_silent(sjekk_variabelnavn(d, names(d)))
  expect_silent(sjekk_variabelnavn(d, c("Petal.Width", "Sepal.Width")))
  expect_silent(sjekk_variabelnavn(d, c("Petal.Width", "Sepal.Width", "Petal.Width")))
  expect_silent(sjekk_variabelnavn(d, character()))
})

test_that("sjekk_variabelnavn() gjev feilmelding viss variablar manglar", {
  d = datasets::iris
  feilmelding_ekstrakol = "^Mangler kolonner: ekstrakol$"
  feilmelding_ekstrakol_testkol = "^Mangler kolonner: ekstrakol, testkol$"
  feilmelding_testkol_ekstrakol = "^Mangler kolonner. testkol, ekstrakol$"
  expect_error(sjekk_variabelnavn(d, "ekstrakol"), feilmelding_ekstrakol)
  expect_error(sjekk_variabelnavn(d, c("Petal.Width", "ekstrakol", "Sepal.Width")), feilmelding_ekstrakol)
  expect_error(sjekk_variabelnavn(d, c("ekstrakol", "testkol")), feilmelding_ekstrakol_testkol)
  expect_error(sjekk_variabelnavn(d, c("ekstrakol", "testkol", "ekstrakol")), feilmelding_ekstrakol_testkol)
  expect_error(sjekk_variabelnavn(d, c("testkol", "ekstrakol")), feilmelding_testkol_ekstrakol)
})


context("sjekk_variabelverdier")

# Eksempel på inndata med bare gyldige tallverdier
d_gyldig_eks1 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, 1, 1, 10, 10,
  2, 2, 2, 20, 20,
  3, 1, 2, 20, 10
)

# Eksempel på inndata med både gyldige tallverdier og gyldige NA-verdier
d_gyldig_eks2 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, 1, 1, 10, 10,
  2, 2, 2, 20, 20,
  3, 1, 2, NA, 10
)

# Eksempel på inndata med ugyldige NA-verdier (for "gen" og "fys1")
d_ugyldig_eks1 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, NA, 1, 10, 10,
  2, 2, 2, 20, 20,
  NA, 1, 2, NA, 10
)

# Eksempel på inndata med både ugyldige tallverdier (26 og 43) og ugyldige NA-verdier (for "gen" og "fys1")
d_ugyldig_eks2 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, NA, 1, 10, 10,
  2, 2, 43, 20, 20,
  NA, 26, 2, NA, 10
)

# Eksempel på skåringstabell med ugyldige kolonnenavn
skaaringstabell_ugyldig_eks1 = dplyr::rename(skaaringstabell_eks, tullball = variabel, ballball = verdi)

# Eksempel på skåringstabell med ugyldig format
skaaringstabell_ugyldig_eks2 = table(skaaringstabell_eks)

# Eksempel på skåringstabell med både ugyldige kolonnenavn og ugyldig format
skaaringstabell_ugyldig_eks3 = table(skaaringstabell_ugyldig_eks1)

# Test at funksjonen gir feilmelding dersom de to aktuelle variablene ikke er i koblingstabellen og den ikke er en tibble
test_that("sjekk_variabelverdier() gir feilmelding dersom de to aktuelle variablene
          ('variabel' og 'verdi') ikke er i koblingstabellen og den ikke er en tibble/data.frame", {
  expect_error(
    sjekk_variabelverdier(d_gyldig_eks1, skaaringstabell_ugyldig_eks1),
    "Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'"
  )
  expect_error(
    sjekk_variabelverdier(d_gyldig_eks1, skaaringstabell_ugyldig_eks2),
    "Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'"
  )
  expect_error(
    sjekk_variabelverdier(d_gyldig_eks1, skaaringstabell_ugyldig_eks3),
    "Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'"
  )
})

test_that("sjekk_variabelverdier() gjev inga feilmelding for datasett med gyldige variabelverdiar", {
  expect_silent(sjekk_variabelverdier(d_gyldig_eks1, skaaringstabell_eks, godta_manglende = FALSE))
  expect_silent(sjekk_variabelverdier(d_gyldig_eks2, skaaringstabell_eks, godta_manglende = TRUE))
})

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med NA-verdiar viss godta_manglende = FALSE (", {
  expect_error(
    sjekk_variabelverdier(d_ugyldig_eks1, skaaringstabell_eks, godta_manglende = FALSE),
    "Ugyldige verdier: gen: NA, fys1: NA"
  )
})

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med ugyldige variabelverdiar", {
  feilmelding_ugyldig_verdi = "^Ugyldige verdier: gen: 200$"
  feilmelding_ugyldige_verdier = "^Ugyldige verdier: gen: 200, blablabla$"
  feilmelding_ugyldige_verdier_na = "^Ugyldige verdier: gen: 200, blablabla, NA$"
  expect_error(
    sjekk_variabelverdier(tibble(gen = c(200, 200)), skaaringstabell_eks, godta_manglende = FALSE),
    feilmelding_ugyldig_verdi
  )
  expect_error(
    sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla")), skaaringstabell_eks, godta_manglende = FALSE),
    feilmelding_ugyldige_verdier
  )
  expect_error(
    sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla", NA)), skaaringstabell_eks, godta_manglende = FALSE),
    feilmelding_ugyldige_verdier_na
  )
  expect_error(
    sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla", NA)), skaaringstabell_eks, godta_manglende = TRUE),
    feilmelding_ugyldige_verdier
  )
})
