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


context("finn_ugyldige_verdier")

# Eksempeldata med bare gyldige tallverdier
d_gyldig_eks1 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, 1, 1, 10, 10,
  2, 2, 2, 20, 20,
  3, 1, 2, 20, 10
)

# Tom dataramme med spesifiserte variabeltyper
ugyldighetstabell_tom = tibble(
  radnr = integer(),
  variabel = character(),
  feilverdi = numeric()
)

test_that("finn_ugyldige_verdier() gir ut en dataramme (med kun kolonnenavn) hvis inndata er gyldig", {
  expect_identical(finn_ugyldige_verdier(d_gyldig_eks1, skaaringstabell_eks), ugyldighetstabell_tom)
})

# Eksempeldata med 1 feil
d_ugyldig_1_feil = d_gyldig_eks1
d_ugyldig_1_feil$fys1[1] = 13

# Ugyldighetstabell for d_ugyldig_1_feil
ugyldighetstabell_1_feil = tibble(
  radnr = 1L,
  variabel = "fys1",
  feilverdi = 13
)

# Eksempeldata med 3 feil, der 2 av de gjelder samme variabel
d_ugyldig_2_feil_samme_variabel = d_gyldig_eks1
d_ugyldig_2_feil_samme_variabel$gen[1] = 9
d_ugyldig_2_feil_samme_variabel$gen[3] = 6
d_ugyldig_2_feil_samme_variabel$psyk2[2] = NA

# Ugyldighetstabell for d_ugyldig_2_feil_samme_variabel
ugyldighetstabell_2_feil_samme_variabel = tibble(
  radnr = c(1L, 2L, 3L),
  variabel = c("gen", "psyk2", "gen"),
  feilverdi = c(9, NA, 6)
)

# Eksempeldata med 3 feil, der 2 av de er i samme rad
d_ugyldig_2_feil_samme_rad = d_gyldig_eks1
d_ugyldig_2_feil_samme_rad$gen[1] = 9
d_ugyldig_2_feil_samme_rad$fys1[2] = 10
d_ugyldig_2_feil_samme_rad$psyk2[1] = NA

# Ugyldighetstabell for d_ugyldig_2_feil_samme_rad
ugyldighetstabell_2_feil_samme_rad = tibble(
  radnr = c(1L, 1L, 2L),
  variabel = c("gen", "psyk2", "fys1"),
  feilverdi = c(9, NA, 10)
)

# Eksempeldata med 3 feil, der 2 av de er like og gjelder samme variabel
d_ugyldig_2_like_feil_samme_variabel = d_gyldig_eks1
d_ugyldig_2_like_feil_samme_variabel$gen[1] = 4
d_ugyldig_2_like_feil_samme_variabel$gen[3] = 4
d_ugyldig_2_like_feil_samme_variabel$psyk2[2] = NA

# Ugyldighetstabell for d_ugyldig_2_like_feil_samme_variabel
ugyldighetstabell_2_like_feil_samme_variabel = tibble(
  radnr = c(1L, 2L, 3L),
  variabel = c("gen", "psyk2", "gen"),
  feilverdi = c(4, NA, 4)
)

# Eksempeldata med bare 1 ugyldig NA-verdi
d_ugyldig_na_feil = d_gyldig_eks1
d_ugyldig_na_feil$fys1[1] = NA

# Ugyldighetstabell for d_ugyldig_na_feil
ugyldighetstabell_na_feil = tibble(
  radnr = 1L,
  variabel = "fys1",
  feilverdi = as.numeric(NA)
)

test_that("finn_ugyldige_verdier() gir ut korrekt feiloversikt hvis det finnes ugyldige verdier datasettet", {
  expect_identical(
    finn_ugyldige_verdier(d_ugyldig_1_feil, skaaringstabell_eks),
    ugyldighetstabell_1_feil
  )
  expect_identical(
    finn_ugyldige_verdier(d_ugyldig_2_feil_samme_variabel, skaaringstabell_eks),
    ugyldighetstabell_2_feil_samme_variabel
  )
  expect_identical(
    finn_ugyldige_verdier(d_ugyldig_2_feil_samme_rad, skaaringstabell_eks),
    ugyldighetstabell_2_feil_samme_rad
  )
  expect_identical(
    finn_ugyldige_verdier(d_ugyldig_2_like_feil_samme_variabel, skaaringstabell_eks),
    ugyldighetstabell_2_like_feil_samme_variabel
  )
  expect_identical(
    finn_ugyldige_verdier(d_ugyldig_na_feil, skaaringstabell_eks),
    ugyldighetstabell_na_feil
  )
})


context("oppsummer_ugyldige_verdier")

test_that("oppsummer_ugyldige_verdier() presenterer korrekte feilverdier alfabetisk etter variabelnavn", {
  expect_identical(
    oppsummer_ugyldige_verdier(ugyldighetstabell_2_feil_samme_variabel),
    "Fant 3 ugyldige verdier:\ngen: 9, 6\npsyk2: NA"
  )
  expect_identical(
    oppsummer_ugyldige_verdier(ugyldighetstabell_2_like_feil_samme_variabel),
    "Fant 3 ugyldige verdier:\ngen: 4, 4\npsyk2: NA"
  )
})

test_that("oppsummer_ugyldige_verdier() gir ut korrekt melding hvis det ikke finnes feilverdier", {
  expect_identical(
    oppsummer_ugyldige_verdier(ugyldighetstabell_tom),
    "Alle verdiene er gyldige"
  )
})


################################# Midlertidig kode ################################

# Eksempel på inndata med både gyldige tallverdier og gyldige NA-verdier
d_gyldig_eks2 = d_gyldig_eks1
d_gyldig_eks2$psyk1[3] = NA

# Eksempel på inndata med ugyldige NA-verdier (for "gen" og "fys1")
d_ugyldig_eks1 = d_gyldig_eks1
d_ugyldig_eks1$gen[3] = NA
d_ugyldig_eks1$fys1[1] = NA

# Eksempel på inndata med både ugyldige tallverdier (26 og 43) og ugyldige NA-verdier (for "gen" og "fys1")
d_ugyldig_eks2 = d_gyldig_eks1
d_ugyldig_eks2$gen[3] = NA
d_ugyldig_eks2$fys1[c(1, 3)] = c(NA, 26)
d_ugyldig_eks2$psyk1[c(2, 3)] = c(43, NA)

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

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med NA-verdiar viss godta_manglende = FALSE", {
  expect_error(sjekk_variabelverdier(d_ugyldig_eks1, skaaringstabell_eks, godta_manglende = FALSE))
})

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med ugyldige variabelverdiar", {
  expect_error(sjekk_variabelverdier(tibble(gen = c(200, 200)), skaaringstabell_eks, godta_manglende = FALSE))
  expect_error(sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla")), skaaringstabell_eks, godta_manglende = FALSE))
  expect_error(sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla", NA)), skaaringstabell_eks, godta_manglende = FALSE))
  expect_error(sjekk_variabelverdier(tibble(gen = c(200, 200, "blablabla", NA)), skaaringstabell_eks, godta_manglende = TRUE))
})
