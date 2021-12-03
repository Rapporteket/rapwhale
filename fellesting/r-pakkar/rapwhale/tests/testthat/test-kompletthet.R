
context("Erstatt_ukjent")

# Baseobjekt for testdata
data_inn = tibble::tibble(
  pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
  sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
  var_1 = c(1L, NA_integer_, NA_integer_, -1L, 2L, 99L),
  var_2 = c(1L, 2L, 3L, NA_integer_, -1L, -1L)
)

# Inndata -----------------------------------------------------------------
test_that("Feilmelding hvis 'data' ikke inneholder nødvendige kolonner", {
  data_uten_var = data_inn %>%
    select(-var_1)
  feilmelding = "'var_1' mangler i inndata."
  expect_error(
    erstatt_ukjent(data_uten_var, variabel = "var_1", na_vektor = -1L),
    feilmelding
  )
})
test_that("Feilmelding hvis na_vektor er tom", {
  expect_error(erstatt_ukjent(data_inn, variabel = "var_1"))
})

test_that("Feilmelding hvis na_vektor og variabel har ulik datatype", {
  feilmelding = "'var_1' og na_vektor må være av samme datatype"
  expect_error(
    erstatt_ukjent(data_inn, variabel = "var_1", na_vektor = "Ukjent"),
    feilmelding
  )
})

test_that("Feilmelding hvis 'variabel' ikke er en streng", {
  expect_error(erstatt_ukjent(data_inn, variabel = var_1, na_vektor = -1))
})

# Utdata ------------------------------------------------------------------

test_that("Fungerer uavhengig av hvilken datatype 'variabel' er", {
  data_med_andre_vartyper = data_inn %>%
    tibble::add_column(
      var_tekst = c("glad", "sliten", "sulten", "ikke svart", "ikke svart", NA),
      var_desimal = c(0.2, 0.1, -1.0, NA, 1.0, 99.0),
      var_heltall = c(1L, 3L, 6L, -1L, 99L, 9L)
    )

  data_ut_var_tekst = data_med_andre_vartyper %>%
    select(-var_tekst) %>%
    tibble::add_column(
      var_tekst = c("glad", "sliten", "sulten", NA_character_, NA_character_, NA_character_),
      .before = 5
    )

  data_ut_var_desimal = data_med_andre_vartyper %>%
    select(-var_desimal) %>%
    tibble::add_column(
      var_desimal = c(0.2, 0.1, NA_real_, NA_real_, 1.0, NA_real_),
      .before = 6
    )

  data_ut_var_heltall = data_med_andre_vartyper %>%
    select(-var_heltall) %>%
    tibble::add_column(var_heltall = c(1L, 3L, 6L, NA_integer_, NA_integer_, 9L))

  expect_identical(
    erstatt_ukjent(data_med_andre_vartyper,
      variabel = "var_tekst",
      na_vektor = "ikke svart"
    ),
    data_ut_var_tekst
  )

  expect_identical(
    erstatt_ukjent(data_med_andre_vartyper,
      variabel = "var_desimal",
      na_vektor = c(-1.0, 99.0)
    ),
    data_ut_var_desimal
  )

  expect_identical(
    erstatt_ukjent(data_med_andre_vartyper,
      variabel = "var_heltall",
      na_vektor = c(99L, -1L)
    ),
    data_ut_var_heltall
  )
})

test_that("Fungerer med grupperte inndata og ugrupperte inndata", {
  data_inn_gruppert = data_inn %>%
    group_by(sykehus)

  data_ut_gruppert = tibble::tibble(
    pas_id = c(1L:6L),
    sykehus = c(
      "HUS", "HUS", "SVG",
      "SVG", "SVG", "OUS"
    ),
    var_1 = c(
      1L, NA_integer_, NA_integer_,
      NA_integer_, 2L, NA_integer_
    ),
    var_2 = c(1L, 2L, 3L, NA_integer_, -1L, -1L)
  ) %>%
    group_by(sykehus)

  expect_identical(
    erstatt_ukjent(data_inn_gruppert,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_ut_gruppert
  )
})
test_that("Konverterer flere verdier hvis det er oppgitt i na_vektor", {
  data_ut = data_inn %>%
    select(-var_1) %>%
    tibble::add_column(var_1 = c(
      1L, NA_integer_, NA_integer_,
      NA_integer_, 2L, NA_integer_
    ), .before = 3)

  expect_identical(
    erstatt_ukjent(data_inn,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_ut
  )
})
test_that("Gjør ingenting hvis inndata mangler verdier fra na_vektor", {
  expect_identical(
    erstatt_ukjent(data_inn,
      variabel = "var_1",
      na_vektor = 759L
    ),
    data_inn
  )
})
