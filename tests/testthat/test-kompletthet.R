# Erstatt_ukjent ----------------------------------------------------------

# Baseobjekter for testdata
data_inn = tibble(
  pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
  sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
  var_1 = c(1L, NA_integer_, NA_integer_, -1L, 2L, 99L),
  var_2 = c(1L, 2L, 3L, NA_integer_, -1L, -1L)
)

data_inn_gruppert = group_by(data_inn, sykehus)



# Inndata -----------------------------------------------------------------
test_that("Feilmelding hvis 'data' ikke inneholder nødvendige kolonner", {
  data_uten_var = select(data_inn, -var_1)
  feilmelding = "'var_1' mangler i inndata"
  expect_error(
    erstatt_ukjent(data_uten_var, variabel = "var_1", na_vektor = -1L),
    feilmelding
  )
})
test_that("Feilmelding hvis na_vektor er tom", {
  expect_error(erstatt_ukjent(data_inn, variabel = "var_1"))
})

test_that("Feilmelding hvis 'variabel' ikke er en streng", {
  expect_error(erstatt_ukjent(data_inn, variabel = var_1, na_vektor = -1))
})

# Utdata ------------------------------------------------------------------

test_that("Fungerer uavhengig av hvilken datatype 'variabel' er", {
  data_med_andre_vartyper = data_inn |>
    add_column(
      var_tekst = c("glad", "sliten", "sulten", "ikke svart", "ikke svart", NA),
      var_desimal = c(0.2, 0.1, -1.0, NA, 1.0, 99.0),
      var_heltall = c(1L, 3L, 6L, -1L, 99L, 9L)
    )

  data_ut_var_tekst = data_med_andre_vartyper |>
    select(-var_tekst) |>
    add_column(
      var_tekst = c("glad", "sliten", "sulten", NA_character_, NA_character_, NA_character_),
      .before = 5
    )

  data_ut_var_desimal = data_med_andre_vartyper |>
    select(-var_desimal) |>
    add_column(
      var_desimal = c(0.2, 0.1, NA_real_, NA_real_, 1.0, NA_real_),
      .before = 6
    )

  data_ut_var_heltall = data_med_andre_vartyper |>
    select(-var_heltall) |>
    add_column(var_heltall = c(1L, 3L, 6L, NA_integer_, NA_integer_, 9L))

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
  data_ut_ugruppert = tibble(
    pas_id = 1L:6L,
    sykehus = c(
      "HUS", "HUS", "SVG",
      "SVG", "SVG", "OUS"
    ),
    var_1 = c(
      1L, NA_integer_, NA_integer_,
      NA_integer_, 2L, NA_integer_
    ),
    var_2 = c(1L, 2L, 3L, NA_integer_, -1L, -1L)
  )

  data_ut_gruppert = tibble(
    pas_id = 1L:6L,
    sykehus = c(
      "HUS", "HUS", "SVG",
      "SVG", "SVG", "OUS"
    ),
    var_1 = c(
      1L, NA_integer_, NA_integer_,
      NA_integer_, 2L, NA_integer_
    ),
    var_2 = c(1L, 2L, 3L, NA_integer_, -1L, -1L)
  ) |>
    group_by(sykehus)


  expect_identical(
    erstatt_ukjent(data_inn,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_ut_ugruppert
  )
  expect_identical(
    erstatt_ukjent(data_inn_gruppert,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_ut_gruppert
  )
})

test_that("Konverterer flere verdier hvis det er oppgitt i na_vektor", {
  data_ut = data_inn |>
    select(-var_1) |>
    add_column(var_1 = c(
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


# Beregn_kompletthet ------------------------------------------------------

test_that("Feilmelding hvis 'data' ikke inneholder nødvendige kolonner", {
  data_uten_var = select(data_inn, -var_1)
  feilmelding = "'var_1' mangler i inndata"
  expect_error(
    beregn_kompletthet(data = data_uten_var, variabel = "var_1"),
    feilmelding
  )
})

test_that("Gir riktig resultat for antall og andel missing for ugrupperte data", {
  data_forventet_ut_ugruppert =
    tibble(
      variabel = "var_1",
      totalt_antall = 6L,
      antall_na = 2L,
      andel_na = 2 / 6
    )

  expect_identical(
    beregn_kompletthet(data_inn, "var_1"),
    data_forventet_ut_ugruppert
  )
})

test_that("Gir forventet utdata med grupperte data", {
  data_forventet_ut_gruppert = tibble(
    sykehus = c("SVG", "HUS", "OUS"),
    variabel = c(rep("var_1", 3)),
    totalt_antall = c(3L, 2L, 1L),
    antall_na = c(1L, 1L, 0L),
    andel_na = c(1 / 3, 1 / 2, 0 / 2)
  )

  expect_identical(
    beregn_kompletthet(data = data_inn_gruppert, variabel = "var_1"),
    data_forventet_ut_gruppert
  )
})

test_that("Gir feilmelding hvis variabel ikke er tekststreng", {
  expect_error(beregn_kompletthet(data_inn, variabel = var_1))
})


# Beregn_kompletthet_med_ukjent -------------------------------------------

test_that("Feilmelding hvis 'data' ikke inneholder nødvendige kolonner", {
  data_uten_var_1 = select(data_inn, -var_1)
  feilmelding = "'var_1' mangler i inndata"

  expect_error(
    beregn_kompletthet_med_ukjent(
      data = data_uten_var_1,
      variabel = "var_1",
      na_vektor = c(-1)
    ),
    feilmelding
  )
})
test_that("Feilmelding hvis na_vektor er tom", {
  expect_error(beregn_kompletthet_med_ukjent(
    data = data_inn,
    variabel = "var_1"
  ))
})

test_that("Gir riktig resultat for antall og andel missing", {
  data_forventet_ut_med_ukjent_ugruppert = tibble(
    variabel = "var_1",
    totalt_antall = 6L,
    antall_na = 2L,
    andel_na = 2 / 6,
    antall_na_med_ukjent = 4L,
    andel_na_med_ukjent = 4 / 6
  )

  expect_identical(
    beregn_kompletthet_med_ukjent(
      data = data_inn,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_forventet_ut_med_ukjent_ugruppert
  )
})

test_that("Gir forventet utdata med grupperte data", {
  data_forventet_ut_med_ukjent_gruppert = tibble(
    sykehus = c("SVG", "HUS", "OUS"),
    variabel = c(rep("var_1", 3)),
    totalt_antall = c(3L, 2L, 1L),
    antall_na = c(1L, 1L, 0L),
    andel_na = c(1 / 3, 1 / 2, 0 / 2),
    antall_na_med_ukjent = c(2L, 1L, 1L),
    andel_na_med_ukjent = c(2 / 3, 1 / 2, 1 / 1)
  )

  expect_identical(
    beregn_kompletthet_med_ukjent(
      data = data_inn_gruppert,
      variabel = "var_1",
      na_vektor = c(-1L, 99L)
    ),
    data_forventet_ut_med_ukjent_gruppert
  )
})

test_that("Gir feilmelding hvis variabel ikke er tekststreng", {
  expect_error(beregn_kompletthet_med_ukjent(data_inn,
    variabel = var_1,
    na_vektor = c(-1L, 99L)
  ))
})

# beregn_kompletthet_datasett ---------------------------------------------

d_test = tibble(
  pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
  sykehus = c("HUS", "HUS", "SUS", "SUS", "SUS", "OUS"),
  vekt = c(60L, NA_integer_, 100L, NA_integer_, 99L, -1L),
  vekt_2 = c(55L, NA_integer_, 99L, -1L, NA_integer_, 50L),
  hoyde = c(1.52, NA_real_, 1.89, 2.15, NA_real_, 99.9),
  symptom = c("svett", "klam", NA_character_, "trøtt", "vet ikke", "Ukjent"),
  test_logisk = c(TRUE, FALSE, NA, NA, FALSE, TRUE)
)

ukjent_datasett = tibble(
  variabel = c(
    "pas_id", "sykehus", rep("vekt", 2), rep("vekt_2", 2),
    "hoyde", rep("symptom", 2), "test_logisk"
  ),
  ukjent_verdi_integer = c(
    NA_integer_, NA_integer_, 99, -1, 99, -1,
    NA_integer_, NA_integer_, NA_integer_, NA_integer_
  ),
  ukjent_verdi_real = c(
    NA_real_, NA_real_, NA_real_, NA_real_, NA_real_,
    NA_real_, 99.9, NA_real_, NA_real_, NA_real_
  ),
  ukjent_verdi_tekst = c(
    NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
    NA_character_, NA_character_, "vet ikke", "Ukjent", NA_character_
  )
)


d_test_ut_uten_ukjent = tibble(
  variabel = c("pas_id", "sykehus", "vekt", "vekt_2", "hoyde", "symptom", "test_logisk"),
  totalt_antall = c(rep(6L, 7L)),
  antall_na = c(0L, 0L, 2L, 2L, 2L, 1L, 2L),
  andel_na = c(0L / 6L, 0L / 6L, 2L / 6L, 2L / 6L, 2L / 6L, 1L / 6L, 2L / 6L)
)

d_test_ut_med_ukjent = tibble(
  variabel = c("pas_id", "sykehus", "vekt", "vekt_2", "hoyde", "symptom", "test_logisk"),
  totalt_antall = c(rep(6L, 7L)),
  antall_na = c(0L, 0L, 2L, 2L, 2L, 1L, 2L),
  andel_na = c(0L / 6L, 0L / 6L, 2L / 6L, 2L / 6L, 2L / 6L, 1L / 6L, 2L / 6L),
  antall_na_med_ukjent = c(0L, 0L, 4L, 4L, 3L, 3L, 2L),
  andel_na_med_ukjent = c(0L / 6L, 0L / 6L, 4L / 6L, 4L / 6L, 3L / 6L, 3L / 6L, 2L / 6L)
)

test_that("Returnerer forventet resultat", {
  expect_identical(
    beregn_kompletthet_datasett(data = d_test),
    d_test_ut_uten_ukjent
  )
})

# beregn_kompletthet_datasett_med_ukjent ----------------------------------
test_that("Returnerer forventet resultat", {
  expect_identical(
    beregn_kompletthet_datasett_med_ukjent(data = d_test, ukjent_datasett = ukjent_datasett),
    d_test_ut_med_ukjent
  )
})
