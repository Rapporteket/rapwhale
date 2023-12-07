# Eksempel på gyldig valideringsdatasett
d_vld_gyldig = tibble(
  pasid = c(5, 5, 5, 7, 7, 13, 13, 14),
  dato_inn = as.Date(c(
    "2020-06-07",
    "2020-06-07", "2020-12-13", "2020-08-09",
    "2020-08-09", "2021-01-05", "2021-01-05", "2021-01-05"
  )),
  kjonn = c("M", "M", "M", "K", "K", "M", "M", "M"),
  sjukehus = c(
    "Bergen",
    "Bergen", "Førde", "Bergen", "Bergen", "Førde",
    "Førde", "Førde"
  ),
  vld_varnamn = c(
    "vekt",
    "diabetes", "vekt", "dato_ut", "vekt", "vekt",
    "diabetes", "hogd"
  ),
  vld_vartype = c(
    "tal", "logisk",
    "tal", "dato", "tal", "tal", "logisk", "tal"
  ),
  vld_verdi_intern_tal = c(78, NA, NA, NA, 711, NA, NA, 182),
  vld_verdi_ekstern_tal = c(78, NA, 82, NA, 71, NA, NA, NA),
  vld_verdi_intern_logisk = c(NA, FALSE, NA, NA, NA, NA, TRUE, NA),
  vld_verdi_ekstern_logisk = c(NA, TRUE, NA, NA, NA, NA, TRUE, NA),
  vld_verdi_intern_dato = as.Date(c(NA, NA, NA, "2020-08-13", NA, NA, NA, NA)),
  vld_verdi_ekstern_dato = as.Date(c(NA, NA, NA, "2020-08-13", NA, NA, NA, NA))
)

test_that("Skal stoppa med feilmelding dersom inndata ikkje er gyldig", {
  d_vld_ugyldig = tibble(
    pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
    vld_verdi_intern_tal = 76, vld_verdi_ekstern_logisk = TRUE
  )
  feilmelding_ugyldig = "Datasettet er ikkje på rett format"

  expect_error(analyser_valideringsdatasett(d_vld_ugyldig), feilmelding_ugyldig)
})

test_that("Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane
          i kvar rad er «like»", {
  expect_identical(
    analyser_valideringsdatasett(d_vld_gyldig),
    bind_cols(d_vld_gyldig,
      ki_krit_teller = c(
        TRUE, FALSE, FALSE, TRUE,
        FALSE, TRUE, TRUE, FALSE
      ),
      ki_krit_nevner = TRUE
    )
  )
})

test_that("Gjev ut valideringsdatasettet med rett ekstrakolonne også om inndata har 0 rader", {
  d_vld_tom = d_vld_gyldig[c(), ]
  expect_identical(
    analyser_valideringsdatasett(d_vld_tom),
    bind_cols(d_vld_tom, ki_krit_teller = logical(), ki_krit_nevner = logical())
  )
})

test_that("Skal stoppa med feilmelding dersom samanliknaren gjev ut NA-verdiar", {
  samanliknar_lag_na = function(verdi1, verdi2, varnamn) {
    return(rep(NA, length(verdi1)))
  }
  feilmelding_na = "NA-verdiar frå samanliknaren"

  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_lag_na),
    feilmelding_na
  )
})

test_that("Skal stoppa med feilmelding dersom utdata frå samanliknaren har feil lengd", {
  samanliknar_feil_lengd = function(verdi1, verdi2, varnamn) {
    return(c(samanlikn_identisk(verdi1, verdi2, varnamn), TRUE))
  }
  samanliknar_feil_lengd_kort = function(verdi1, verdi2, varnamn) {
    return(head(samanlikn_identisk(verdi1, verdi2, varnamn), -1))
  }
  samanliknar_feil_lengd_null = function(verdi1, verdi2, varnamn) {
    return(logical())
  }

  feilmelding_lengd = "Utdata frå samanliknaren har feil lengd"

  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd),
    feilmelding_lengd
  )
  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd_kort),
    feilmelding_lengd
  )
  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd_null),
    feilmelding_lengd
  )
})

test_that("Skal stoppa med feilmelding dersom utdata frå samanliknaren ikkje er logisk vektor", {
  samanliknar_feil_type = function(verdi1, verdi2, varnamn) {
    return(as.character(samanlikn_identisk(verdi1, verdi2, varnamn)))
  }

  feilmelding_type = "Ikkje logisk vektor frå samanliknaren"

  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_type),
    feilmelding_type
  )
})

test_that("Skal fungera med grupperte inndata, og utdata skal bevara grupperinga", {
  d_vld_gruppert = d_vld_gyldig |>
    group_by(kjonn, sjukehus)
  d_vld_gruppert_resultat = bind_cols(d_vld_gruppert,
    ki_krit_teller = c(
      TRUE, FALSE, FALSE, TRUE,
      FALSE, TRUE, TRUE, FALSE
    ),
    ki_krit_nevner = TRUE
  )

  expect_identical(analyser_valideringsdatasett(d_vld_gruppert), d_vld_gruppert_resultat)
  expect_identical(
    groups(analyser_valideringsdatasett(d_vld_gruppert)),
    groups(d_vld_gruppert)
  )
})
