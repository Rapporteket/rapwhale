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
  expect_error(analyser_valideringsdatasett(d_vld_ugyldig))
})

test_that("Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane
          i kvar rad er «like»", {
  expect_identical(
    analyser_valideringsdatasett(d_vld_gyldig),
    bind_cols(d_vld_gyldig,
      vld_verdiar_er_like = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
    )
  )
})

test_that("Skal stoppa med feilmelding dersom samanliknaren gjev ut NA-verdiar", {
  samanliknar_lag_NA = function(varnamn, verdi1, verdi2) {
    return(rep(NA, length(verdi1)))
  }
  expect_error(analyser_valideringsdatasett(d_vld_gyldig, samanliknar_lag_NA))
})

test_that("Skal stoppa med feilmelding dersom utdata frå samanliknaren har feil lengd", {
  samanliknar_feil_lengd = function(varnamn, verdi1, verdi2) {
    return(c(samanlikn_identisk(varnamn, verdi1, verdi2), TRUE))
  }
  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd),
    "Utdata frå samanliknaren har feil lengd"
  )

  samanliknar_feil_lengd_kort = function(varnamn, verdi1, verdi2) {
    return(head(samanlikn_identisk(varnamn, verdi1, verdi2), -1))
  }
  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd_kort),
    "Utdata frå samanliknaren har feil lengd"
  )

  samanliknar_feil_lengd_null = function(varnamn, verdi1, verdi2) {
    return(logical())
  }
  expect_error(
    analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_lengd_null),
    "Utdata frå samanliknaren har feil lengd"
  )
})

test_that("Skal stoppa med feilmelding dersom utdata frå samanliknaren ikkje er logisk vektor", {
  samanliknar_feil_type = function(varnamn, verdi1, verdi2) {
    return(as.character(samanlikn_identisk(varnamn, verdi1, verdi2)))
  }
  expect_error(analyser_valideringsdatasett(d_vld_gyldig, samanliknar_feil_type))
})

test_that("Skal fungera med grupperte inndata, og utdata skal bevara grupperinga", {
  d_vld_gruppert = d_vld_gyldig %>%
    group_by(kjonn, sjukehus)
  d_vld_gruppert_resultat = bind_cols(d_vld_gruppert,
    vld_verdiar_er_like = c(
      TRUE, FALSE, FALSE, TRUE,
      FALSE, TRUE, TRUE, FALSE
    )
  )

  expect_identical(analyser_valideringsdatasett(d_vld_gruppert), d_vld_gruppert_resultat)
  expect_identical(
    groups(analyser_valideringsdatasett(d_vld_gruppert)),
    groups(d_vld_gruppert)
  )
})
