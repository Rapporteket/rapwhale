# Eksempel på gyldig valideringsdatasett
d_vld = tibble(
  stringsAsFactors = FALSE,
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

test_that("Returnerer TRUE på gyldige valideringsdatasett", {
  expect_true(er_valideringsdatasett_gyldig(d_vld))
})
