d_reg = tibble::tribble(
  ~pasid, ~dato_inn, ~dato_ut, ~vekt, ~hogd, ~biverk, ~biverk_hovud, ~biverk_mage, ~biverk_fot,
  5, as.Date("2020-06-07"), as.Date("2020-06-15"), 78, 183, TRUE, FALSE, TRUE, TRUE,
  5, as.Date("2020-12-13"), as.Date("2020-12-13"), 50, 179, TRUE, FALSE, TRUE, TRUE,
  7, as.Date("2020-08-09"), as.Date("2020-08-13"), 711, 196, TRUE, TRUE, TRUE, TRUE,
  13, as.Date("2021-01-05"), NA, NA, 163, FALSE, NA, NA, NA,
  14, as.Date("2021-01-05"), as.Date("2021-01-09"), 101, 182, TRUE, TRUE, FALSE, FALSE
)

indvars = c("pasid", "dato_inn")

test_that("Feilmelding viss indsvars ikkje finst i datasettet", {
  indvars_ugyldig = c("pasid", "forlopsid")
  expect_error(lag_valideringsdatasett(d_reg, indvars_ugyldig))
})

test_that("Rekkjefølgja på pasientforløpa er teke vare på", {
  d_vld = lag_valideringsdatasett(d_reg, indvars)
  expect_identical(
    distinct(d_reg, pasid, dato_inn),
    distinct(d_vld, pasid, dato_inn)
  )
})

test_that("Rekkjefølgja på datavariablane er teke vare på", {
  datavars = names(d_reg) %>%
    setdiff(indvars)
  d_vld = lag_valideringsdatasett(d_reg, indvars)
  varnamn = unique(d_vld$vld_varnamn)
  expect_identical(datavars, varnamn)
})


# Sjå på aktuelle testar/sjekkar i gamal funksjon i ekstern_validering.R
