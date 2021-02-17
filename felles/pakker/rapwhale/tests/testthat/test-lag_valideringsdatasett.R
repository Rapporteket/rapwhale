d_reg = tibble::tribble(
  ~pasid, ~dato_inn, ~dato_ut, ~vekt, ~hogd, ~biverk, ~biverk_hovud, ~biverk_mage, ~biverk_fot,
  5, as.Date("2020-06-07"), as.Date("2020-06-15"), 78, 183, TRUE, FALSE, TRUE, TRUE,
  5, as.Date("2020-12-13"), as.Date("2020-12-13"), 50, 179, TRUE, FALSE, TRUE, TRUE,
  7, as.Date("2020-08-09"), as.Date("2020-08-13"), 711, 196, TRUE, TRUE, TRUE, TRUE,
  13, as.Date("2021-01-05"), NA, NA, 163, FALSE, NA, NA, NA,
  14, as.Date("2021-01-05"), as.Date("2021-01-09"), 101, 182, TRUE, TRUE, FALSE, FALSE
)

indvars = c("pasid", "dato_inn")

d_vld = lag_valideringsdatasett(d_reg, indvars)

test_that("Feilmelding viss inndata ikkje er data.frame/tibble", {
  expect_error(lag_valideringsdatasett(c(pasid = 4, vekt = 78), "pasid"))
  expect_error(lag_valideringsdatasett(as.list(d_reg), indvars))
})

test_that("Feilmelding viss indsvars ikkje finst i datasettet", {
  indvars_ugyldig = c("pasid", "forlopsid")
  expect_error(lag_valideringsdatasett(d_reg, indvars_ugyldig))
})

test_that("Feilmelding viss indeksvariabelvektoren har duplikatverdiar", {
  indvars_duplikat = c("pasid", "dato_inn", "pasid")
  expect_error(lag_valideringsdatasett(d_reg, indvars_duplikat))
})

test_that("Feilmelding viss ikkje indeksvariablane identifiserer alle radene unikt", {
  indvars_ikkje_unik = "pasid"
  expect_error(lag_valideringsdatasett(d_reg, indvars_ikkje_unik))
})

test_that("Gjev ut gyldig valideringsdatasett", {
  expect_true(er_valideringsdatasett_gyldig(d_vld))
})

test_that("Gjev ut datasett med rette kolonnar", {
  kolonnar_som_finst = names(d_vld)
  kolonnar_som_skal_finnast = c(
    "pasid", "dato_inn", "vld_varnamn", "vld_vartype",
    "vld_verdi_intern_Date", "vld_verdi_ekstern_Date",
    "vld_verdi_intern_numeric", "vld_verdi_ekstern_numeric",
    "vld_verdi_intern_logical", "vld_verdi_ekstern_logical"
  )
  expect_identical(kolonnar_som_finst, kolonnar_som_skal_finnast)

  # Også med spesifiserte vartypar
  vartypar = c("dato", "tal", "tal", "logisk", "logisk", "logisk", "logisk")
  d_vld_spes_vartypar = lag_valideringsdatasett(d_reg, indvars, vartypar)
  kolonnar_som_finst = names(d_vld_spes_vartypar)
  kolonnar_som_skal_finnast = c(
    "pasid", "dato_inn", "vld_varnamn", "vld_vartype",
    "vld_verdi_intern_dato", "vld_verdi_ekstern_dato",
    "vld_verdi_intern_tal", "vld_verdi_ekstern_tal",
    "vld_verdi_intern_logisk", "vld_verdi_ekstern_logisk"
  )
  expect_identical(kolonnar_som_finst, kolonnar_som_skal_finnast)
})

test_that("Gjev ut datasett med rette kolonnar viss inndata har 0 rader", {
  d_reg_tom = d_reg[c(), ]
  d_vld_tom = lag_valideringsdatasett(d_reg_tom, indvars)
  d_tom_kolonnar = tibble(
    pasid = numeric(),
    dato_inn = as.Date(NULL),
    vld_varnamn = character(),
    vld_vartype = character(),
    vld_verdi_intern_Date = as.Date(NULL),
    vld_verdi_ekstern_Date = as.Date(NULL),
    vld_verdi_intern_numeric = numeric(),
    vld_verdi_ekstern_numeric = numeric(),
    vld_verdi_intern_logical = logical(),
    vld_verdi_ekstern_logical = logical()
  )
  expect_identical(d_vld_tom, d_tom_kolonnar)
})

test_that("Rekkjefølgja på pasientforløpa er teke vare på", {
  expect_identical(
    distinct(d_reg, pasid, dato_inn),
    distinct(d_vld, pasid, dato_inn)
  )
})

test_that("Rekkjefølgja på datavariablane er teke vare på", {
  datavars = names(d_reg) %>%
    setdiff(indvars)
  varnamn = unique(d_vld$vld_varnamn)
  expect_identical(datavars, varnamn)
})


# Sjå på aktuelle testar/sjekkar i gamal funksjon i ekstern_validering.R
