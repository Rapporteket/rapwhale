test_that("Skal stoppa med feilmelding dersom inndata ikkje er gyldig", {
  d_vld_ugyldig = tibble(
    pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
    vld_verdi_intern_tal = 76, vld_verdi_ekstern_logisk = TRUE
  )
  expect_error(analyser_valideringsdatasett(d_vld_ugyldig))
})
