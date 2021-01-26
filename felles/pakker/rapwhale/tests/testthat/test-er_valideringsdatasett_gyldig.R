# Eksempel på gyldig valideringsdatasett
d_vld = tibble(
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
  expect_true(er_valideringsdatasett_gyldig(as.data.frame(d_vld)))
})

test_that("Valideringsdatasett med 0 rader vert rekna som gyldig (viss resten er gyldig)", {
  expect_true(er_valideringsdatasett_gyldig(d_vld[c(), ]))
})

test_that("Inndata som ikkje er data.frame/tibble, vert rekna som ugyldig", {
  expect_false(er_valideringsdatasett_gyldig(4))
  expect_false(er_valideringsdatasett_gyldig(environment()))
})

test_that("Inndata som ikkje har tekstkolonnar «vld_varnamn» og «vld_vartype», vert rekna som ugyldig", {
  expect_false(er_valideringsdatasett_gyldig(select(d_vld, -vld_varnamn)))
  expect_false(er_valideringsdatasett_gyldig(select(d_vld, -vld_vartype)))
  expect_false(er_valideringsdatasett_gyldig(mutate(d_vld, vld_varnamn = 1:nrow(d_vld))))
  expect_false(er_valideringsdatasett_gyldig(mutate(d_vld, vld_vartype = 1:nrow(d_vld))))
})

test_that("Gyldige inndata vert rekna som gyldige sjølv om «vld_varnamn» og «vld_vartype»
          òg har attributt eller andre klassar i tillegg til «character»", {
  # Realistisk eksempel på data me kan få frå SPSS (med haven::read_spss())
  d_vld_med_label = d_vld
  class(d_vld_med_label$vld_varnamn) = c("haven_labelled", "vctrs_vctr", "character")
  attr(d_vld_med_label$vld_varnamn, "label") = "Variabelnamn"
  class(d_vld_med_label$vld_vartype) = c("haven_labelled", "vctrs_vctr", "character")
  attr(d_vld_med_label$vld_vartype, "label") = "Variabeltype"
  attr(d_vld_med_label$vld_vartype, "labels") = c(
    "Heiltal og desimtal" = "tal",
    "Datoar" = "dato",
    "Boolske verdiar" = "logisk"
  )

  expect_true(er_valideringsdatasett_gyldig(d_vld_med_label))
})

test_that("Datasett med NA-verdiar eller tomme tekststrengar i vld_varnamn eller
          vld_vartype vert rekna som ugyldig", {
  d_vld_ugyldig = d_vld
  d_vld_ugyldig$vld_varnamn[3] = NA
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
  d_vld_ugyldig$vld_varnamn[3] = ""
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))

  d_vld_ugyldig = d_vld
  d_vld_ugyldig$vld_vartype[3] = NA
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
  d_vld_ugyldig$vld_vartype[3] = ""
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
})

test_that("vld_verdi_intern_x og vld_verdi_ekstern_x med ulik klasse vert rekna
          som ugyldig", {
  d_vld_ugyldig = d_vld
  d_vld_ugyldig$vld_verdi_intern_tal = as.character(d_vld_ugyldig$vld_verdi_intern_tal)
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))

  # Viss ein kolonne har *fleire* klassar, må motsvarande
  # òg ha same klassehierarki
  d_vld_ugyldig = d_vld
  class(d_vld_ugyldig$vld_verdi_intern_tal) = c("foo", "numeric")
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
})

test_that("vld_verdi_intern_x og vld_verdi_ekstern_x som har likt klassehierarki (men med meir enn eitt nivå) vert rekna som like", {
  d_vld_fleire_klassar = d_vld
  class(d_vld_fleire_klassar$vld_verdi_intern_tal) = c("foo", "numeric")
  class(d_vld_fleire_klassar$vld_verdi_ekstern_tal) = c("foo", "numeric")
  expect_true(er_valideringsdatasett_gyldig(d_vld_fleire_klassar))
})

test_that("Datasett med NA-verdiar i primærnøkkel vert rekna som gyldig (viss
          resten er gyldig", {
  d_vld$dato_inn[3] = NA
  expect_true(er_valideringsdatasett_gyldig(d_vld))
})

test_that("Kolonnar med namn som vld_tull skal ikkje vera lov (vld_ er reservert prefiks)", {
  d_vld_ugyldig = d_vld
  names(d_vld_ugyldig)[4] = "vld_sjukehus"
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
})

test_that("Viss vld_verdi_intern_x finst, finst også vld_verdi_ekstern_x, og vice versa", {
  expect_false(er_valideringsdatasett_gyldig(
    tibble(
      pasid = numeric(), vld_varnamn = character(),
      vld_vartype = character(), vld_verdi_intern_tal = numeric()
    )
  ))
})

test_that("For kvar unike verdi x av vld_vartype så skal det finnast ein
          variabel vld_verdi_intern_x og vld_verdi_ekstern_x", {
  expect_false(er_valideringsdatasett_gyldig(select(d_vld, -vld_verdi_intern_dato, -vld_verdi_ekstern_dato)))
})

test_that("vld_vartype må starta med ein bokstav, og berre innehalda bokstavar og/eller siffer", {
  expect_false(er_valideringsdatasett_gyldig(
    tibble(
      pasid = 101, vld_varnamn = "vekt", vld_vartype = "32int",
      vld_verdi_intern_32int = 76, vld_verdi_ekstern_32int = as.numeric(NA)
    )
  ))
  expect_false(er_valideringsdatasett_gyldig(
    tibble(
      pasid = 101, vld_varnamn = "vekt", vld_vartype = "t_",
      vld_verdi_intern_t_ = 76, vld_verdi_ekstern_t_ = as.numeric(NA)
    )
  ))
})

test_that("Kvar kombinasjon av verdiar til vld_varnamn eller variablar som ikkje
          startar med vld_ skal vera unike", {
  d_vld_ugyldig = tibble(
    pasid = c(101, 101),
    dato_inn = as.Date(c("2020-06-07", "2020-06-07")),
    kjonn = c("M", "M"),
    sjukehus = c("Bergen", "Bergen"),
    vld_varnamn = c("vekt", "vekt"),
    vld_vartype = c("tal", "tal"),
    vld_verdi_intern_tal = c(78, 75),
    vld_verdi_ekstern_tal = c(78, NA)
  )
  expect_false(er_valideringsdatasett_gyldig(d_vld_ugyldig))
})

test_that("Viss vld_vartype = x, så må vld_verdi_intern_y og
          vld_verdi_ekstern_y vera tomme viss x != y", {
  expect_false(er_valideringsdatasett_gyldig(
    tibble(
      pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
      vld_verdi_intern_tal = 76, vld_verdi_ekstern_tal = 76,
      vld_verdi_intern_dato = as.Date("2020-06-07"),
      vld_verdi_ekstern_dato = as.Date(NA)
    )
  ))

  expect_false(er_valideringsdatasett_gyldig(
    tibble(
      pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
      vld_verdi_intern_tal = 76, vld_verdi_ekstern_tal = 76,
      vld_verdi_intern_logisk = as.logical(NA),
      vld_verdi_ekstern_logisk = TRUE
    )
  ))
})

test_that("Skal vera lov å ha vld_verdi_intern_x og vld_verdi_ekstern_x utan at
          det nødvendigvis finst ein vld_vartype med verdi x", {
  expect_true(er_valideringsdatasett_gyldig(
    tibble(
      pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
      vld_verdi_intern_tal = 76, vld_verdi_ekstern_tal = 76,
      vld_verdi_intern_dato = as.Date(NA),
      vld_verdi_ekstern_dato = as.Date(NA)
    )
  ))
})
