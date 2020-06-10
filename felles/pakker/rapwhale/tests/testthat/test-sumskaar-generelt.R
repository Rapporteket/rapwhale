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


context("skaar_datasett")

# Eksempel på inndata som inkluderer både basisvariabler og spørreskjema-variabler
d_gyldig_inn = tibble::tribble(
  ~pas_id, ~kjonn, ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2, ~dato,
  1, 1, 1, 2, 1, 10, 20, "2020-01-10",
  2, 2, 2, 1, 2, 20, 10, "2020-02-20",
  3, 1, 3, 1, 2, NA, 10, "2020-03-30"
)

# Eksempel på utdata (skal være identisk til 'd_gyldig_inn' og i tillegg inneholde
# kolonner med sumskårer helt til høyre)
d_gyldig_ut = d_gyldig_inn
d_gyldig_ut = tibble::add_column(d_gyldig_ut, total = c(0.518, 0.775, 1.14), psykisk = c(1, -3, -6.5), .after = "dato")

# Eksempel på inndata hvor sumskår-kolonner finnes fra før
d_inn_inkl_sumskaarer = d_gyldig_inn
d_inn_inkl_sumskaarer = tibble::add_column(d_inn_inkl_sumskaarer, total = NA, psykisk = 5, .after = "psyk2")

test_that("skaar_datasett() fungerer hvis en av de to sumskår-kolonnene finnes fra før", {
  d_inn_inkl_1_sumskaar = d_inn_inkl_sumskaarer
  d_inn_inkl_1_sumskaar$psykisk = NULL

  d_ut_1_erstattet_og_1_ekstra_sumskaar_fasit = d_inn_inkl_1_sumskaar
  d_ut_1_erstattet_og_1_ekstra_sumskaar_fasit$total = c(0.518, 0.775, 1.14)
  d_ut_1_erstattet_og_1_ekstra_sumskaar_fasit = tibble::add_column(d_ut_1_erstattet_og_1_ekstra_sumskaar_fasit,
    psykisk = c(1, -3, -6.5), .after = "dato"
  )

  d_ut_funksjon = suppressWarnings(skaar_datasett(d_inn_inkl_1_sumskaar, skaaringstabell = skaaringstabell_eks))

  expect_equal(d_ut_funksjon, d_ut_1_erstattet_og_1_ekstra_sumskaar_fasit)
})

test_that("skaar_datasett() gir advarsel hvis en eller flere sumskår-kolonner finnes fra før", {
  expect_warning(skaar_datasett(d_inn_inkl_sumskaarer, skaaringstabell = skaaringstabell_eks))
})

test_that("skaar_datasett() fungerer hvis man oppgir variabelnavn", {
  d_inn_feil_variabelnavn = d_gyldig_inn
  d_inn_feil_variabelnavn = rename(d_inn_feil_variabelnavn, fysisk1 = fys1, psykisk2 = psyk2)
  nye_navn = c(fys1 = "fysisk1", psyk2 = "psykisk2")

  d_ut_funksjon = skaar_datasett(d_inn_feil_variabelnavn, variabelnavn = nye_navn, skaaringstabell = skaaringstabell_eks)
  d_gyldig_ut = rename(d_gyldig_ut, fysisk1 = fys1, psykisk2 = psyk2)

  expect_equal(d_ut_funksjon, d_gyldig_ut)
})

test_that("skaar_datasett() fungerer hvis man bytter om to variabelnavn", {
  d_inn_omvendt_variabelnavn = d_gyldig_inn
  d_inn_omvendt_variabelnavn = rename(d_inn_omvendt_variabelnavn, fys1 = fys2, fys2 = fys1)
  navn_omvendt = c(fys2 = "fys1", fys1 = "fys2")

  d_ut_funksjon = skaar_datasett(d_inn_omvendt_variabelnavn, variabelnavn = navn_omvendt, skaaringstabell = skaaringstabell_eks)
  d_gyldig_ut = rename(d_gyldig_ut, fys1 = fys2, fys2 = fys1)

  expect_equal(d_ut_funksjon, d_gyldig_ut)
})

test_that("skaar_datasett() gir ut feilmelding hvis skåringstabell, variabelnavn og/eller variabelverdier er ugyldige", {

  # Skåringstabell som har en variabel med flere rader som har samme verdi innenfor samme delskala
  ugyldig_skaaringstabell = skaaringstabell_eks
  ugyldig_skaaringstabell$verdi[2:3] = 6
  expect_error(skaar_datasett(d_gyldig_inn, skaaringstabell = ugyldig_skaaringstabell))

  # Inndata med ugyldig variabelnavn
  d_ugyldig_variabelnavn = d_gyldig_inn
  d_ugyldig_variabelnavn = rename(d_ugyldig_variabelnavn, fysisk1 = fys1)
  expect_error(skaar_datasett(d_ugyldig_variabelnavn, skaaringstabell = skaaringstabell_eks))

  # Inndata med ugyldig variabelverdi
  d_ugyldig_variabelverdier = d_gyldig_inn
  d_ugyldig_variabelverdier$psyk1[1] = 100
  expect_error(skaar_datasett(d_ugyldig_variabelverdier, skaaringstabell = skaaringstabell_eks))
})

test_that("skaar_datasett() gir ut like sumskårer uavhengig om verdiene i datasettet er tekstverdier eller numeriske verdier", {
  d_gyldig_inn_tekst = d_gyldig_inn
  d_gyldig_inn_tekst$psyk1 = as.character(d_gyldig_inn_tekst$psyk1)

  expect_identical(
    skaar_datasett(d_gyldig_inn, skaaringstabell = skaaringstabell_eks)$total,
    skaar_datasett(d_gyldig_inn_tekst, skaaringstabell = skaaringstabell_eks)$total
  )
  expect_identical(
    skaar_datasett(d_gyldig_inn, skaaringstabell = skaaringstabell_eks)$psykisk,
    skaar_datasett(d_gyldig_inn_tekst, skaaringstabell = skaaringstabell_eks)$psykisk
  )
})


context("sjekk_skaaringstabell")

test_that("sjekk_skaaringstabell() gir ingen feilmelding hvis skåringstabellen er gyldig", {
  expect_silent(sjekk_skaaringstabell(skaaringstabell_eks))
})

test_that("sjekk_skaaringstabell() gir feilmelding hvis skåringstabellen ikke har riktige kolonnenavn", {
  feilmelding_kolonnenavn = "Skåringstabellen må inneholde kolonnene 'delskala', 'variabel', 'verdi' og 'koeffisient'"

  skaaringstabell_ugyldig_navn_delskala = skaaringstabell_eks
  skaaringstabell_ugyldig_navn_delskala = rename(skaaringstabell_ugyldig_navn_delskala, deelskala = delskala)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_navn_delskala), feilmelding_kolonnenavn)

  skaaringstabell_ugyldig_navn_variabel = skaaringstabell_eks
  skaaringstabell_ugyldig_navn_variabel = rename(skaaringstabell_ugyldig_navn_variabel, var = variabel)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_navn_variabel), feilmelding_kolonnenavn)

  skaaringstabell_ugyldig_navn_verdi = skaaringstabell_eks
  skaaringstabell_ugyldig_navn_verdi = rename(skaaringstabell_ugyldig_navn_verdi, tallverdi = verdi)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_navn_verdi), feilmelding_kolonnenavn)

  skaaringstabell_ugyldig_navn_koeffisient = skaaringstabell_eks
  skaaringstabell_ugyldig_navn_koeffisient = rename(skaaringstabell_ugyldig_navn_koeffisient, koeff = koeffisient)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_navn_koeffisient), feilmelding_kolonnenavn)
})

test_that("sjekk_skaaringstabell() gir feilmelding hvis skåringstabellen har
          en variabel med flere rader som har samme verdi innenfor samme delskala", {
  skaaringstabell_ugyldig_dupl_verdi = skaaringstabell_eks
  skaaringstabell_ugyldig_dupl_verdi$verdi[1:2] = 1

  expect_error(
    sjekk_skaaringstabell(skaaringstabell_ugyldig_dupl_verdi),
    "Skåringstabellen kan ikke inneholde dupliserte verdier for en variabel innenfor samme delskala"
  )
})

test_that("sjekk_skaaringstabell() gir feilmelding hvis koeffisient-kolonnen
          i skåringstabellen inneholder NA-verdier", {
  skaaringstabell_ugyldig_na_koeffisient = skaaringstabell_eks
  skaaringstabell_ugyldig_na_koeffisient$koeffisient[2] = NA

  expect_error(
    sjekk_skaaringstabell(skaaringstabell_ugyldig_na_koeffisient),
    "Koeffisient-kolonnen i skåringstabellen kan ikke inneholde NA-verdier"
  )
})

test_that("sjekk_skaaringstabell() gir feilmelding hvis skåringstabellen innholder feil variabeltyper", {
  feilmelding_kolonneformat = "Verdi-kolonnen og koeffisient-kolonnen må være numeriske og variabel-kolonnen må være av typen tekst"

  skaaringstabell_ugyldig_verdi_kolonne = skaaringstabell_eks
  skaaringstabell_ugyldig_verdi_kolonne$verdi = as.character(skaaringstabell_ugyldig_verdi_kolonne$verdi)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_verdi_kolonne), feilmelding_kolonneformat)

  skaaringstabell_ugyldig_koeffisient_kolonne = skaaringstabell_eks
  skaaringstabell_ugyldig_koeffisient_kolonne$koeffisient = as.character(skaaringstabell_ugyldig_koeffisient_kolonne$koeffisient)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_koeffisient_kolonne), feilmelding_kolonneformat)

  skaaringstabell_ugyldig_variabel_kolonne = skaaringstabell_eks
  skaaringstabell_ugyldig_variabel_kolonne$variabel = 1:nrow(skaaringstabell_ugyldig_variabel_kolonne)
  expect_error(sjekk_skaaringstabell(skaaringstabell_ugyldig_variabel_kolonne), feilmelding_kolonneformat)
})


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


context("sjekk_variabelverdier")

# Ved å benytte den overordnede funksjonen skaar_datasett() vil alle variabler i datasettet
# som ikke er spørreskjema-variabler bli filtrert vekk før sjekk_variabelverdier() kalles på.
# Derfor inneholder eksempel-data for disse testene kun spørreskjema-variabler.

# Eksempeldata med bare gyldige tallverdier
d_gyldig_eks1 = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, 1, 1, 10, 10,
  2, 2, 2, 20, 20,
  3, 1, 2, 20, 10
)

# Eksempel på inndata med både gyldige tallverdier og gyldige NA-verdier
d_gyldig_eks2 = d_gyldig_eks1
d_gyldig_eks2$psyk1[3] = NA

# Eksempel på inndata med ugyldige NA-verdier (for "gen" og "fys1")
d_ugyldig_eks1 = d_gyldig_eks1
d_ugyldig_eks1$gen[3] = NA
d_ugyldig_eks1$fys1[1] = NA

# Eksempel på skåringstabell med ugyldige kolonnenavn
skaaringstabell_ugyldig_eks1 = rename(skaaringstabell_eks, tullball = variabel, ballball = verdi)

# Eksempel på skåringstabell med ugyldig format
skaaringstabell_ugyldig_eks2 = table(skaaringstabell_eks)

# Eksempel på skåringstabell med både ugyldige kolonnenavn og ugyldig format
skaaringstabell_ugyldig_eks3 = table(skaaringstabell_ugyldig_eks1)

test_that("sjekk_variabelverdier() gir feilmelding hvis ikke begge de to aktuelle variablene
          ('variabel' og 'verdi') er i koblingstabellen og/eller den ikke er en tibble/data.frame", {
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

test_that("sjekk_variabelverdier() gir feilmelding hvis verdiene ikke er numeriske", {
  d_feil_variabeltype = d_gyldig_eks1
  d_feil_variabeltype$fys1 = as.factor(d_feil_variabeltype$fys1)
  expect_error(
    sjekk_variabelverdier(d_feil_variabeltype, skaaringstabell_eks, godta_manglende = FALSE),
    "Datasettet inneholder verdier som ikke er numeriske"
  )
})

test_that("sjekk_variabelverdier() gjev inga feilmelding for datasett med gyldige variabelverdiar", {
  expect_silent(sjekk_variabelverdier(d_gyldig_eks1, skaaringstabell_eks, godta_manglende = FALSE))
  expect_silent(sjekk_variabelverdier(d_gyldig_eks2, skaaringstabell_eks, godta_manglende = TRUE))
})

test_that("sjekk_variabelverdier() gir ingen feilmelding for datasett med ugyldige NA-verdiar viss godta_manglende = TRUE", {
  expect_silent(sjekk_variabelverdier(d_ugyldig_eks1, skaaringstabell_eks, godta_manglende = TRUE))
})

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med ugyldige NA-verdiar viss godta_manglende = FALSE", {
  expect_error(sjekk_variabelverdier(d_ugyldig_eks1, skaaringstabell_eks, godta_manglende = FALSE))
})

test_that("sjekk_variabelverdier() gjev feilmelding for datasett med ugyldige variabelverdiar", {
  expect_error(sjekk_variabelverdier(tibble(gen = c(200, 200)), skaaringstabell_eks, godta_manglende = FALSE))
})


context("finn_ugyldige_verdier")

# Tom dataramme med spesifiserte variabeltyper
ugyldighetstabell_tom = tibble(
  radnr = integer(),
  variabel = character(),
  feilverdi = numeric()
)

test_that("finn_ugyldige_verdier() gir ut en dataramme (med kun kolonnenavn) hvis inndata er gyldig", {
  expect_identical(finn_ugyldige_verdier(d_gyldig_eks1, skaaringstabell_eks), ugyldighetstabell_tom)
})

# Eksempeldata med 1 feil # fixme: bør d_gyldig_eks_1 vises her?
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

test_that("finn_ugyldige_verdier() gir ut korrekt feiloversikt hvis det finnes ugyldige verdier i datasettet", {
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

test_that("oppsummer_ugyldige_verdier() gir ut det samme uavhengig av rekkefølgen til radene i datarammen", {
  ugyldighetstabell_2_feil_samme_variabel_annen_radrekkefolge = ugyldighetstabell_2_feil_samme_variabel
  ugyldighetstabell_2_feil_samme_variabel_annen_radrekkefolge = ugyldighetstabell_2_feil_samme_variabel_annen_radrekkefolge[c(2, 1, 3), ]
  expect_identical(
    oppsummer_ugyldige_verdier(ugyldighetstabell_2_feil_samme_variabel),
    oppsummer_ugyldige_verdier(ugyldighetstabell_2_feil_samme_variabel_annen_radrekkefolge)
  )
})


context("skaar_datasett_uten_validering")

# Eksempeldata som bare inneholder verdier som finnes i skåringstabellen
# (datasettet inneholder alle mulige verdier for hver variabel minst en gang)
d_gyldig_alle_verdier = tibble::tribble(
  ~gen, ~fys1, ~fys2, ~psyk1, ~psyk2,
  1, 2, 1, 10, 20,
  2, 1, 2, 20, 10,
  3, 1, 2, NA, 10
)

# Manuell utregning av sumskår for d_gyldig_alle_verdier
sumskaar_total_rad1 = 0.2 + 0.3 + 0 + 0 + 0.018
sumskaar_psykisk_rad1 = -2 + (-5) + 8
sumskaar_total_rad2 = 0.4 + 0 + 0.35 + 0.025 + 0
sumskaar_psykisk_rad2 = 0 + 5 + (-8)
sumskaar_total_rad3 = 0.8 + 0 + 0.35 + (-0.01) + 0
sumskaar_psykisk_rad3 = 2 + (-0.5) + (-8)

# Utregnede sumskårer for d_gyldig_alle_verdier
sumskaar_tabell = tibble::tribble(
  ~total, ~psykisk,
  sumskaar_total_rad1, sumskaar_psykisk_rad1,
  sumskaar_total_rad2, sumskaar_psykisk_rad2,
  sumskaar_total_rad3, sumskaar_psykisk_rad3
)

test_that("skaar_datasett_uten_validering() regner ut korrekt sumskår hvis alle verdiene finnes i skåringstabellen", {
  expect_equal(
    skaar_datasett_uten_validering(d_gyldig_alle_verdier, skaaringstabell_eks),
    sumskaar_tabell
  )
})

# Eksempeldata som inneholder en NA-verdi uten tilknyttet koeffisient
d_na = d_gyldig_alle_verdier
d_na[1, 3] = NA

# Manuell utregning av sumskår for eksempeldata som inneholder en NA-verdi uten tilknyttet koeffisient
sumskaar_na_total_rad1 = NA_real_
sumskaar_na_psykisk_rad1 = sumskaar_psykisk_rad1
sumskaar_na_total_rad2 = sumskaar_total_rad2
sumskaar_na_psykisk_rad2 = sumskaar_psykisk_rad2
sumskaar_na_total_rad3 = sumskaar_total_rad3
sumskaar_na_psykisk_rad3 = sumskaar_psykisk_rad3

# Utregnede sumskårer for eksempeldata som inneholder en NA-verdi uten tilknyttet koeffisient
sumskaar_na_tabell = tibble::tribble(
  ~total, ~psykisk,
  sumskaar_na_total_rad1, sumskaar_na_psykisk_rad1,
  sumskaar_na_total_rad2, sumskaar_na_psykisk_rad2,
  sumskaar_na_total_rad3, sumskaar_na_psykisk_rad3
)

test_that("skaar_datasett_uten_validering() gir ut NA som sumskår ved NA-verdier uten tilknyttet koeffisient", {
  expect_equal(
    skaar_datasett_uten_validering(d_na, skaaringstabell_eks),
    sumskaar_na_tabell
  )
})

# Eksempeldata hvor 1 av besvarelsene har NA-verdier på alle spørsmål
d_1_besvarelse_bare_na = d_gyldig_alle_verdier
d_1_besvarelse_bare_na[1, ] = NA

# Manuell utregning av sumskår for eksempeldata hvor 1 av besvarelsene har NA-verdier på alle spørsmål
sumskaar_1_besvarelse_bare_na_total_rad1 = NA_real_
sumskaar_1_besvarelse_bare_na_psykisk_rad1 = NA_real_
sumskaar_1_besvarelse_bare_na_total_rad2 = sumskaar_total_rad2
sumskaar_1_besvarelse_bare_na_psykisk_rad2 = sumskaar_psykisk_rad2
sumskaar_1_besvarelse_bare_na_total_rad3 = sumskaar_total_rad3
sumskaar_1_besvarelse_bare_na_psykisk_rad3 = sumskaar_psykisk_rad3

# Utregnede sumskårer eksempeldata hvor 1 av besvarelsene har NA-verdier på alle spørsmål
sumskaar_1_besvarelse_bare_na_tabell = tibble::tribble(
  ~total, ~psykisk,
  sumskaar_1_besvarelse_bare_na_total_rad1, sumskaar_1_besvarelse_bare_na_psykisk_rad1,
  sumskaar_1_besvarelse_bare_na_total_rad2, sumskaar_1_besvarelse_bare_na_psykisk_rad2,
  sumskaar_1_besvarelse_bare_na_total_rad3, sumskaar_1_besvarelse_bare_na_psykisk_rad3
)

test_that("skaar_datasett_uten_validering() gir ut riktig sumskår hvis 1 av besvarelselse har NA-verdier på alle spørsmål", {
  expect_equal(
    skaar_datasett_uten_validering(d_1_besvarelse_bare_na, skaaringstabell_eks),
    sumskaar_1_besvarelse_bare_na_tabell
  )
})

# Eksempeldata hvor alle besvarelsene har NA-verdier på alle spørsmål
d_alle_besvarelser_bare_na = d_gyldig_alle_verdier
d_alle_besvarelser_bare_na[] = NA

# Manuell utregning av sumskår for eksempeldata hvor alle besvarelsene har NA-verdier på alle spørsmål
sumskaar_alle_besvarelser_bare_na_total_rad1 = NA_real_
sumskaar_alle_besvarelser_bare_na_psykisk_rad1 = NA_real_
sumskaar_alle_besvarelser_bare_na_total_rad2 = NA_real_
sumskaar_alle_besvarelser_bare_na_psykisk_rad2 = NA_real_
sumskaar_alle_besvarelser_bare_na_total_rad3 = NA_real_
sumskaar_alle_besvarelser_bare_na_psykisk_rad3 = NA_real_

# Utregnede sumskårer for eksempeldata hvor alle besvarelsene har NA-verdier på alle spørsmål
sumskaar_alle_besvarelser_bare_na_tabell = tibble::tribble(
  ~total, ~psykisk,
  sumskaar_alle_besvarelser_bare_na_total_rad1, sumskaar_alle_besvarelser_bare_na_psykisk_rad1,
  sumskaar_alle_besvarelser_bare_na_total_rad2, sumskaar_alle_besvarelser_bare_na_psykisk_rad2,
  sumskaar_alle_besvarelser_bare_na_total_rad3, sumskaar_alle_besvarelser_bare_na_psykisk_rad3
)

test_that("skaar_datasett_uten_validering() gir ut riktige sumskårer hvis alle besvarelsene har NA-verdier på alle spørsmål", {
  expect_identical(
    skaar_datasett_uten_validering(d_alle_besvarelser_bare_na, skaaringstabell_eks),
    sumskaar_alle_besvarelser_bare_na_tabell
  )
})

# Eksempeldata som bare inneholder 1 besvarelse
d_1_besvarelse = d_gyldig_alle_verdier
d_1_besvarelse = d_1_besvarelse[-c(1, 3), ]

# Manuell utregning av sumskår for eksempeldata som bare inneholder 1 besvarelse
sumskaar_1_besvarelse_total = sumskaar_total_rad2
sumskaar_1_besvarelse_psykisk = sumskaar_psykisk_rad2

# Utregnede sumskårer for eksempeldata som bare inneholder 1 besvarelse
sumskaar_1_besvarelse_tabell = tibble::tribble(
  ~total, ~psykisk,
  sumskaar_1_besvarelse_total, sumskaar_1_besvarelse_psykisk
)

test_that("skaar_datasett_uten_validering() regner ut korrekt sumskår ved bare 1 besvarelse", {
  expect_identical(
    skaar_datasett_uten_validering(d_1_besvarelse, skaaringstabell_eks),
    sumskaar_1_besvarelse_tabell
  )
})


test_that("skaar_datasett_uten_validering() gir ut 0-rads resultat med alle skårkolonnene når inndata har 0 rader", {
  d_0_besvarelser = d_gyldig_alle_verdier[0, ]
  sumskaar_0_besvarelser = tibble(total = double(), psykisk = double())
  expect_identical(
    skaar_datasett_uten_validering(d_0_besvarelser, skaaringstabell_eks),
    sumskaar_0_besvarelser
  )
})

test_that("skaar_datasett_uten_validering() gir ut riktige sumskårer i samme rekkefølge som i delskala-kolonnen i skåringstabellen", {
  skaaringstabell_flere_delskalaer = tibble::tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "b", "fys", 1, 0.2,
    "b", "fys", 2, 0.3,
    "b", "psyk", 1, 0.4,
    "b", "psyk", 2, 0.2,
    "a", "fys", 1, 0.8,
    "a", "fys", 2, 0,
    "a", "psyk", 1, 0.9,
    "a", "psyk", 2, 0,
    "d", "fys", 1, 0.3,
    "d", "fys", 2, 0,
    "d", "psyk", 1, 0.6,
    "d", "psyk", 2, 0,
    "c", "fys", 1, 0.35,
    "c", "fys", 2, -0.08,
    "c", "psyk", 1, 0.55,
    "c", "psyk", 2, -0.01,
  )

  d_enkelt_eks_inn = tibble::tribble(
    ~fys, ~psyk,
    1, 2,
    2, 1,
    2, 1
  )

  d_enkelt_eks_ut = tibble::tribble(
    ~b, ~a, ~d, ~c,
    0.4, 0.8, 0.3, 0.34,
    0.7, 0.9, 0.6, 0.47,
    0.7, 0.9, 0.6, 0.47
  )

  expect_equal(
    skaar_datasett_uten_validering(d_enkelt_eks_inn, skaaringstabell = skaaringstabell_flere_delskalaer),
    d_enkelt_eks_ut
  )
})


context("legg_til_na_i_skaaringstabell")

test_that("legg_til_na_i_skaaringstabell() fungerer", {
  # Original skåringstabell (merk at éi rad òg har NA-verdiar)
  skaaringstabell_orig = tibble::tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "total", "var_a", 1, 5,
    "total", "var_a", 2, 10,
    "total", "var_b", 1, 3,
    "total", "var_b", NA, NA,
    "psykisk", "var_a", 1, 0
  )

  # Skåringstabell med NA fylt ut
  skaaringstabell_med_na = tibble::tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "total", "var_a", 1, 5,
    "total", "var_a", 2, 10,
    "total", "var_a", NA, NA,
    "total", "var_b", 1, 3,
    "total", "var_b", NA, NA,
    "psykisk", "var_a", 1, 0,
    "psykisk", "var_a", NA, NA
  )

  # Funksjonen fungerer i enkelt tilfelle
  expect_identical(
    legg_til_na_i_skaaringstabell(skaaringstabell_orig),
    skaaringstabell_med_na
  )

  # Ekstrasjekk på at funksjonen er idempotent
  # (i praksis at han handterer NA-verdiar som finst frå før)
  expect_identical(
    legg_til_na_i_skaaringstabell(skaaringstabell_orig),
    legg_til_na_i_skaaringstabell(legg_til_na_i_skaaringstabell(skaaringstabell_orig))
  )
  expect_identical(
    legg_til_na_i_skaaringstabell(skaaringstabell_eks),
    legg_til_na_i_skaaringstabell(legg_til_na_i_skaaringstabell(skaaringstabell_eks))
  )
})

test_that("legg_til_na_i_skaaringstabell() overskriv ikkje eksisterande NA-verdiar", {
  # Skåringstabell med NA-verdi som skal gje ut koeffisient som *ikkje* er NA
  skaaringstabell_med_na = tibble::tribble(
    ~delskala, ~variabel, ~verdi, ~koeffisient,
    "total", "var_a", 1, 5,
    "total", "var_a", NA, 10
  )

  # Her skal koeffisientverdien bevarast
  expect_identical(
    legg_til_na_i_skaaringstabell(skaaringstabell_med_na),
    skaaringstabell_med_na
  )
})


context("legg_til_eller_erstatt_kolonner")

d_eks_inkl_sumskaar = tibble::tribble(
  ~pas_id, ~fys, ~psyk, ~dato, ~sumskaar_total, ~sumskaar_psykisk,
  1, 2, 3, "2020-05-15", 4, 5
)

test_that("legg_til_eller_erstatt_kolonner() fungerer hvis ingen, noen eller alle sumskår-kolonner finnes fra før", {

  # Ingen sumskår-kolonner finnes fra før
  expect_identical(
    legg_til_eller_erstatt_kolonner(
      d_orig = select(d_eks_inkl_sumskaar, -c(sumskaar_total, sumskaar_psykisk)),
      d_ekstrakol = select(d_eks_inkl_sumskaar, c(sumskaar_total, sumskaar_psykisk))
    ),
    d_eks_inkl_sumskaar
  )

  # 1 av 2 sumskår-kolonner finnes fra før
  d_eks_sumskaar_total_til_hoyre = subset(d_eks_inkl_sumskaar, select = c(pas_id:dato, sumskaar_psykisk, sumskaar_total))
  expect_identical(
    suppressWarnings(legg_til_eller_erstatt_kolonner(
      d_orig = select(d_eks_inkl_sumskaar, -sumskaar_total),
      d_ekstrakol = select(d_eks_inkl_sumskaar, c(sumskaar_total, sumskaar_psykisk))
    )),
    d_eks_sumskaar_total_til_hoyre
  )

  # Begge sumskår-kolonner finnes fra før
  expect_identical(
    suppressWarnings(legg_til_eller_erstatt_kolonner(
      d_orig = d_eks_inkl_sumskaar,
      d_ekstrakol = select(d_eks_inkl_sumskaar, c(sumskaar_total, sumskaar_psykisk))
    )),
    d_eks_inkl_sumskaar
  )
})

test_that("legg_til_eller_erstatt_kolonner() gir advarsel hvis en eller flere sumskår-kolonner finnes fra før", {
  expect_warning(legg_til_eller_erstatt_kolonner(d_eks_inkl_sumskaar,
    d_ekstrakol = select(d_eks_inkl_sumskaar, c(sumskaar_total, sumskaar_psykisk))
  ))
})
