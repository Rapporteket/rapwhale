# tester for tidslinje-funksjon:

# Tester - Periode til tidslinje -----------------------------------------------------

context("periode_til_tidslinje: feilmeldinger ved ugyldige inndata")

test_that("Det gis feilmelding om årstall-vektor ikke er heltallsvektor", {
  expect_error(periode_til_tidslinje(c(2017, 2018, 2019, 2020.5), 1:4, 4), "aar må være heltall")
})

test_that("Det gis feilmelding ved ugyldige delnummer", {
  expect_error(periode_til_tidslinje(2001:2004, 1:4, 3), "delnummer må være verdier i 1:3")
  expect_error(periode_til_tidslinje(2001:2004, c(1, 1.5, 2, 3), 3), "delnummer må være verdier i 1:3")
  expect_error(periode_til_tidslinje(2001:2004, -1:2, 12), "delnummer må være verdier i 1:12")
})

test_that("Det gis feilmelding hvis årstall og delnummer ikke er like lange", {
  expect_error(periode_til_tidslinje(2001:2004, 1:5, 12))
  expect_error(periode_til_tidslinje(2001:2005, 1:4, 12))
})

test_that("Det gis feilmelding om antall_deler ikke har lengde 1", {
  expect_error(periode_til_tidslinje(2001:2004, 1:4, c(4, 12)), "antall_deler må ha lengde 1")
})

test_that("Det gis feilmelding om antall_deler ikke er heltallig", {
  expect_error(periode_til_tidslinje(2001:2004, 1:4, 2.5), "antall_deler må være et heltall")
})

test_that("Det gis feilmelding om antall_deler er mindre enn 1", {
  expect_error(periode_til_tidslinje(2001:2004, 1:4, 0), "antall_deler må være >= 1")
})

test_that("Det gis advarsel om det finnes NA-verdier i årstall eller delnummer", {
  expect_warning(periode_til_tidslinje(c(2001, NA, 2003), 1:3, 4), "Inndata inneholder NA-verdier")
  expect_warning(periode_til_tidslinje(2001:2003, c(1, NA, 2), 4), "Inndata inneholder NA-verdier")
})

context("periode_til_tidslinje: Utdata")

test_that("Utdata samsvarer med forventet resultat.", {
  aar = c(rep(2019, 4), 2020)
  kvart = c(1, 2, 3, 4, 1)
  mnd = c(1, 7, 2, 12, 1)
  forvent = c(2019.125, 2019.375, 2019.625, 2019.875, 2020.125)
  forvent12 = c(2019 + (1 / 12) * mnd[1:4] - 1 / 24, 2020 + 1 / 24)
  expect_identical(periode_til_tidslinje(aar, kvart, 4), forvent)
  expect_equal(periode_til_tidslinje(aar, mnd, 12), forvent12)
})

context("periode_til_tidslinje: Grensetilfeller")

test_that("Funksjonen fungerer med kun ett årstall", {
  expect_equal(periode_til_tidslinje(2019, 1, 4), 2019.125)
})

test_that("Funksjonen fungerer med kun én del", {
  expect_equal(periode_til_tidslinje(2019, 1, 1), 2019.5)
})

test_that("Funksjonen fungerer med 366 deler", {
  expect_equal(periode_til_tidslinje(2019, 1, 366), 2019 + (1 / 366 / 2))
})

# Tester - tid_til_tidslinje -----------------------------------------------------

context("tid_til_tidslinje - feilmeldinger ved ugyldige inndata")

test_that("Det gis advarsel om det finnes NA-verdier i datovektor", {
  dato_m_na = c(as.Date("2019-01-01"), NA)
  expect_warning(tid_til_tidslinje(dato_m_na, 3), "Det finnes NA-verdier i dato-vektor")
})

test_that("Det gis feilmelding om dato ikke er i Date-format", {
  expect_error(tid_til_tidslinje(2001:2004, 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
  expect_error(tid_til_tidslinje(as.character(2001:2004), 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
})

test_that("Det gis feilmelding om antall_deler ikke har lengde 1", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(tid_til_tidslinje(dato = dato, antall_deler = c(1, 4)), "antall_deler må ha lengde 1")
})

test_that("Det gis feilmelding om antall_deler ikke er heltallig", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(tid_til_tidslinje(dato = dato, antall_deler = 1.5), "antall_deler må være et heltall")
})

test_that("Det gis feilmelding om antall_deler er mindre enn 1", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(tid_til_tidslinje(dato = dato, antall_deler = 0), "antall_deler må være >= 1")
})


context("tid_til_tidslinje - Utdata")

test_that("Utdata samsvarer med forventet resultat, ", {
  dato = as.Date(c(
    "2019-01-01",
    "2019-04-01", "2019-04-02",
    "2019-06-30", "2019-07-01", "2019-07-02",
    "2019-09-30", "2019-10-01",
    "2019-12-31", "2020-01-01"
  ))

  forvent4 = c(
    2019.125, 2019.125,
    2019.375, 2019.375, 2019.375,
    2019.625, 2019.625, 2019.625,
    2019.875,
    2020.125
  )

  # Om ingen klokkeslett er oppgitt vil funksjonen gi hver dato klokkeslettet 12:00.
  # Ved en 4-deling av året er grensene for hvert intervall [0,250,500,750,1)
  # Decimal_date for dato over *gitt* klokkeslett 12:00 er:
  # (.0014 .2479,
  #  .2507, .4945, .4973,
  #  .5000, .7466, .7493,
  #  .9986,
  #  2020.000)
  # slik at forventet utverdi stemmer.

  # Vi ønsker at nedre grense for intervallet inkluderes i et intervall, mens den øvre grensen ikke inkluderes.
  # Hvis et punkt får verdien 2020, skal det være med i det første intervallet i 2020, ikke det siste i 2019.
  # Midtpunktet i et år blir med decimal_date 02-Juli 12:00 UCT / 02-Juli 13:00 CET.

  forvent2 = c(
    2019.25, 2019.25, 2019.25, 2019.25, 2019.25,
    2019.75, 2019.75, 2019.75, 2019.75,
    2020.25
  )

  # Merk: Skal ein plassera datoar i intervall, må dei nødvendigvis
  # representerast ved eit *tidspunkt*. Elles kan ein dato høyra
  # heime i meir enn eitt intervall. Deler ein for eksempel året i
  # to, vil 1. juli høyra heime i begge intervalla, første 12 timar i
  # første intervall og neste 12 timar i andre intervall. Me føreset
  # at kvar dato vert representert ved klokka 12 på den aktuelle datoen.
  expect_identical(tid_til_tidslinje(dato, 4), forvent4)
  expect_identical(tid_til_tidslinje(dato, 2), forvent2)
})

test_that("Det gis ulike resultat ved forskjellige klokkeslett", {
  tider_tidlig = as.POSIXlt(c("2019-01-01 02:00:00", "2019-04-01 02:00:00", "2019-08-01 02:00:00", "2019-12-01 02:00:00"))
  tider_sent = as.POSIXlt(c("2019-01-01 21:00:00", "2019-04-01 21:00:00", "2019-08-01 21:00:00", "2019-12-01 21:00:00"))
  expect_false(any(tid_til_tidslinje(tider_tidlig, 730) == tid_til_tidslinje(tider_sent, 730)))
})

test_that("Det taes hensyn til tidssone", {
  # Utregningene skal foregå i tidssonen til inndataene.
  # Antar altså at det bare finnes én tidssone
  # (sant for POSIXct-objekt, hvis vi ser vekk fra sommertid).
  # (For Norge er det strengt tatt to tidssoner, én for normaltid (CET) og én for sommertid (CEST),
  # men utregninger mellom disse går likevel fint, og tidssonen for starten av året er lik
  # tidssonen for slutten av året.)

  # Lager variabler for klokka 02:15 (UTC + 1) for alle dagene i 2019.
  # Men merk at *veggtid* vil variere (UTC + 1 for vintertid og UTC + 2 for sommertid).
  tid_norsk = as.POSIXct("2019-01-01 02:15", tz = "CET") + (0:364) * 24 * 60 * 60

  # Tilsvarende variablar for klokka 02:15 UTC.
  tid_utc = as.POSIXct("2019-01-01 02:15", tz = "UTC") + (0:364) * 24 * 60 * 60

  # Skal ha like intervall for norsk tidssone og UTC-tidssone
  ant_per = 365 * 24 * 2
  expect_identical(tid_til_tidslinje(tid_norsk, ant_per), tid_til_tidslinje(tid_utc, ant_per))

  # Det går ett døgn mellom hvert tidspunkt, så differansen/inkrementene
  # mellom etterfølgende punkt på tidslinjene skal være konstant
  # (her lik 2 * 24 / ant_per)
  unike_inkrement = function(x, desimaler = 4) {
    inkrement = round(diff(x) * ant_per, desimaler)
    length(unique(inkrement))
  }
  expect_identical(unike_inkrement(tid_til_tidslinje(tid_norsk, ant_per)), 1L)
  expect_identical(unike_inkrement(tid_til_tidslinje(tid_utc, ant_per)), 1L)
})


test_that("Utdata verdi er alltid innen det samme året som inn-dato", {
  set.seed(1234)
  dato_diger = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)
  expect_identical(floor(tid_til_tidslinje(dato_diger, 10000)), lubridate::year(dato_diger))
})

test_that("Avstand mellom ut-verdier er symmetrisk fordelt", {
  test_symmetri = function(antall_deler) {
    expect_identical(
      tid_til_tidslinje(as.Date("2019-01-01"), antall_deler) - 2019,
      2020 - tid_til_tidslinje(as.Date("2019-12-31"), antall_deler)
    )
  }
  test_symmetri(antall_deler = 1000)
  test_symmetri(antall_deler = 365)
  test_symmetri(antall_deler = 5)
  test_symmetri(antall_deler = 13)
})

test_that("Vi får samme resultat med dato-vektor uten klokkeslett, og dato vektor med klokkeslett = 12:00", {
  dato_klokke = as.POSIXlt(c("2019-01-01 12:00:00", "2019-04-01 12:00:00", "2019-08-01 12:00:00", "2019-12-01 12:00:00"))
  dato_u_klokke = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(tid_til_tidslinje(dato_u_klokke, 366), tid_til_tidslinje(dato_klokke, 366))
  expect_equal(tid_til_tidslinje(dato_u_klokke, 3000), tid_til_tidslinje(dato_klokke, 3000))
  expect_equal(tid_til_tidslinje(dato_u_klokke, 3), tid_til_tidslinje(dato_klokke, 3))
  expect_equal(tid_til_tidslinje(dato_u_klokke, 4), tid_til_tidslinje(dato_klokke, 4))
})

test_that("Vi får ønsket utverdi når dato er i POSIXct-format med klokkeslett", {
  dato_ct_med_klokke = as.POSIXct(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_ct_med_klokke_diff = as.POSIXct(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(tid_til_tidslinje(dato_ct_med_klokke, 500), tid_til_tidslinje(dato_Date, 500))
  expect_true(any(tid_til_tidslinje(dato_ct_med_klokke_diff, 500) != tid_til_tidslinje(dato_ct_med_klokke, 500)))
})

test_that("Vi får ønsket utverdi når dato er i POSIXlt-format med klokkeslett", {
  dato_lt_med_klokke = as.POSIXlt(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_lt_med_klokke_diff = as.POSIXlt(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_lt_med_klokke_midnatt = as.POSIXlt(c("2019-01-01 00:00:00", "2019-05-31 13:00:00", "2019-07-01 00:00:00", "2019-12-01 12:00:00"),
    tz = "UTC"
  )
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(tid_til_tidslinje(dato_lt_med_klokke, 5), tid_til_tidslinje(dato_Date, 5))
  expect_true(any(tid_til_tidslinje(dato_lt_med_klokke_diff, 500) != tid_til_tidslinje(dato_Date, 500)))
  expect_identical(tid_til_tidslinje(dato_lt_med_klokke_midnatt, 500), c(2019.001, 2019.413,2019.495, 2019.917))
})

test_that("Vi får ønsket utverdi når dato og tid er like over nyttår (01:00:00)", {
  dato_nyttaar = as.POSIXlt(c(
    "2019-12-31 23:59:00", "2020-01-01 00:00:01", "2020-01-01 00:00:15", "2020-01-01 00:00:30",
    "2020-01-01 00:00:45", "2020-01-01 01:00:00", "2020-01-01 05:00:00"
  ))
  forventet_ut = c(2019.875, 2020.125, 2020.125, 2020.125, 2020.125, 2020.125, 2020.125)
  expect_identical(tid_til_tidslinje(dato_nyttaar, 4), forventet_ut)
})


context("tid_til_tidslinje - Grensetilfeller")

test_that("Funksjonen fungerer med kun én datoverdi", {
  dato = as.Date("2019-01-01")
  expect_equal(tid_til_tidslinje(dato, 4), 2019.125)
})

test_that("Funksjonen fungerer med kun én del", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(tid_til_tidslinje(dato, 1), c(2019.5, 2019.5, 2019.5, 2019.5))
})

test_that("Funksjonen fungerer ved ikke-etterfølgende årstall", {
  expect_equal(tid_til_tidslinje(as.Date(c("2019-01-01", "2022-01-01")), 2), c(2019.25, 2022.25))
})
