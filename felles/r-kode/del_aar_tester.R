# tester for tidslinje-funksjon:

# Tester - Funksjon 1 -----------------------------------------------------

context("del_aar: feilmeldinger ved ugyldige inndata")

test_that("Det gis feilmelding om årstall-vektor ikke er heltallsvektor", {
  expect_error(del_aar(c(2017, 2018, 2019, 2020.5), 1:4, 4), "aar må være heltall")
})

test_that("Det gis feilmelding ved ugyldige delnummer", {
  expect_error(del_aar(2001:2004, 1:4, 3), "delnummer må være verdier i 1:3")
  expect_error(del_aar(2001:2004, c(1, 1.5, 2, 3), 3), "delnummer må være verdier i 1:3")
  expect_error(del_aar(2001:2004, -1:2, 12), "delnummer må være verdier i 1:12")
})

test_that("Det gis feilmelding hvis årstall og delnummer ikke er like lange", {
  expect_error(del_aar(2001:2004, 1:5, 12))
  expect_error(del_aar(2001:2005, 1:4, 12))
})

test_that("Det gis feilmelding om antall_deler ikke har lengde 1", {
  expect_error(del_aar(2001:2004, 1:4, c(4, 12)), "antall_deler må ha lengde 1")
})

test_that("Det gis feilmelding om antall_deler ikke er heltallig", {
  expect_error(del_aar(2001:2004, 1:4, 2.5), "antall_deler må være et heltall")
})

test_that("Det gis feilmelding om antall_deler er mindre enn 1", {
  expect_error(del_aar(2001:2004, 1:4, 0), "antall_deler må være >= 1")
})

test_that("Det gis advarsel om det finnes NA-verdier i årstall eller delnummer", {
  expect_warning(del_aar(c(2001, NA, 2003), 1:3, 4), "Inndata inneholder NA-verdier")
  expect_warning(del_aar(2001:2003, c(1, NA, 2), 4), "Inndata inneholder NA-verdier")
})

context("del_aar: Utdata")

test_that("Utdata samsvarer med forventet resultat.", {
  aar = c(rep(2019, 4), 2020)
  kvart = c(1, 2, 3, 4, 1)
  mnd = c(1, 7, 2, 12, 1)
  forvent = c(2019.125, 2019.375, 2019.625, 2019.875, 2020.125)
  forvent12 = c(2019 + (1 / 12) * mnd[1:4] - 1 / 24, 2020 + 1 / 24)
  expect_identical(del_aar(aar, kvart, 4), forvent)
  expect_equal(del_aar(aar, mnd, 12), forvent12)
})

context("del_aar: Grensetilfeller")

test_that("Funksjonen fungerer med kun ett årstall", {
  expect_equal(del_aar(2019, 1, 4), 2019.125)
})

test_that("Funksjonen fungerer med kun en del", {
  expect_equal(del_aar(2019, 1, 1), 2019.5)
})

test_that("Funksjonen fungerer med 366 deler", {
  expect_equal(del_aar(2019, 1, 366), 2019 + (1 / 366 / 2))
})

# Tester - lag_periode -----------------------------------------------------

context("lag_periode - feilmeldinger ved ugyldige inndata")

test_that("Det gis advarsel om det finnes NA-verdier i datovektor", {
  dato_m_na = c(sample(seq(from = as.Date("2019-01-01"), to = as.Date("2021-12-31"), "days"), size = 30), NA, NA, NA)
  expect_warning(lag_periode(dato_m_na, 3), "Det finnes NA-verdier i dato-vektor",
    fixed = TRUE
  )
})

test_that("Det gis feilmelding om dato ikke er i Date-format", {
  ar_i_db = c(2015, 2020, 2015, 2013, 2019, 2020, 2015, 2019, 2018, 2017)
  ar_i_char = c("2015", "2020", "2015", "2013", "2019", "2020", "2015", "2019", "2018", "2017")
  expect_error(lag_periode(ar_i_db, 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
  expect_error(lag_periode(ar_i_char, 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
})

test_that("Det gis feilmelding om antall_deler ikke har lengde 1", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(lag_periode(dato = dato, antall_deler = c(1, 4)), "antall_deler må ha lengde 1")
})

test_that("Det gis feilmelding om antall_deler ikke er heltallig", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(lag_periode(dato = dato, antall_deler = 1.5), "antall_deler må være et heltall")
})

test_that("Det gis feilmelding om antall_deler er mindre enn 1", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_error(lag_periode(dato = dato, antall_deler = 0.8), "antall_deler må være >= 1")
})


context("lag_periode - Utdata")

test_that("Utdata samsvarer med forventet resultat, ", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  forvent = c(2019.125, 2019.125, 2019.625, 2019.875)
  forvent2 = c(2019.25, 2019.25, 2019.75, 2019.75)
  expect_identical(lag_periode(dato, 4), forvent)
  expect_identical(lag_periode(dato, 2), forvent2)
})

test_that("Det gis ulike resultat ved forskjellige klokkeslett", {
  dato_tidlig = as.POSIXlt(c("2019-01-01 02:00:00", "2019-04-01 02:00:00", "2019-08-01 02:00:00", "2019-12-01 02:00:00"))
  dato_sent = as.POSIXlt(c("2019-01-01 21:00:00", "2019-04-01 21:00:00", "2019-08-01 21:00:00", "2019-12-01 21:00:00"))
  expect_false(any(lag_periode(dato_tidlig, 730) == lag_periode(dato_sent, 730)))
})

test_that("Utdata aldri er et heltall", {
  dato_diger = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)
  dato_aarevis = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 30, replace = TRUE)
  expect_false(any(lag_periode(dato_diger, 10000) == floor(lag_periode(dato_diger, 10000)))) &
    expect_false(any(lag_periode(dato_diger, 3) == floor(lag_periode(dato_diger, 3)))) &
    expect_false(any(lag_periode(dato_aarevis, 1000) == floor(lag_periode(dato_aarevis, 1000))))
})

test_that("Utdata verdi er alltid innen det samme året som inn-dato", {
  dato_diger = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)
  expect_identical(floor(lag_periode(dato_diger, 10000)), year(dato_diger))
})

test_that("Avstand mellom ut-verdier er symmetrisk fordelt", {
  dato = as.Date(c("2019-01-01", "2019-12-31"))
  expect_identical((lag_periode(dato, 365)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 365)[2])))
  expect_identical((lag_periode(dato, 5)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 5)[2])))
  expect_identical((lag_periode(dato, 13)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 13)[2])))
})

test_that("Vi får samme resultat med dato-vektor uten klokkeslett, og dato vektor med klokkeslett = 12", {
  dato_klokke = as.POSIXlt(c("2019-01-01 12:00:00", "2019-04-01 12:00:00", "2019-08-01 12:00:00", "2019-12-01 12:00:00"))
  dato_u_klokke = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(lag_periode(dato_u_klokke, 366), lag_periode(dato_klokke, 366))
  expect_equal(lag_periode(dato_u_klokke, 3), lag_periode(dato_klokke, 3))
  expect_equal(lag_periode(dato_u_klokke, 4), lag_periode(dato_klokke, 4))
})

test_that("Vi får ønsket utverdi når dato er i POSIXct-format uten klokkeslett", {
  dato_ct_uten_klokke = as.POSIXct(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_identical(lag_periode(dato_ct_uten_klokke, 5), lag_periode(dato_Date, 5))
})

test_that("Vi får ønsket utverdi når dato er i POSIXct-format med klokkeslett", {
  dato_ct_med_klokke = as.POSIXct(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_ct_med_klokke_diff = as.POSIXct(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_ct_med_klokke, 500), lag_periode(dato_Date, 500)) # Klokken 12 blir likt for begge typer
  expect_true(any(lag_periode(dato_ct_med_klokke_diff, 500) != lag_periode(dato_ct_med_klokke, 500))) # Forskjellig klokkeslett gir forskjellig utverdi gitt korte nok interval.
})

test_that("Vi får ønsket utverdi når dato er i POSIXlt-format uten klokkeslett", {
  dato_lt_uten_klokke = as.POSIXlt(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_lt_uten_klokke, 5), lag_periode(dato_Date, 5))
})

test_that("Vi får ønsket utverdi når dato er i POSIXlt-format med klokkeslett", {
  dato_lt_med_klokke = as.POSIXlt(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_lt_med_klokke_diff = as.POSIXlt(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_lt_med_klokke, 5), lag_periode(dato_Date, 5))
  expect_true(any(lag_periode(dato_lt_med_klokke_diff, 500) != lag_periode(dato_Date, 500)))
})


context("lag_periode - Grensetilfeller")

test_that("Funksjonen fungerer med kun en datoverdi", {
  dato = as.Date("2019-01-01")
  expect_equal(lag_periode(dato, 4), 2019.125)
})

test_that("Funksjonen fungerer med kun en del", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(lag_periode(dato, 1), c(2019.5, 2019.5, 2019.5, 2019.5))
})
