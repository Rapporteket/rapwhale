# tester for tidslinje-funksjon:

# Tester - Funksjon 1 -----------------------------------------------------

context("Funksjon 1 - inndata")

# Sjekk at det gis feilmelding om årstall inneholder desimal:
test_that("Det gis feilmelding om årstall-vektor ikke er heltallsvektor", {
  ar_m_des = c(2017, 2018, 2019, 2017, 2015, 2010, 2020, 2018.5)
  kvart_i_db = c(1, 2, 3, 4, 1, 2, 3, 1, 2, 3)
  expect_error(del_aar(ar_m_des, kvart_i_db, 4))
})

# Sjekk at det gis feilmelding om antall_deler er mindre enn 1..
test_that("Det gis feilmelding om antall_deler er mindre enn 1.", {
  ar_i_db = c(2015, 2020, 2015, 2013, 2019, 2020, 2015, 2019, 2018, 2017)
  kvart_i_db = c(1, 2, 3, 4, 1, 2, 3, 1, 2, 3)
  expect_error(del_aar(ar_i_db, kvart_i_db, 0.5), "Antall deler er < 1")
})

# Sjekk at funksjonen gir en advarsel om årstall inneholder NA:
test_that("Det gis advarsel om det finnes NA-verdier i årstall", {
  ar_i_db_na = c(2017, 2018, 2019, NA_integer_, 2020, 2018, 2015, 2020, 2019, 2270)
  kvart_i_db = c(1, 2, 3, 4, 1, 2, 3, 1, 2, 3)
  expect_warning(del_aar(ar_i_db_na, kvart_i_db, 4), "År inneholder NA-verdier",
    fixed = TRUE
  )
})

context("Funksjon 1 - Utdata")

# Utdata er aldri heltall, selv med flere enn 365 deler.
test_that("Utdata aldri er et heltall", {
  ar_i_db_diger = sample(x = c(2017, 2018, 2019, 2020), size = 1000, replace = TRUE)
  kvart_i_db_diger = sample(x = 1:4, size = 1000, replace = TRUE)
  # expect_false(any(del_aar(ar_i_db_diger, kvart_i_db_diger, 4) == floor(del_aar(ar_i_db_diger, kvart_i_db_diger, 4))))
  expect_false(isTRUE(del_aar(ar_i_db_diger, kvart_i_db_diger, 4) == floor(del_aar(ar_i_db_diger, kvart_i_db_diger, 4))))
})

# Utdata samsvarer med forventet utverdi.
test_that("Utdata samsvarer med forventet resultat.", {
  aar = rep(2019, 4)
  kvart = c(1, 2, 3, 4)
  mnd = c(1, 7, 2, 12)
  forvent = c(2019.125, 2019.375, 2019.625, 2019.875)
  forvent12 = c(2019 + (1 / 12) * mnd - 1 / 24)
  expect_identical(del_aar(aar, kvart, 4), forvent)
  expect_equal(del_aar(aar, mnd, 12), forvent12)
})


context("Funksjon 1 - Grensetilfeller")

# Få observasjoner i årstall
test_that("Funksjonen fungerer med kun ett årstall", {
  expect_equal(del_aar(2019, 1, 4), 2019.125)
})

# Funksjonen fungerer med kun 1 del.
test_that("Funksjonen fungerer med kun en del", {
  expect_equal(del_aar(2019, 1, 1), 2019.5)
})

# Funksjonen fungerer med 366 deler
test_that("Funksjonen fungerer med 366 deler", {
  expect_equal(del_aar(2019, 1, 366), 2019 + (1 / 366 / 2))
})

# Tester - Funksjon 2 -----------------------------------------------------


context("Funksjon 2 - inndata")

# Sjekk at funksjonen gir en advarsel om dato-vektor inneholder NA:
test_that("Det gis advarsel om det finnes NA-verdier i datovektor", {
  dato_m_na = c(sample(seq(from = as.Date("2019-01-01"), to = as.Date("2021-12-31"), "days"), size = 30), NA, NA, NA)
  expect_warning(lag_periode(dato_m_na, 3), "Det finnes NA-verdier i dato-vektor",
    fixed = TRUE
  )
})

# Dato-vektor er ikke Date-format.
test_that("Det gis feilmelding om dato ikke er i Date-format", {
  ar_i_db = c(2015, 2020, 2015, 2013, 2019, 2020, 2015, 2019, 2018, 2017)
  ar_i_char = c("2015", "2020", "2015", "2013", "2019", "2020", "2015", "2019", "2018", "2017")
  expect_error(lag_periode(ar_i_db, 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
  expect_error(lag_periode(ar_i_char, 12), "Dato-vektor er ikke i Date- eller POSIXt-format")
})

# Ut-data er forskjellig for ulike klokkeslett.
test_that("Det gis ulike resultat ved forskjellige klokkeslett", {
  dato_tidlig = as.POSIXlt(c("2019-01-01 02:00:00", "2019-04-01 02:00:00", "2019-08-01 02:00:00", "2019-12-01 02:00:00"))
  dato_sent = as.POSIXlt(c("2019-01-01 21:00:00", "2019-04-01 21:00:00", "2019-08-01 21:00:00", "2019-12-01 21:00:00"))
  expect_false(any(lag_periode(dato_tidlig, 730) == lag_periode(dato_sent, 730)))
})


# Decimal_date gir ulik ut-data basert på klokkeslett i dato-vektor.
test_that("Decimal_date gir ulik ut-data ved ulike klokkeslett i dato-vektor. ", {
  dato_tidlig = as.POSIXlt(c("2019-01-01 02:00:00", "2019-04-01 02:00:00", "2019-08-01 02:00:00", "2019-12-01 02:00:00"))
  dato_sent = as.POSIXlt(c("2019-01-01 21:00:00", "2019-04-01 21:00:00", "2019-08-01 21:00:00", "2019-12-01 21:00:00"))

  dato_tidlig_ct = as.POSIXct(c("2019-01-01 02:00:00", "2019-04-01 02:00:00", "2019-08-01 02:00:00", "2019-12-01 02:00:00"))
  dato_sent_ct = as.POSIXct(c("2019-01-01 21:00:00", "2019-04-01 21:00:00", "2019-08-01 21:00:00", "2019-12-01 21:00:00"))

  expect_false(any(decimal_date(dato_tidlig) == decimal_date(dato_sent)))
})


# Sjekke at vi får samme resultat med Dato-vektor og ingen klokkeslett og Dato-vektor med klokkeslett = 12.
test_that("Vi får samme resultat med dato-vektor uten klokkeslett, og dato vektor med klokkeslett = 12", {
  dato_klokke = as.POSIXlt(c("2019-01-01 12:00:00", "2019-04-01 12:00:00", "2019-08-01 12:00:00", "2019-12-01 12:00:00"))
  dato_u_klokke = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(lag_periode(dato_u_klokke, 366), lag_periode(dato_klokke, 366))
  expect_equal(lag_periode(dato_u_klokke, 3), lag_periode(dato_klokke, 3))
  expect_equal(lag_periode(dato_u_klokke, 4), lag_periode(dato_klokke, 4))
})

# Sjekke at vi får ønsket utverdi når dato er i POSIXct format UTEN klokkeslett
test_that("Vi får ønsket utverdi når dato er i POSIXct-format uten klokkeslett", {
  dato_ct_uten_klokke = as.POSIXct(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_identical(lag_periode(dato_ct_uten_klokke, 5), lag_periode(dato_Date, 5))
})
# Sjekke at vi får ønsket utverdi når dato er i POSIXct format MED klokkeslett
test_that("Vi får ønsket utverdi når dato er i POSIXct-format uten klokkeslett", {
  dato_ct_med_klokke = as.POSIXct(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_ct_med_klokke_diff = as.POSIXct(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_ct_med_klokke, 500), lag_periode(dato_Date, 500)) # Klokken 12 blir likt for begge typer
  expect_true(any(lag_periode(dato_ct_med_klokke_diff, 500) != lag_periode(dato_ct_med_klokke, 500))) # Forskjellig klokkeslett gir forskjellig utverdi gitt korte nok interval.
})
# Sjekke at vi får ønsket utverdi når dato er i POSIXlt format UTEN klokkeslett
test_that("Vi får ønsket utverdi når dato er i POSIXlt-format uten klokkeslett", {
  dato_lt_uten_klokke = as.POSIXlt(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_lt_uten_klokke, 5), lag_periode(dato_Date, 5))
})
# Sjekke at vi får ønsket utverdi når dato er i POSIXlt format MED klokkeslett
test_that("Vi får ønsket utverdi når dato er i POSIXlt-format uten klokkeslett", {
  dato_lt_med_klokke = as.POSIXlt(c("2019-01-01 12:00", "2019-04-01 12:00", "2019-08-01 12:00", "2019-12-01 12:00"))
  dato_lt_med_klokke_diff = as.POSIXlt(c("2019-01-01 02:00", "2019-04-01 02:00", "2019-08-01 02:00", "2019-12-01 02:00"))
  dato_Date = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))

  expect_identical(lag_periode(dato_lt_med_klokke, 5), lag_periode(dato_Date, 5))
  expect_true(any(lag_periode(dato_lt_med_klokke_diff, 500) != lag_periode(dato_Date, 500)))
})



context("Funksjon 2 - Utdata")

# Utdata samsvarer med forventet resultat
test_that("Utdata samsvarer med forventet resultat, ", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  forvent = c(2019.125, 2019.125, 2019.625, 2019.875)
  forvent2 = c(2019.25, 2019.25, 2019.75, 2019.75)
  expect_identical(lag_periode(dato, 4), forvent)
  expect_identical(lag_periode(dato, 2), forvent2)
})

# Utdata er aldri heltall, selv med flere enn 365 deler.
test_that("Utdata aldri er et heltall", {
  dato_diger = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)
  dato_aarevis = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 30, replace = TRUE)
  expect_false(any(lag_periode(dato_diger, 10000) == floor(lag_periode(dato_diger, 10000)))) &
    expect_false(any(lag_periode(dato_diger, 3) == floor(lag_periode(dato_diger, 3)))) &
    expect_false(any(lag_periode(dato_aarevis, 1000) == floor(lag_periode(dato_aarevis, 1000))))
})

# Utdata er alltid et tall innen riktig år sammenlignet med dato.
test_that("Utdata aldri er et heltall", {
  dato_diger = sample(seq(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)
  expect_identical(floor(lag_periode(dato_diger, 10000)), year(dato_diger))
})

# Symmetri i utdata rundt årsskifte.
test_that("Avstand mellom ut-verdier er symmetrisk fordelt", {
  dato = as.Date(c("2019-01-01", "2019-12-31"))
  expect_identical((lag_periode(dato, 365)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 365)[2])))
  expect_identical((lag_periode(dato, 5)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 5)[2])))
  expect_identical((lag_periode(dato, 13)[1] - year(dato)[1]), ((year(dato)[1] + 1 - lag_periode(dato, 13)[2])))
})

context("Funksjon 2 - Grensetilfeller")

# Få observasjoner i årstall
test_that("Funksjonen fungerer med kun en datoverdi", {
  dato = as.Date("2019-01-01")
  expect_equal(lag_periode(dato, 4), 2019.125)
})

# Funksjonen fungerer med kun 1 del.
test_that("Funksjonen fungerer med kun en del", {
  dato = as.Date(c("2019-01-01", "2019-04-01", "2019-08-01", "2019-12-01"))
  expect_equal(lag_periode(dato, 1), c(2019.5, 2019.5, 2019.5, 2019.5))
})
