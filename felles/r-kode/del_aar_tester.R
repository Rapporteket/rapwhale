# tester for tidslinje-funksjon:

# Testdata ----------------------------------------------------------------

# Enkeltstående år
ar = 2019

# Vektor med årstall
arevis = c(2019:2021)

# Vektor med manglende årstall
ar_med_hull = c(2019, 2020, 2022, 2023)

# Vektor med NA årstall
ar_med_na = c(2019, 2020, NA, 2022)

# Vektor med desimal-årstall:
ar_med_des = c(2019, 2020.5, NA, 2022)

# Vektor med årstall i tekst:
ar_med_tekst = c("tjueatten", "tjuenitten", "tjuetjue")

# antall deler:
ad = 4
ad_desi = 4.2
ad_desi_hel = 4.0

# Dato vektor, daglig:
dagevis = seq.Date(from = as.Date("2018/1/1"), as.Date("2019/10/10"), "days")
ukevis = seq.Date(from = as.Date("2018/1/1"), as.Date("2019/10/10"), "weeks")
mndvis = seq.Date(from = as.Date("2018/1/1"), as.Date("2019/10/10"), "months")


# Dato-uttrekk, inkludert 0101 og 3112:
date = c(seq.Date(from = as.Date("2018-01-01"), to = as.Date("2019-01-02"), "days"))

dato_sample_m_nyttar = c(
  as.Date("2019-01-01"), sample(date, size = 15), as.Date("2019-12-31"),
  as.Date("2020-01-01"), as.Date("2020-01-02")
)

# Tilfeldige datoer fra flere år
dato_arevis = sample(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 30)
dato_diger = sample(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2025-12-31"), "days"), size = 3000, replace = TRUE)

# Dato fra flere år hvor alle mangler OKT, NOV, DES.
dato_ar_m_hull = c(
  sample(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-09-30"), "days"), size = 10),
  sample(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-09-30"), "days"), size = 10),
  sample(seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-09-30"), "days"), size = 10)
)

# Dato med NA-verdier:
dato_m_na = c(
  sample(seq.Date(from = as.Date("2019-01-01"), to = as.Date("2019-12-31"), "days"), size = 30),
  c(NA, NA, NA)
)

# Dato med skuddår:
dato_m_skudd = c(
  sample(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), "days"), size = 300),
  as.Date("2020-02-29")
)

# Dato rundt halvårs-skifte:
dato_halv = seq.Date(from = as.Date("2019-05-01"), to = as.Date("2019-07-31"), "days")

# Generere datasett som tilsvarer hva en ser i en database:
ar_i_db = sample(x = c(2017, 2018, 2019), size = 10, replace = TRUE)
ar_i_db_diger = sample(x = c(2017, 2018, 2019, 2020), size = 1000, replace = TRUE)
ar_med_gap = c(rep(2017:2019, each = 4))
ar_i_db_na = sample(x = c(NA_integer_, 2018, 2019), size = 10, replace = TRUE)
ar_m_des = c(sample(x = c(2017, 2018, 2019), size = 9, replace = TRUE), 2018.5)
ar_m_des2 = c(sample(x = c(2017, 2018, 2019), size = 9, replace = TRUE), 2018.0)

# Delnummer-vektor hentet fra database:
kvart_i_db = sample(x = 1:4, size = 10, replace = TRUE)
kvart_i_db_diger = sample(x = 1:4, size = 1000, replace = TRUE)
kvart_med_gap = c(1, 2, 3, 4, 1, 3, 3, 4, 1, 2, 3, 4)
kvart_uten_3 = c(1, 2, 2, 4, 1, 2, 4, 1, 2, 4)
kvart_uten_4 = c(1, 2, 2, 3, 1, 2, 3, 1, 2, 3)
kvart_i_db_na = c(sample(x = 1:4, size = 9, replace = TRUE), NA_integer_)
mnd_i_db = sample(x = 1:12, size = 10, replace = TRUE)



# Tester - Funksjon 1 -----------------------------------------------------

context("Funksjon 1 - inndata")

# Sjekk at det gis feilmelding om årstall inneholder desimal:
test_that("Det gis feilmelding om årstall-vektor ikke er heltallsvektor", {
  expect_error(del_aar(ar_m_des, kvart_i_db, 4))
})

# Sjekk at det gis feilmelding om antall_deler ikke er et heltall.
test_that("Det gis feilmelding om antall_deler ikke er et heltall", {
  expect_error(del_aar(ar_i_db, kvart_i_db, 4.6), "Antall deler er ikke et heltall.")
})

# Sjekk at funksjonen gir en advarsel om årstall inneholder NA:
test_that("Det gis advarsel om det finnes NA-verdier i årstall", {
  expect_warning(del_aar(ar_i_db_na, kvart_i_db, 4), "År inneholder NA-verdier",
    fixed = TRUE
  )
})

context("Funksjon 1 - Utdata")

# Utdata er aldri heltall, selv med flere enn 365 deler.
test_that("Utdata aldri er et heltall", {
  expect_false(any(del_aar(ar_i_db_diger, kvart_i_db_diger, 4) == floor(del_aar(ar_i_db_diger, kvart_i_db_diger, 4))))
})

context("Funksjon 1 - Grensetilfeller")

# Få observasjoner i årstall
test_that("Funksjonen fungerer med kun ett årstall", {
  expect_equal(del_aar(2019, 1, 4), 2019.125)
})

# Tester - Funksjon 2 -----------------------------------------------------


context("Funksjon 2 - inndata")

# Sjekk at funksjonen gir en advarsel om dato-vektor inneholder NA:
test_that("Det gis advarsel om det finnes NA-verdier i datovektor", {
  expect_warning(lag_periode(dato_m_na, 3), "Det finnes NA-verdier i dato-vektor",
    fixed = TRUE
  )
})


# Kjører del_aar gitt antall_deler er 4 eller 12.
# Forventer at del_aar blir kalt med de ønskede lubridatefunksjonene for å hente ut riktig delnummer fra hver dato.
test_that("Det gis samme output når antall_deler er 4 for lag_periode og del_aar", {
  expect_identical(lag_periode(dato_arevis, 4), del_aar(year(dato_arevis), quarter(dato_arevis), 4))
})

# Dele opp i mnd
test_that("Det gis samme output når antall_deler er 12 lag_periode og del_aar", {
  expect_identical(lag_periode(dato_arevis, 12), del_aar(year(dato_arevis), month(dato_arevis), 12))
})

# Dato-vektor er ikke Date-format.
test_that("Det gis feilmelding om dato ikke er i Date-format", {
  expect_error(lag_periode(ar_i_db, 12), "Dato-vektor er ikke i Date-format")
})


context("Funksjon 2 - Utdata")

# Utdata er aldri heltall, selv med flere enn 365 deler.
test_that("Utdata aldri er et heltall", {
  expect_false(any(lag_periode(dato_diger, 10000) == floor(lag_periode(dato_diger, 10000)))) &
    expect_false(any(lag_periode(dato_diger, 3) == floor(lag_periode(dato_diger, 3)))) &
    expect_false(any(lag_periode(dato_arevis, 1000) == floor(lag_periode(dato_arevis, 1000))))
})

# Utdata er alltid et tall innen riktig år sammenlignet med dato.
test_that("Utdata aldri er et heltall", {
  expect_identical(floor(lag_periode(dato_diger, 10000)), year(dato_diger))
})
