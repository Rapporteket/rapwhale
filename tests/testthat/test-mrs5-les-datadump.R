

# mrs5_konverter_til_logisk -----------------------------------------------

# Gir forventet resultat
test_that("Funksjonen returnerer forventet resultat", {
  
  # Gyldig streng 
  test_ok = c("1", "0", "1", "")
  test_ok_na = c("1", "0", "1", NA_character_)
  test_ok_negativ = c("1", "0", "1", "-1")
  test_ok_tekst = c("True", "False", "TRUE", NA_character_)
  
  # Ugyldig streng
  test_feil = c("1", "0", "2")
  
  # Ugyldig type (numerisk)
  test_numerisk = c(1,0,1,1)
  
  # Allerede logisk
  test_logisk = c(TRUE, FALSE, TRUE, NA)
  
  ####
  test_ok_res = c(TRUE, FALSE, TRUE, NA)
  test_feil_type_res = "Inndata må være en tekst-vektor."
  test_feil_res = "Forventet inndata er en av: '-1', '0', '1', 'True', 'False', 'TRUE', 'FALSE'."
  
  expect_equal(mrs5_konverter_til_logisk(test_ok), test_ok_res)
  expect_equal(mrs5_konverter_til_logisk(test_ok_na), test_ok_res)
  expect_equal(mrs5_konverter_til_logisk(test_ok_negativ), test_ok_res)
  expect_equal(mrs5_konverter_til_logisk(test_ok_tekst), test_ok_res)
  expect_error(mrs5_konverter_til_logisk(test_feil), test_feil_res)
  expect_error(mrs5_konverter_til_logisk(test_numerisk), test_feil_type_res)
  expect_equal(mrs5_konverter_til_logisk(test_logisk), test_ok_res)
  
})


# mrs5_håndter_dato_kl ----------------------------------------------------

# typekontroll
test_feil_type = c(2L, 3L)
feilmelding_feil_type = "Inndata må være en tekst-vektor."

# dato dmy
dato_punktum_dmy = "12.01.2024"
dato_strek_dmy = "12-01-2024"
dato_tekst_dmy = "12. jan, 2024"

# dato ymd
dato_punktum_ymd = "2024.01.12"
dato_strek_ymd = "2024-01-12"
dato_tekst_ymd = "2024, jan-12"

# dato_kl_dmy
dato_kl_punktum_dmy = "12.01.2024 12:00"
dato_kl_strek_dmy = "12-01-2024 12:00"

# dato_kl ymd
dato_kl_punktum_ymd = "2024.01.12 12:00"
dato_kl_strek_ymd = "2024-01-12 12:00"

dato_all_na = rep(NA_character_, 5)
dato_all_na_not = c(dato_all_na, "01-01-2024")
dato_kl_all_na_not = c(dato_all_na, "01-01-2024 12:00")

dato_res = as.POSIXct("2024-01-12", tz = "UTC")
dato_kl_res = as.POSIXct("2024-01-12 12:00:00", tz = "UTC")

feilmelding_dato = "Klarte ikke å finne dato med forventet format: 'YYYY.MM.DD'."
feilmelding_dato_kl = "Klarte ikke å finne dato-klokkeslett med forventet format: 'YYYY.MM.DD HH:MM'."

# Gir forventet resultat
test_that("Funksjonen gir forventet resultat", {
  # dato
  expect_equal(mrs5_håndter_dato_kl(dato_punktum_dmy), dato_res)
  expect_equal(mrs5_håndter_dato_kl(dato_strek_dmy), dato_res)

  # dato_kl
  expect_equal(mrs5_håndter_dato_kl(dato_kl_punktum_dmy), dato_kl_res)
  expect_equal(mrs5_håndter_dato_kl(dato_kl_strek_dmy), dato_kl_res)
})

test_that("Funksjonen gir forventet feilmelding ved feil dato-format", {
  # Feil format dato ymd 
  expect_equal(mrs5_håndter_dato_kl(dato_punktum_ymd), dato_res)
  expect_equal(mrs5_håndter_dato_kl(dato_strek_ymd), dato_res)

  # Feil format dato_kl ymd
  expect_equal(mrs5_håndter_dato_kl(dato_kl_punktum_ymd), dato_kl_res)
  expect_equal(mrs5_håndter_dato_kl(dato_kl_strek_ymd), dato_kl_res)
})

test_that("Funksjonen gir forventet feilmelding ved feil dataformat", {
  # Feil type
  expect_error(mrs5_håndter_dato_kl(test_feil_type), feilmelding_feil_type)
})

