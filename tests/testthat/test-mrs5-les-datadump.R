

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
