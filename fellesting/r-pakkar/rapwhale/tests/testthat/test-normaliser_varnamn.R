test_that("Funksjonen gir feilmelding om inndata ikke er tekst", {
  variabelnavn_tall = 1
  variabelnavn_faktor = as.factor("var1")
  variabelnavn_dato = as.Date("jul2622", "%b%d%y")
  
  expect_error(normaliser_varnamn(variabelnavn_tall))
  expect_error(normaliser_varnamn(variabelnavn_faktor))
  expect_error(normaliser_varnamn(variabelnavn_dato))
})

test_that("Funksjonen gir feilmelding om inndata ikke inneholder en bokstav", {
  variabelnavn_ugyldig = "._."
  variabelnavn_flere_ugyldig = c("._.", "var1", ".")
  
  feilmelding = paste0(
    "Alle variabelnavn må inneholde minst en bokstav.",
    "\nFølgende variabelnavn inneholder ingen bokstaver ",
    "(indeks i inndata i parentes):"
  )
  
  expect_error(
    normaliser_varnamn(variabelnavn_ugyldig),
    paste0(feilmelding, "\n  (1) ._."),
    fixed = TRUE
  )
  expect_error(
    normaliser_varnamn(variabelnavn_flere_ugyldig),
    paste0(feilmelding, "\n  (1) ._.\n  (3) ."),
    fixed = TRUE
  )
})

test_that("Funksjonen gir advarsel om den produserer to variabelnavn som er like", {
  variabelnavn_like = c("var1", "Var1")
  
  advarsel = "Utdata inneholder flere like variabelnavn (indeks i parentes): "
  
  expect_warning(
    normaliser_varnamn(variabelnavn_like),
    paste0(advarsel, "\n  (1,2) var1"),
    fixed = TRUE
  )
})

test_that("Funksjonen gir riktig resultat", {
  variabelnavn_ok = c("var1", "VariabelNr2")
  variabelnavn_caps = "DARLIG_VARIABELNAVN"
  
  expect_identical(
    normaliser_varnamn(variabelnavn_ok),
    c("var1", "variabel_nr2")
  )
  expect_identical(
    normaliser_varnamn(variabelnavn_caps),
    "d_a_r_l_i_g_v_a_r_i_a_b_e_l_n_a_v_n"
  )
})
