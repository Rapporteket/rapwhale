# Testdata ---------------------------------------------------------------

kb_kanonisk = mrs5_lag_kanonisk_kb(mrs5_parse_kodebok(filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx")))

kb_flere_skjemanavn = purrr::modify_at(kb_kanonisk, "Kodebok", ~ bind_rows(
  .x,
  tibble(
    skjema_id = "Testskjema",
    skjemanavn = "Testeri",
    variabel_id = "Prima",
    variabeletikett = "Vera",
    hjelpetekst = NA,
    variabeltype = "tekst",
    obligatorisk = FALSE,
    desimaler = NA,
    regler = FALSE
  )
))

kb_flere_variabeldef = purrr::modify_at(kb_kanonisk, "Kodebok", ~ bind_rows(
  .x,
  tibble(
    skjema_id = "Testskjema",
    skjemanavn = "Testskjema",
    variabel_id = "Hoyde",
    variabeletikett = "Hoyde (cm)",
    hjelpetekst = NA,
    variabeltype = "numerisk",
    obligatorisk = FALSE,
    desimaler = NA,
    regler = FALSE
  )
))
  

kb_kanonisk_na_obligatorisk = kb_kanonisk
kb_kanonisk_na_obligatorisk$Kodebok$skjema_id[1] = NA_character_


kb_ukjent_variabeltype = purrr::modify_at(kb_kanonisk, "Kodebok", ~ bind_rows(
  .x,
  tibble(
    skjema_id = "Testskjema",
    skjemanavn = "Testskjema",
    variabel_id = "Genserfarge",
    variabeletikett = "farge på genser",
    hjelpetekst = NA,
    variabeltype = "farge",
    obligatorisk = FALSE,
    desimaler = NA,
    regler = FALSE
  )
))

kb_feil_regler = purrr::modify_at(kb_kanonisk, 
                                  "Kodebok", 
                                  ~ mutate(.x, regler = as.character(regler))) |> 
  purrr::modify_at("Kodebok", 
                   ~ bind_rows(
                     .x, 
                     tibble(
                       skjema_id = "Testskjema",
                       skjemanavn = "Testskjema",
                       variabel_id = "Genserfarge",
                       variabeletikett = "farge på genser",
                       hjelpetekst = NA,
                       variabeltype = "tekst",
                       obligatorisk = FALSE,
                       desimaler = NA,
                       regler = "Nei"
                     )
                   ))

kb_feil_obligatorisk = purrr::modify_at(kb_kanonisk, 
                                        "Kodebok", 
                                        ~ mutate(.x, 
                                                 obligatorisk = as.character(obligatorisk)))

kb_feil_desimaler = purrr::modify_at(kb_kanonisk, "Kodebok", ~ bind_rows(
  .x,
  tibble(
    skjema_id = "Testskjema",
    skjemanavn = "Testskjema",
    variabel_id = "Lengde",
    variabeletikett = "Hvor lang er pasienten?",
    hjelpetekst = NA,
    variabeltype = "numerisk",
    obligatorisk = FALSE,
    desimaler = 0.5,
    regler = FALSE
  )
))

kb_ugyldig_varnavn = purrr::modify_at(kb_kanonisk, "Kodebok", ~ bind_rows(
  .x,
  tibble(
    skjema_id = "Testskjema",
    skjemanavn = "Testskjema",
    variabel_id = "2grads_forbrenning",
    variabeletikett = "okei",
    hjelpetekst = NA,
    variabeltype = "logisk",
    obligatorisk = FALSE,
    desimaler = NA,
    regler = FALSE
  )
))

# valider_kb_skjema -------------------------------------------------------
test_that("Funksjonen gir forventet tilbakemelding", {
  
  val_feil_skjema = "Skjema_id kan bare ha ett unikt tilhørende skjemanavn."
  val_feil_variabel = "En variabel må være unikt definert innenfor et skjema."
  val_feil_obligatorisk = "Alle obligatoriske variabler må være fylt ut"
  
  expect_silent(valider_kanonisk_skjema(kb_kanonisk))
  expect_error(valider_kanonisk_skjema(kb_flere_skjemanavn), 
                   val_feil_skjema)
  expect_error(valider_kanonisk_skjema(kb_flere_variabeldef), 
                   val_feil_variabel)
  expect_error(valider_kanonisk_skjema(kb_kanonisk_na_obligatorisk), 
                   val_feil_obligatorisk)
})

# valider_kb_kolonne ------------------------------------------------------

test_that("Funksjonen gir forventet tilbakemelding", {
  val_ukjent_vartype = "Variabeltype 'farge' er ikke støttet. Må være en av: 'kategorisk', 'tekst', 'boolsk', 'dato', 'numerisk', 'kl' eller 'dato_kl'"

  val_feil_regler = "'regler' må være TRUE eller FALSE"
  val_feil_obligatorisk = "'obligatorisk' må være TRUE eller FALSE"
  val_feil_desimaler = "Desimaler må være ikke-negative heltall."
  val_ugyldig_varnavn = "Variabelnavn: '' er ikke gyldig. 
  Variabelnavn må kun inneholde bokstaver, tall og '_'. Kan ikke starte med et tall."


  expect_error(
    valider_kanonisk_kolonner(kb_ukjent_variabeltype),
    val_ukjent_vartype
  )
  expect_error(
    valider_kanonisk_kolonner(kb_feil_regler),
    val_feil_regler
  )
  expect_error(
    valider_kanonisk_kolonner(kb_feil_obligatorisk),
    val_feil_obligatorisk
  )
  expect_error(
    valider_kanonisk_kolonner(kb_feil_desimaler),
    val_feil_desimaler
  )
  expect_error(
    valider_kanonisk_kolonner(kb_ugyldig_varnavn),
    val_ugyldig_varnavn
  )
  expect_silent(valider_kanonisk_kolonner(kb_kanonisk))
})

# valider_kb_variabler ----------------------------------------------------


