# Hovedfunksjon -----------------------------------------------------------


# Parse-funksjoner --------------------------------------------------------


# mrs5_parse_kodebok ------------------------------------------------------

# mrs5_parse_kodebok_skjema -----------------------------------------------

# Argumenter
test_that("typekontroll filsti", {
  
  feilmelding_feil_type_filsti = "Filsti må være en tekststreng"

  expect_error(
    mrs5_parse_kodebok_skjema(
      filsti = 2L, 
      skjemanavn = NULL
    ),
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_skjema(
      filsti = 2.5, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_skjema(
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_skjema(
      filsti = TRUE, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
})

test_that("typekontroll_skjemanavn", {
  
  feilmelding_feil_type_skjemanavn = "skjemanavn må være NULL eller en tekst-vektor"

  expect_error(
    mrs5_parse_kodebok_skjema(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_skjema_ok.csv"), 
      skjemanavn = 1
    ), 
    feilmelding_feil_type_skjemanavn
    )
  
  expect_error(
    mrs5_parse_kodebok_skjema(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_skjema_ok.csv"), 
      skjemanavn = TRUE
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
})

test_that("Gir feilmelding hvis skjemanavn ikke eksisterer i kodebok", {
feilmelding_feil_skjemanavn = "Skjemanavn finnes ikke i kodebok"

expect_error(
  mrs5_parse_kodebok_skjema(
    filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_skjema_ok.csv"),
    skjemanavn = "feil navn"
  ),
  feilmelding_feil_skjemanavn
)
})

# Utdata
test_that("Gir forventet resultat", {
  
  kb_skjema_raa = tibble::tibble(
    `...1` = c("Skjematypenavn", "Skjematype-ID", "Foreldreskjematype-ID", 
               "Antall felter", "Antall regler", "Skjemadato hentes fra felt", 
               "Aldersberegning skjer i forhold til felt", "Er ePROM skjematype", 
               "Tilgjengelig i skjemasøk", "Tilgjengelig i skjemaopprettelsesdialog",
               "Tilgjengelig i skjemaeksport", "Tilgjengelig i rapporter", 
               "Vises på pasientsiden", "Vises i skjematellinger", NA, 
               "Versjonsnummer", "1", "2"),
    `...2` = c("Testskjema", "1", NA, "12", "5", "Innlagt", "CreationDate",
               "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", NA, "Navn", 
               "Versjon 1", "Versjon 2"),
    `...3` = c(rep(NA, 15), "Dato", "01.03.2020 12:00", "01.05.2021 12:00"),
    `...4` = c(rep(NA, 15), "Kan opprettes", "Nei", "Ja"),
    `...5` = c(rep(NA, 15), "Kan endres", "Ja", "Ja"),
    `...6` = c(rep(NA, 15), "Kan slettes", "Ja", "Ja")
  )

  # Teste henting av info for ett enkelt skjema
  expect_identical(
    mrs5_parse_kodebok_skjema(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.csv"),
      skjemanavn = "Testskjema"
    ),
    kb_skjema_raa
  )
})
  
# mrs5_parse_kodebok_felter -----------------------------------------------

# Argumenter 

# Utdata 




# mrs5_parse_kodebok_regler -----------------------------------------------

# mrs5_kombiner_parsed ----------------------------------------------------


# Hjelpefunksjoner for parse ----------------------------------------------

# mrs5_hent_metadata

# Forventet utdata
kb_skjema_raa_ett_skjema = list(
  versjonslogg = tibble::tibble(
    versjonsnummer = c(1L, 2L), 
    navn = c("Versjon 1", "Versjon 2"), 
    dato = lubridate::as_datetime(c("2020-01-01 12:00:00", "2021-01-01 12:00:00")), 
    kan_opprettes = c(FALSE, TRUE),
    kan_endres = c(TRUE, TRUE),	
    kan_slettes = c(TRUE, TRUE)
  ),
  metainfo = tibble::tibble(
    metavariabel = c("skjematypenavn", "skjematype_ID", 
                     "foreldreskjematype_ID", "antall_felter", "antall_regler", 
                     "skjemadato_hentes_fra_felt", 
                     "aldersberegning_skjer_i_forhold_til_felt",
                     "er_ePROM_skjematype", "tilgjengelig_i_skjemasok",
                     "tilgjengelig_i_skjemaopprettelsesdialog",
                     "tilgjengelig_i_skjemaeksport", "tilgjengelig_i_rapporter",
                     "vises_paa_pasientsiden", "vises_i_skjematellinger"
    ),
    metaverdi = c("Testskjema", "1", "", "12", "5", "Innlagt", "CreationDate",
                  "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")
  )
)

test_that("Gir forventet utdata", {
  
  expect_identical(
    mrs5_hent_metadata(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_feil_skjema.csv"),
      skjemanavn = "Testskjema"
    ), 
    kb_skjema_raa_ett_skjema 
  )
})

# Feil format skjema
test_that("Gir forventet feilmelding ved feil format på kodebok", {
  
  feilmelding_feil_struktur = "Kodebok må være på MRS5-struktur"
  
  expect_error(
    mrs5_hent_metadata(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_feil_skjema.csv"),
      skjemanavn = "Testskjema"
    ),
    feilmelding_feil_struktur)
})

# Konverter til kanonisk --------------------------------------------------


# Hjelpefunksjoner kanonisk -----------------------------------------------


# Validering --------------------------------------------------------------



kb_skjema_raa_flere_skjema = list(
  versjonslogg = tibble::tibble(
    skjemanavn = c("Testskjema", "Testskjema", "Sluttskjema", "Sluttskjema", "Sluttskjema"),
    versjonsnummer = c(1L, 2L, 1L, 2L, 3L), 
    navn = c("Versjon 1", "Versjon 2", "Versjon 1", "Versjon 2", "Versjon 3"), 
    dato = lubridate::as_datetime(c("2020-01-01 12:00:00", "2021-01-01 12:00:00", 
                                    "2020-01-01 12:00:00", "2021-01-01 12:00:00", 
                                    "2022-01-01 12:00:00")), 
    kan_opprettes = c("Nei", "Ja", "Nei", "Nei", "Ja"),
    kan_endres = c(rep("Ja", 5)),	
    kan_slettes = c(rep("Ja", 5))
  ),
  metainfo = tibble::tibble(
    skjematypenavn = c("Testskjema", "Sluttskjema"), 
    skjematype_ID = c(1L, 2L),
    foreldreskjematype_ID = c(NA, 1L), 
    antall_felter = c(12L, 12L), 
    antall_regler = c(5L, 5L),
    skjemadato_hentes_fra_felt = c("Innlagt", "Utskrevet"),
    aldersberegning_skjer_i_forhold_til_felt = c("Innlagt", "Utskrevet"),
    er_ePROM_skjematype = c("Nei", "Nei"), 
    tilgjengelig_i_skjemasok = c("Ja", "Ja"),
    tilgjengelig_i_skjemaopprettelsesdialog = c("Ja", "Ja"),
    tilgjengelig_i_skjemaeksport = c("Ja", "Ja"), 
    tilgjengelig_i_rapporter = c("Ja", "Ja"),
    vises_paa_pasientsiden = c("Ja", "Ja"), 
    vises_i_skjematellinger = c("Ja", "Ja")
  )
)


# Teste henting av info for alle skjema
expect_identical(
  mrs5_parse_kodebok_skjema(
    filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.csv"),
    skjemanavn = NULL
  ),
  kb_skjema_raa_flere_skjema
)

# Teste henting av info for alle skjema navngitt
expect_identical(
  mrs5_parse_kodebok_skjema(
    filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.csv"),
    skjemanavn = NULL
  ),
  mrs5_parse_kodebok_skjema(
    filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.csv"),
    skjemanavn = c("Testskjema", "Sluttskjema")
  )
)

})
