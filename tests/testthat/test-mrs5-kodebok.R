# Hovedfunksjon -----------------------------------------------------------


# Parse-funksjoner --------------------------------------------------------

# mrs5_parse_kodebok ------------------------------------------------------

# mrs5_parse_kodebok_meta -----------------------------------------------

# Argumenter
test_that("typekontroll filsti", {
  
  feilmelding_feil_type_filsti = "Filsti må være en tekststreng"

  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = 2L, 
      skjemanavn = NULL
    ),
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = 2.5, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = NULL, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = TRUE, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
})

test_that("typekontroll_skjemanavn", {
  
  feilmelding_feil_type_skjemanavn = "skjemanavn må være NULL eller en tekst-vektor"

  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = 1
    ), 
    feilmelding_feil_type_skjemanavn
    )
  
  expect_error(
    mrs5_parse_kodebok_meta(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = TRUE
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
})

test_that("Gir feilmelding hvis skjemanavn ikke eksisterer i kodebok", {
feilmelding_feil_skjemanavn = "Skjemanavn finnes ikke i kodebok"

expect_error(
  mrs5_parse_kodebok_meta(
    filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
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
    `...2` = c("Testskjema", "1", NA, "11", "5", "Innlagt", "CreationDate",
               "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja", NA, "Navn", 
               "Versjon 1", "Versjon 2"),
    `...3` = c(rep(NA, 15), "Dato", "01.03.2020 12:00", "01.05.2021 12:00"),
    `...4` = c(rep(NA, 15), "Kan opprettes", "Nei", "Ja"),
    `...5` = c(rep(NA, 15), "Kan endres", "Ja", "Ja"),
    `...6` = c(rep(NA, 15), "Kan slettes", "Ja", "Ja")
  )

  # Teste henting av info for ett enkelt skjema
  expect_identical(
    mrs5_parse_kodebok_meta(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "Testskjema"
    ),
    kb_skjema_raa
  )
})
  
# mrs5_parse_kodebok_felter -----------------------------------------------

# Argumenter 
test_that("typekontroll filsti", {
  
  feilmelding_feil_type_filsti = "Filsti må være en tekststreng"
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = 2L, 
      skjemanavn = NULL
    ),
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = 2.5, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = NULL, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = TRUE, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
})

test_that("typekontroll_skjemanavn", {
  
  feilmelding_feil_type_skjemanavn = "skjemanavn må være NULL eller en tekst-vektor"
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = 1
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = TRUE
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
})

test_that("Gir feilmelding hvis skjemanavn ikke eksisterer i kodebok", {
  feilmelding_feil_skjemanavn = "Skjemanavn finnes ikke i kodebok"
  
  expect_error(
    mrs5_parse_kodebok_felter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "feil navn"
    ),
    feilmelding_feil_skjemanavn
  )
})

# Utdata
test_that("Gir forventet resultat", {
  
  kb_felter_raa = tibble::tibble(
    Skjemanavn = c(rep("Testskjema", 11)), 
    Variabelnavn = c("PasientGUID", "Skjematype", "UnitId", "PatientAge", 
                     "Hoyde", "CreationDate", "Innlagt", "FormStatus", 
                     "PatientGender", "Hoyde", "HoydeUkjent"),
    Visningstekst = c("PasientGUID", "Skjematype", "UnitId", "PatientAge", 
                      "Høyde (cm)", "Opprettet", "Innlagt", "FormStatus", 
                      "PatientGender", "Høyde (cm)", "Høyde ukjent"),
    `Unik teknisk referanse` = c("PatientInRegistryGuid", "FormTypeName", 
                                 "UnitId", "PatientAge", "Hoyde", "CreationDate", 
                                 "Innlagt", "FormStatus", "PatientGender", 
                                 "Hoyde", "HoydeUkjent"),
    `Mulige verdier` = c(NA, NA, NA, NA, NA, NA, NA, 
                         "0 = Ingen, 1 = Kladd, 2 = Ferdigstilt, 4 = Slettet, 5 = Returnert", 
                         "0 = Ukjent, 1 = Mann, 2 = Kvinne", NA, NA),
    Felttype = c("String", "String", "Number", "Number", "Number", "DateTime", 
                 "DateTime", "Enum", "Enum", "Number", "Bool"),
    Kodeverk = c(rep(NA_character_, 11)), 
    Kjernefelt = c("Ja", "Ja", "Ja", "Ja", "Nei", "Ja", "Ja", 
                   "Ja", "Ja", "Nei", "Nei"),
    Identifiserbar = c("Nei", "Nei", "Nei", "Nei", NA, "Nei", 
                       "Nei", "Nei", "Nei", NA, NA),
    `Gyldig fra og med skjemaversjon` = c("1", "1", "1", "1", "10", "1", 
                                         "1", "1", "1", "10", "10"),
    `Fjernet fra og med skjemaversjon` = c(rep(NA_character_, 11)),
    `Tillat kun positive verdier` = c(rep(NA_character_, 11)),
    Hjelpetekst = c(rep(NA_character_, 11)),
    `Skal vises i registeruttrekk` = c(rep("Ja", 11)),
    `Skal eksporteres til Hap Helsedata` =  c(rep("Ja", 11)),
    `Gammelt navn` = c(rep(NA_character_, 11)),
    Beskrivelse = c("GUID til pasienten, ulik lokalt og nasjonalt", 
                    "Navnet til skjematypen", 
                    "ID til enheten som skjemaet er opprettet på", 
                    "PatientAge blir kalkulert ved (hver) skjemalagring. Den beregnes ut i fra hva skjematypen har definert som PropertyForAgeCalculation. Er ikke denne definert, brukes opprettelsesdatoen på skjemaet.", 
                    NA, "Dato og tidspunkt skjemaet ble opprettet", 
                    "Dato og tidspunkt pasienten ble innlagt", "Skjemaets status", 
                    "Pasientens kjønn gitt fra Personregisteret", NA, NA),
    `Patientvennlig term` = c(rep(NA_character_, 11)),
    Tema = c(rep(NA_character_, 11)),
    Kommentar = c(rep(NA_character_, 11)),
    `Intern kommentar` = c(rep(NA_character_, 11)),
    Feltkilde = c("Calculated", "Calculated", "Calculated", "Calculated", 
                  "DirectFromForm", "Calculated", "DirectFromForm", 
                  "Calculated", "Calculated", "DirectFromForm", 
                  "DirectFromForm")
  )
  
  expect_identical(
    mrs5_parse_kodebok_felter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "Testskjema"
    ),
    kb_felter_raa
  )
})
# mrs5_parse_kodebok_regler -----------------------------------------------
test_that("typekontroll filsti", {
  
  feilmelding_feil_type_filsti = "Filsti må være en tekststreng"
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = 2L, 
      skjemanavn = NULL
    ),
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = 2.5, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = NULL, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = TRUE, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
})

test_that("typekontroll_skjemanavn", {
  
  feilmelding_feil_type_skjemanavn = "skjemanavn må være NULL eller en tekst-vektor"
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = 1
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = TRUE
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
})

test_that("Gir feilmelding hvis skjemanavn ikke eksisterer i kodebok", {
  feilmelding_feil_skjemanavn = "Skjemanavn finnes ikke i kodebok"
  
  expect_error(
    mrs5_parse_kodebok_regler(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "feil navn"
    ),
    feilmelding_feil_skjemanavn
  )
})

# Utdata
test_that("Gir forventet resultat", {

  kb_regler_raa = tibble::tibble(
    Skjemanavn = rep("Testskjema", 5),
    Id = c("1000", "1008", "1005", "1058", "1011"),
    Eiertype = c("Field", "Field", "Field", "Field", "Field"),
    Eier  = c("Innlagt", "Innlagt", "Innlagt", "HoydeUkjent", "Innlagt"),
    Regeltype = c("105", "107", "109", "42", "100"),
    `Regeltype navn` = c("Påkrevd", "Er større enn eller lik gitt verdi", 
                       "Er mindre enn eller lik gitt verdi", "Skjul hvis", 
                       "Registerspesifikk validering"), 
    Melding = c("Feltet må besvares.", 
                "Feltet må være større enn eller lik 01.01.2020 00:00:00", 
                "Feltet må være mindre enn eller lik 16.04.2024 10:59:26", 
                "Feltet er skjult hvis: Feltet (Hoyde) må besvares.", 
                "Innleggelsesdato kan ikke være senere enn CreationDate."),
    Forklaring = c("Feltet (Innlagt) må besvares.", 
                   "Feltet (Innlagt) må være større enn eller lik 01.01.2020 00:00:00.", 
                   "Feltet (Innlagt) må være mindre enn eller lik 16.04.2024 10:59:26.", 
                   "Feltet (HoydeUkjent) er skjult hvis: Feltet (Hoyde) må besvares.", 
                   "Innleggelsesdato kan ikke være senere enn CreationDate."),
    `Gyldig fra og med skjemaversjon` = c("0", "0", "0", "0", "0"),
    `Fjernet fra og med skjemaversjon` = c(rep(NA_character_, 5)),
    Sammenligningsverdi = c(NA, "43831", "45398.457943530098", NA, NA),
    Konsekvens = c("Error", "Error", "Error", "Info", "Error")
    )
  
  expect_identical(
    mrs5_parse_kodebok_regler(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "Testskjema"
    ),
    kb_regler_raa 
  )
  
})
# mrs5_kombiner_parsed ----------------------------------------------------

# typekontroll
test_that("Gir forventet feilmelding hvis inndata er feil type", {
  
})

test_that("Gir forventet utdata", {

})

# Hjelpefunksjoner for parse ----------------------------------------------

# mrs5_trekk_ut_skjemanavn ------------------------------------------------


# Konverter til kanonisk --------------------------------------------------

# mrs5_hent_metadata -----------------------------------------------------

# Forventet utdata
kb_raa_test = list(
  versjonslogg = tibble::tibble(
    Skjemanavn = c("Testskjema", "Testskjema"),
    versjonsnummer = c("1", "2"),
    navn = c("Versjon 1", "Versjon 2"),
    dato = c("01.03.2020 12:00", "01.05.2021 12:00"),
    kan_opprettes = c("Nei", "Ja"),
    kan_endres = c("Ja", "Ja"),
    kan_slettes = c("Ja", "Ja")
  ),
  metainfo = tibble::tibble(
    metavariabel = c("skjematypenavn", "skjematype_ID",
                     "foreldreskjematype_ID", "antall_felter", "antall_regler",
                     "skjemadato_hentes_fra_felt",
                     "aldersberegning_skjer_i_forhold_til_felt",
                     "er_ePROM_skjematype", "tilgjengelig_i_skjemasok",
                     "tilgjengelig_i_skjemaopprettelsesdialog",
                     "tilgjengelig_i_skjemaeksport", "tilgjengelig_i_rapporter",
                     "vises_pa_pasientsiden", "vises_i_skjematellinger"
    ),
    metaverdi = c("Testskjema", "1", NA_character_, "11", "5", "Innlagt", "CreationDate",
                  "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")
  )
)

kb_test_versjonslogg = kb_raa_test[['versjonslogg']]

kb_test_metainfo = kb_raa_test[['metainfo']] |> 
  tidyr::pivot_wider(names_from = "metavariabel", 
                     values_from = "metaverdi") |> 
  janitor::clean_names()

test_that("Hent_versjonslogg gir forventet utdata", {

  expect_identical(
    mrs5_hent_versjonslogg(
      parsed_generelt = mrs5_parse_kodebok_meta(
        filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
        skjemanavn = "Testskjema"
      )
    ),
    kb_test_versjonslogg
  )
})


test_that("Hent_metainfo gir forventet utdata", {
  
  expect_identical(
    mrs5_hent_metainfo(
      parsed_generelt = mrs5_parse_kodebok_meta(
        filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
        skjemanavn = "Testskjema"
      )
    ),
    kb_test_metainfo
  )
})

# mrs5_hent_versjonslogg --------------------------------------------------

# Typekontroll
test_that("Feilmelding hvis inndata ikke er tibble", {
  
})

test_that("Feilmelding hvis inndata ikke er riktig strukturert", {
  
})

# Utdata 
test_that("Gir ut forventet resultat", {
  
})

# mrs5_hent_metainfo ------------------------------------------------------

# Hjelpefunksjoner kanonisk -----------------------------------------------

# Validering --------------------------------------------------------------

# kb_skjema_raa_flere_skjema = list(
#   versjonslogg = tibble::tibble(
#     skjemanavn = c("Testskjema", "Testskjema", "Sluttskjema", "Sluttskjema", "Sluttskjema"),
#     versjonsnummer = c(1L, 2L, 1L, 2L, 3L), 
#     navn = c("Versjon 1", "Versjon 2", "Versjon 1", "Versjon 2", "Versjon 3"), 
#     dato = lubridate::as_datetime(c("2020-01-01 12:00:00", "2021-01-01 12:00:00", 
#                                     "2020-01-01 12:00:00", "2021-01-01 12:00:00", 
#                                     "2022-01-01 12:00:00")), 
#     kan_opprettes = c("Nei", "Ja", "Nei", "Nei", "Ja"),
#     kan_endres = c(rep("Ja", 5)),	
#     kan_slettes = c(rep("Ja", 5))
#   ),
#   metainfo = tibble::tibble(
#     skjematypenavn = c("Testskjema", "Sluttskjema"), 
#     skjematype_ID = c(1L, 2L),
#     foreldreskjematype_ID = c(NA, 1L), 
#     antall_felter = c(12L, 12L), 
#     antall_regler = c(5L, 5L),
#     skjemadato_hentes_fra_felt = c("Innlagt", "Utskrevet"),
#     aldersberegning_skjer_i_forhold_til_felt = c("Innlagt", "Utskrevet"),
#     er_ePROM_skjematype = c("Nei", "Nei"), 
#     tilgjengelig_i_skjemasok = c("Ja", "Ja"),
#     tilgjengelig_i_skjemaopprettelsesdialog = c("Ja", "Ja"),
#     tilgjengelig_i_skjemaeksport = c("Ja", "Ja"), 
#     tilgjengelig_i_rapporter = c("Ja", "Ja"),
#     vises_paa_pasientsiden = c("Ja", "Ja"), 
#     vises_i_skjematellinger = c("Ja", "Ja")
#   )
# )
# 
# 
# # Teste henting av info for alle skjema
# expect_identical(
#   mrs5_parse_kodebok_meta(
#     filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
#     skjemanavn = NULL
#   ),
#   kb_skjema_raa_flere_skjema
# )
# 
# # Teste henting av info for alle skjema navngitt
# expect_identical(
#   mrs5_parse_kodebok_meta(
#     filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
#     skjemanavn = NULL
#   ),
#   mrs5_parse_kodebok_meta(
#     filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
#     skjemanavn = c("Testskjema", "Sluttskjema")
#   )
# )
# 
# })
