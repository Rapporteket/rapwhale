# Hovedfunksjon -----------------------------------------------------------

# Test-objekter -----------------------------------------------------------

# Kodebok-rådata 

{
kb_meta_raa = list(
  kb_test_meta_raa = tibble::tibble(
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
)
} # kb_meta_raa - metaskjema raa

{
kb_felter_raa = list(
  kb_test_felter_raa = tibble::tibble(
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
    ),
  kb_slutt_felter_raa = tibble::tibble(
    Skjemanavn = c(rep("Sluttskjema", 11)), 
    Variabelnavn = c("PasientGUID", "Skjematype", "UnitId", "PatientAge", 
                     "Hoyde", "CreationDate", "Innlagt", "FormStatus", 
                     "PatientGender", "HoydeUkjent", "VektUkjent"),
    Visningstekst = c("PasientGUID", "Skjematype", "UnitId", "PatientAge", 
                      "Høyde (cm)", "Opprettet", "Innlagt", "FormStatus", 
                      "PatientGender", "Høyde ukjent", "Vekt ukjent"),
    `Unik teknisk referanse` = c("PatientInRegistryGuid", "FormTypeName", 
                                 "UnitId", "PatientAge", "Hoyde", "CreationDate", 
                                 "Innlagt", "FormStatus", "PatientGender", 
                                 "HoydeUkjent", "VektUkjent"),
    `Mulige verdier` = c(NA, NA, NA, NA, NA, NA, NA, 
                         "0 = Ingen, 1 = Kladd, 2 = Ferdigstilt, 4 = Slettet, 5 = Returnert", 
                         "0 = Ukjent, 1 = Mann, 2 = Kvinne", NA, NA),
    Felttype = c("String", "String", "Number", "Number", "Number", "DateTime", 
                 "DateTime", "Enum", "Enum", "Bool", "Bool"),
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
)
} # kb_felter_raa

{kb_regler_raa = list(
  kb_test_regler_raa = tibble::tibble(
    Skjemanavn = rep("Testskjema", 5),
    Id = c("1000", "1008", "1005", "1058", "1011"),
    Eiertype = c("Field", "Field", "Field", "Field", "Field"),
    Eier = c("Innlagt", "Innlagt", "Innlagt", "HoydeUkjent", "Innlagt"),
    Regeltype = c("105", "107", "109", "42", "100"),
    `Regeltype navn` = c(
      "Påkrevd", "Er større enn eller lik gitt verdi",
      "Er mindre enn eller lik gitt verdi", "Skjul hvis",
      "Registerspesifikk validering"
    ),
    Melding = c(
      "Feltet må besvares.",
      "Feltet må være større enn eller lik 01.01.2020 00:00:00",
      "Feltet må være mindre enn eller lik 16.04.2024 10:59:26",
      "Feltet er skjult hvis: Feltet (Hoyde) må besvares.",
      "Innleggelsesdato kan ikke være senere enn CreationDate."
    ),
    Forklaring = c(
      "Feltet (Innlagt) må besvares.",
      "Feltet (Innlagt) må være større enn eller lik 01.01.2020 00:00:00.",
      "Feltet (Innlagt) må være mindre enn eller lik 16.04.2024 10:59:26.",
      "Feltet (HoydeUkjent) er skjult hvis: Feltet (Hoyde) må besvares.",
      "Innleggelsesdato kan ikke være senere enn CreationDate."
    ),
    `Gyldig fra og med skjemaversjon` = c("0", "0", "0", "0", "0"),
    `Fjernet fra og med skjemaversjon` = c(rep(NA_character_, 5)),
    Sammenligningsverdi = c(NA, "43831", "45398.457943530098", NA, NA),
    Konsekvens = c("Error", "Error", "Error", "Info", "Error")
  ),
  kb_slutt_regler_raa = tibble::tibble(
    Skjemanavn = rep("Sluttskjema", 3),
    Id = c("1000", "1008", "1005"),
    Eiertype = c("Field", "Field", "Field"),
    Eier = c("Innlagt", "Innlagt", "Innlagt"),
    Regeltype = c("105", "107", "109"),
    `Regeltype navn` = c(
      "Påkrevd", "Er større enn eller lik gitt verdi",
      "Er mindre enn eller lik gitt verdi"
    ),
    Melding = c(
      "Feltet må besvares.",
      "Feltet må være større enn eller lik 01.01.2020 00:00:00",
      "Feltet må være mindre enn eller lik 16.04.2024 10:59:26"
    ),
    Forklaring = c(
      "Feltet (Innlagt) må besvares.",
      "Feltet (Innlagt) må være større enn eller lik 01.01.2020 00:00:00.",
      "Feltet (Innlagt) må være mindre enn eller lik 16.04.2024 10:59:26."
    ),
    `Gyldig fra og med skjemaversjon` = c("0", "0", "0"),
    `Fjernet fra og med skjemaversjon` = c(rep(NA_character_, 3)),
    Sammenligningsverdi = c(NA, "43831", "45398.457943530098"),
    Konsekvens = c("Error", "Error", "Error")
  )
)
} # kb_regler_raa

{
kb_metadata_raa = list(
  kb_metadata_test = list(
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
      metavariabel = c(
        "skjematypenavn", "skjematype_ID",
        "foreldreskjematype_ID", "antall_felter", "antall_regler",
        "skjemadato_hentes_fra_felt",
        "aldersberegning_skjer_i_forhold_til_felt",
        "er_ePROM_skjematype", "tilgjengelig_i_skjemasok",
        "tilgjengelig_i_skjemaopprettelsesdialog",
        "tilgjengelig_i_skjemaeksport", "tilgjengelig_i_rapporter",
        "vises_pa_pasientsiden", "vises_i_skjematellinger"
      ),
      metaverdi = c(
        "Testskjema", "1", NA_character_, "11", "5", "Innlagt", "CreationDate",
        "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"
      )
    )
  ),
  kb_metadata_slutt = list(
    versjonslogg = tibble::tibble(
      Skjemanavn = c("Sluttskjema", "Sluttskjema"),
      versjonsnummer = c("1", "2"),
      navn = c("Versjon 1", "Versjon 2"),
      dato = c("01.03.2020 12:00", "01.05.2021 12:00"),
      kan_opprettes = c("Nei", "Nei"),
      kan_endres = c("Ja", "Ja"),
      kan_slettes = c("Ja", "Ja")
    ),
    metainfo = tibble::tibble(
      metavariabel = c(
        "skjematypenavn", "skjematype_ID",
        "foreldreskjematype_ID", "antall_felter", "antall_regler",
        "skjemadato_hentes_fra_felt",
        "aldersberegning_skjer_i_forhold_til_felt",
        "er_ePROM_skjematype", "tilgjengelig_i_skjemasok",
        "tilgjengelig_i_skjemaopprettelsesdialog",
        "tilgjengelig_i_skjemaeksport", "tilgjengelig_i_rapporter",
        "vises_pa_pasientsiden", "vises_i_skjematellinger"
      ),
      metaverdi = c(
        "Sluttskjema", "2", "1", "11", "3", "Utskrevet", "CreationDate",
        "Nei", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"
      )
    )
  )
)
  
} # kb_metadata_raa - Versjonslogg og metainfo 
  
{
kb_test_metainfo = kb_metadata_raa[['kb_metadata_test']][['metainfo']] |> 
  tidyr::pivot_wider(names_from = "metavariabel", 
                     values_from = "metaverdi") |> 
  janitor::clean_names()

kb_slutt_metainfo = kb_metadata_raa[['kb_metadata_slutt']][['metainfo']] |> 
  tidyr::pivot_wider(names_from = "metavariabel", 
                     values_from = "metaverdi") |> 
  janitor::clean_names()
} # Metainfo 
{
d_forventet_ett_skjema = list(
  "versjonslogg" = list("testskjema" = kb_metadata_raa[['kb_metadata_test']][['versjonslogg']]),
  "metainfo" = list("testskjema" = kb_test_metainfo),
  "felter" = list("testskjema" = kb_felter_raa[['kb_test_felter_raa']]),
  "regler" = list("testskjema" = kb_regler_raa[['kb_test_regler_raa']])
) 

d_forventet_skjema_NULL = list(
  "versjonslogg" = list("testskjema" = kb_metadata_raa[['kb_metadata_test']][['versjonslogg']],
                        "sluttskjema" = kb_metadata_raa[['kb_metadata_slutt']][['versjonslogg']]),
  "metainfo" = list("testskjema" = kb_test_metainfo,
                    "sluttskjema" = kb_slutt_metainfo),
  "felter" = list("testskjema" = kb_felter_raa[['kb_test_felter_raa']],
                  "sluttskjema" = kb_felter_raa[['kb_slutt_felter_raa']]),
  "regler" = list("testskjema" = kb_regler_raa[['kb_test_regler_raa']],
                  "sluttskjema" = kb_regler_raa[['kb_slutt_regler_raa']])
)
} # Forventet output

# Feilmeldinger
feilmelding_feil_type_filsti = "Filsti må være en tekststreng"
feilmelding_feil_type_skjemanavn = "Skjemanavn må være NULL eller en tekst-vektor"
feilmelding_feil_skjemanavn_feil_navn = "Skjemanavn: «feil navn» finnes ikke i kodebok"

# Parse-funksjoner --------------------------------------------------------

# mrs5_parse_kodebok ------------------------------------------------------

test_that("gir forventet resultat", {
  
expect_identical(mrs5_parse_kodebok(filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
                                    skjemanavn = "Testskjema"),
                 d_forventet_ett_skjema)  

expect_identical(mrs5_parse_kodebok(filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
                                    skjemanavn = NULL), 
                 d_forventet_skjema_NULL)
    
})

# mrs5_parse_kodebok_skjema -----------------------------------------------


# Teste at funksjonen fungerer.
# 
# mrs5_parse_kodebok_meta -----------------------------------------------

# Utdata
test_that("Gir forventet resultat", {
  
  
  # Teste henting av info for ett enkelt skjema
  expect_identical(
    mrs5_parse_kodebok_meta(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "1-Testskjema"
    ),
    kb_meta_raa[['kb_test_meta_raa']]
  )
})
  
# mrs5_parse_kodebok_felter -----------------------------------------------

# Utdata
test_that("Gir forventet resultat", {
  
  expect_identical(
    mrs5_parse_kodebok_felter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "1-Testskjema"
    ),
    kb_felter_raa[['kb_test_felter_raa']]
  )
})

# mrs5_parse_kodebok_regler -----------------------------------------------

# Utdata
test_that("Gir forventet resultat", {

  expect_identical(
    mrs5_parse_kodebok_regler(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "1-Testskjema"
    ),
    kb_regler_raa[['kb_test_regler_raa']]
  )
  
})

# Hjelpefunksjoner for parse ----------------------------------------------

# mrs5_trekk_ut_skjemanavn ------------------------------------------------

# mrs5_kontroller_argumenter ----------------------------------------------

# filsti 
# Test for feil filtype
# Test for feil variabeltype 
# Test for feil filsti 

test_that("typekontroll filsti", {
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = 2L, 
      skjemanavn = NULL
    ),
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = 2.5, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = NULL, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = TRUE, 
      skjemanavn = NULL
    ), 
    feilmelding_feil_type_filsti)
})

# skjemanavn 
# Test for variabeltype 
test_that("typekontroll_skjemanavn", {
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = 1
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = TRUE
    ), 
    feilmelding_feil_type_skjemanavn
  )
  
  expect_true(
    mrs5_kontroller_argumenter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = NULL
    )
  ) 
  
  expect_true(
    mrs5_kontroller_argumenter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"), 
      skjemanavn = "Testskjema"
    )
  )
  
})

# skjemanavn innhold 
# Test med ett feil navn 
# Test med to feile navn 
# Test med korrekt navn og feil case 
# Test med flere korrekte navn 
# test med korrekt navn, men oppgitt to ganger - Vil at det kun skal leses inn en versjon 

test_that("Gir feilmelding hvis skjemanavn ikke eksisterer i kodebok", {
  
  expect_error(
    mrs5_kontroller_argumenter(
      filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
      skjemanavn = "feil navn"
    ),
    feilmelding_feil_skjemanavn_feil_navn
  )
})


# Konverter til kanonisk --------------------------------------------------

# mrs5_hent_metadata -----------------------------------------------------

# Forventet utdata

test_that("Hent_versjonslogg gir forventet utdata", {

  expect_identical(
    mrs5_hent_versjonslogg(
      parsed_generelt = mrs5_parse_kodebok_meta(
        filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
        skjemanavn = "1-Testskjema"
      )
    ),
    kb_metadata_raa[['kb_metadata_test']][['versjonslogg']]
  )
})

test_that("Hent_metainfo gir forventet utdata", {
  
  expect_identical(
    mrs5_hent_metainfo(
      parsed_generelt = mrs5_parse_kodebok_meta(
        filsti = test_path("testdata/mrs5-kodebok", "parse_kodebok_ok.xlsx"),
        skjemanavn = "1-Testskjema"
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

