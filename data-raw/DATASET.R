# Generere koblingsnøkkel for å konvertere variabeltyper fra de ulike formatene
# som finnes i MRS5, OQR og eventuelt andre kilder til vårt interne kanoniske
# format.

# FIXME - Finne variabeltyper fra OQR
# FIXME - Finne manglende typer fra MRS5

koblingsliste_vartyper =
  tibble::tribble(
    ~Kanonisk, ~MRS5, ~OQR, ~readr,  ~Beskrivelse,
    "tekst", "String", "tekst", "c", "En tekststreng",
    "kategorisk", "Enum", NA_character_, "c", "En kategorisk variabel som kan ha én verdi fra et sett verdier. Hver mulige verdi har en tilhørende verditekst.",
    "numerisk", "Number", NA_character_, "n", "En tallverdi. For numeriske variabler kan antall desimaler defineres.",
    "boolsk", "Bool", NA_character_, "l", "TRUE/FALSE variabel. Ofte vist som en avkryssningsboks.",
    "dato", NA_character_, NA_character_, "D", "Datoverdi, Inkluderer dag, måned og år. Dato konverteres til formatet YYYY-MM-DD.",
    "dato_kl", "DateTime", NA_character_, "T", "Tidspunktverdi. Inkluderer dag, måned, år, og klokkeslett. Konverteres til formatet YYYY-MM-DD HH:MM:SS.",
    "kl", NA_character_, NA_character_, "t", "Klokkeslettverdi, Oppgir tidspunkt av døgnet."
  )


usethis::use_data(koblingsliste_vartyper,
  internal = FALSE,
  overwrite = TRUE
)
