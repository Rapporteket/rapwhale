library(tidyverse)

lag_kb_format = function() {
  # Funksjon tilsvarende lag_formatspek for å sikre trygg innlesning av kodebok
  # Definerer kolonnespesifikasjon for kb.

  # Her definerer vi de ulike formatene som brukes (OQR, MRS, etc.)

  # Returnerer kb_spek
}

les_kb_oqr_base = function(adresse) {
  # Tar inn:
  # Filplassering for kodebok og kb_spek som angir hvordan kodebok er
  # formatert (hvilke kolonner som behandles som tekst og tall etc.).
  # Sjekker at alle variabelnavn som skal være med er inkludert (via les_csv_oqr)

  kb_spek_oqr = tibble::tribble(
    ~varnavn_kilde, ~varnavn_resultat, ~vartype,
    "skjemanavn", "skjemanavn", "tekst",
    "navn_i_rapporteket", "navn_i_rapporteket", "tekst",
    "ledetekst", "ledetekst", "tekst",
    "obligatorisk", "obligatorisk", "tekst",
    "type", "type", "tekst",
    "listeverdier", "listeverdier", "tekst",
    "listetekst", "listetekst", "tekst",
    "normalintervall_start_numerisk", "normalintervall_start_numerisk", "tekst",
    "normalintervall_slutt_numerisk", "normalintervall_slutt_numerisk", "tekst",
    "maksintervall_start_numerisk", "maksintervall_start_numerisk", "tekst",
    "maksintervall_slutt_numerisk", "maksintervall_slutt_numerisk", "tekst",
    "normalintervall_start_dato", "normalintervall_start_dato", "tekst",
    "normalintervall_slutt_dato", "normalintervall_slutt_dato", "tekst",
    "maksintervall_start_dato", "maksintervall_start_dato", "tekst",
    "maksintervall_slutt_dato", "maksintervall_slutt_dato", "tekst",
    "antall_tegn", "antall_tegn", "heltall",
    "lovlige_tegn", "lovlige_tegn", "tekst",
    "desimaler", "desimaler", "heltall",
    "aktiveringsspoersmaal", "aktiveringsspoersmaal", "tekst",
    "underspoersmaal", "underspoersmaal", "tekst",
    "innfoert_dato", "innfoert_dato", "tekst",
    "utfaset_dato", "utfaset_dato", "tekst",
    "tabell", "tabell", "tekst",
    "fysisk_feltnavn", "fysisk_feltnavn", "tekst",
    "kommentar", "kommentar", "tekst",
    "variabel_id", "variabel_id", "tekst",
    "hjelpetekst", "hjelpetekst", "tekst"
  )

  # Leser inn kodebok med angitt spesifikasjon
  kb_oqr = les_csv_oqr(adresse, spesifikasjon = kb_spek_oqr)

  # Håndtering av problematiske variabler.
  # til_desimal = c("normalintervall_start_numerisk", "normalintervall_slutt_numerisk",
  #                "maksintervall_start_numerisk", "maksintervall_slutt_numerisk")
  # til_dato = c("normalintervall_start_dato", "normalintervall_slutt_dato",
  #             "maksintervall_start_dato", "maksintervall_slutt_dato",
  #             "innfoert_dato", "utfaset_dato")


  # Tester for ikke-glissen kodebok.
  # En del ting fra validering må skje her.


  # Teste for ulike verdier for samme variabler på flere skjema.
  # Se fixme l: 216 dformat

  # Returnerer: fullstendig kodebok som en tibble. Gir ut riktig variabeltype for kb.
  kb_oqr
}

kb_oqr_base_til_std = function(kodebok_validert, kb_kobling) {
  # Tar inn en kodebok-tibble med riktige variabeltyper.

  # Må gjøre en del validering av inndata
  # I funksjonen har vi oppgitt format for standard kodebok vi vil bruke
  # kb_kobling brukes for å koble diverse kodebokformat til standard format.

  # Kalle på funksjon for å fikse variabelnavn
  # skifte varnavn, endre rekkefølge på rader og kolonner.

  # Må ha definert hva et standard kodebokformat skal være.

  # Returnerer fullstendig kodebok på standard format
}

valider_kodebok = function(kodebok) {
  # Tar inn kodebok og gjennomfører komplett testing av innhold (min og max verdier)
  # Sjekker at variabeltyper er blant de aksepterte typene.
  # ++ tester fra kodebok-valider
  # Test for duplikate kategoriske verdier

  # Felles for OQR, MRS osv.
  # Returner kodebok_validert
}

legg_til_variabler_kb = function(kb, ekstra_spek) {
  # Funksjon(er) for å håndtere ekstra variabler som skal legges til KB og
  # eventuelt om variabler skal fjernes.
}

les_kb_oqr_v2 = function(adresse) {
  # Tar inn filplassering for kodebok

  # Har i funksjonen oppgitt kb_spek for det spesifikke register/struktur (her kb_oqr_spek)

  # Kaller på les_kodebok_base()
  # Kaller på valider_kodebok()
  # Kaller på legg_til_variabler_kb()
  # Kaller på konverter kodebok()

  # Returnerer fullstendig kodebok for registeret. (Må inkludere varnavn_kilde, varnavn_resultat og vartype slik det er oppgitt i spesifikasjon i les_dd funksjoner)
}
