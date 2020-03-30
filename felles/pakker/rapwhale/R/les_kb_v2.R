library(tidyverse)

lag_kb_format = function() {
  # Funksjon tilsvarende lag_formatspek for å sikre trygg innlesning av kodebok
  # Definerer kolonnespesifikasjon for kb.
  # Her definerer vi de ulike formatene som brukes (OQR, MRS, etc.)

  # Returnerer kb_spek
}

les_kb_base = function(adresse, kb_spek) {
  # Tar inn:
  # Filplassering for kodebok og kb_spek som angir hvordan kodebok er
  # formatert (hvilke kolonner som behandles som tekst og tall etc.).

  # Returnerer: fullstendig kodebok som en tibble, lest inn basert på oppgitt spesifikasjon.

  # Ser for meg at denne funksjonen vil kalles av les_kb_oqr og tilsvarende funksjoner.
}

konverter_kodebok = function(kodebok_validert, kb_kobling) {
  # Tar inn en validert(alternativ å ikke validere ?) kodebok
  # I funksjonen har vi oppgitt format for standard kodebok vi vil bruke
  # kb_kobling brukes for å koble diverse kodebokformat til standard format.

  # Kalle på funksjon for å fikse variabelnavn
  # janitor::clean_names()?

  # Returnerer fullstendig kodebok på standard format
}

valider_kodebok = function(kodebok) {
  # Tar inn kodebok og gjennomfører komplett testing av innhold (min og max verdier)
  # Sjekker at variabeltyper er blant de aksepterte typene.
  # ++ tester fra kodebok-valider

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
