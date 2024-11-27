
# Hovedfunksjon ---------------------------------------------------------

#' mrs5_hent_kodebok
#' 
#' @description
#' Henter inn kodebok for registre på MRS5 fra `filsti`. 
#' Kaller på de nødvendige hjelpefunksjonene for å lese data, og 
#' lagre data på kanonisk format. 
#'
#' @param filsti Tekststreng som angir filsti til kodebok.  
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn slik det er 
#' navngitt i kodebok. Hvis `skjemanavn = NULL` hentes kodebok for alle skjema inn. 
#' @return
#' Listeobjekt som inneholder kb_kodebok, kb_kategoriske og kb_regler. 
#' @export
#'
#' @examples
mrs5_hent_kodebok = function(filsti, skjemanavn = NULL) {
  
  # Primærfunksjon som kaller på alle nødvendige hjelpefunksjoner for å 
  # lese inn data, konvertere til kanonisk og levere kanonisk kodebok. 
  
  # Kaller funksjoner for hvert skjema i ´skjemanavn´. 
  
  # mrs5_parse_kodebok
  # mrs5_konverter_til_kanonisk
  
  # Retunerer liste kb med tibblene 
  # kodebok, kategoriske og regler + metadata-attributter
} 

# Parse rådata -------------------------------------------------------------

mrs5_parse_kodebok = function(filsti) { 
  
  # Kaller på hjelpefunksjoner for å lese inn de enkelte delene av kodeboken.  
  
  # mrs5_parse_fanenavn()
  # mrs5_parse_kodebok_skjema()
  # mrs5_parse_kodebok_felter()
  # mrs5_parse_kodebok_regler()
  
  # Returnerer 
  }

mrs5_parse_fanenavn = function(filsti) {
  
  # Gir ut en tekstvektor med alle fanenavn slik de er lagret i kodebok 
  # fra filsti
}

mrs5_parse_kodebok_skjema = function(filsti, skjemanavn) {
  
  # Funksjon for å hente inn rådata fra fane '1 - Skjemanavn'. 
}

mrs5_parse_kodebok_felter = function(filsti, skjemanavn) {
  
  # Funksjon for å hente inn rådata fra fane '2 Skjemanavn-felter'. 
}

mrs5_parse_kodebok_regler = function(filsti, skjemanavn){
  
  # Funksjon for å hente inn rådata fra fane '3 Skjemanavn-regler'. 
}

# Konverter til kanonisk --------------------------------------------------

mrs5_konverter_til_kanonisk = function() {
}

# Hjelpefunksjoner kanonisk -----------------------------------------------

# metadata
mrs5_hent_metadata = function() {
  # henter metainfo og versjonslogg
}

mrs5_hent_versjonslogg = function() {
  # Trekke ut dato og versjonsnummer. 
  # Legge som attributt til kodebok
}

mrs5_hent_metainfo = function(){
  # Trekke ut aktuell info fra hovedliste. 
  # Antall felter, Antall regler, Skjemadato-kilde, aldersberegning-kilde, 
}

# kodebok
mrs5_lag_kanonisk_kb = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer 
  # kb[[kodebok]] på kanonisk format. 
}

###
mrs5_lag_skjema_id = function() {
  # Henter ut skjemanavn og genererer maskinlesbar skjema_id.
}

mrs5_kanonisk_variabeltype = function() {
  # Konverter variabeltype til internt navn. 
  # lage datasett med kolonne for alle kilder + kanonisk og legge i rapwhale?   
}

mrs5_definer_obligatorisk = function(){
  # Definer hvilke variabler som er obligatorisk å fylle ut. 
}

mrs5_identifiser_variabler_med_regler = function(){
  # Definer hvilke variabler som har regler tilknyttet. 
}

# kategoriske 
mrs5_lag_kanonisk_kategorisk = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer 
  # kb[[kategoriske]] på kanonisk format. 
}

###
mrs5_ekspander_kategoriske = function() {
  # Fylle ut kb_kategorisk med alle verdi/verditekst-kombinasjoner. 
}

mrs5_definer_manglende = function() {
  
  # Definere hvilke verdier/verditekster som skal regnes som manglande.
  
}

# regler
mrs5_lag_kanonisk_regler = function() {
  
  # Tar inn rådataversjon fra mrs5_parse_kodebok_regler og returnerer 
  # kb[[regler]] på kanonisk format. 
}

###
mrs5_definer_regeltype = function() {}

mrs5_definer_målvariabel = function() {}

mrs5_trekk_ut_regelverdi = function() {}

mrs5_trekk_ut_feilmelding = function() {}

mrs5_returner_manglende_regler = function() {
  
  # Gir ut en oversikt over regler som ikke kunne konverteres
}

# Validering --------------------------------------------------------------
mrs5_valider_kodebok = function(kodebok_kanonisk) {
  
  # Tar inn kodebok på kanonisk format og kaller på valideringsregler vha 
  # hjelpefunksjoner for å validere de ulike kodebok-seksjonene. 
  
  # Kaller på mrs5_valider_kodebok_kb, kategorisk og regler. 
}

###
mrs5_valider_kodebok_kb = function(kodebok_felter) {
  
  # Valideringsregler for kodebok_felter. 
  
  }

mrs5_valider_kodebok_kategorisk = function(kodebok_kategorisk) {
  
}

mrs5_valider_kodebok_regler = function(kodebok_regler) {
  
}
