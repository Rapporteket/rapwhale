#' Les inn csv-fil med OQR formatspesifikasjon
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{les_csv_oqr()} er en spesifikk versjon av \code{les_csv_base()} som er
#' tilpasset OpenQReg registerstruktur.
#' Funksjonen tar inn argumentene adresse og spesifikasjon. Adresse angir
#' filplassering, mens spesifikasjon angir opprinnelige og nye variabelnavn,
#' samt variabeltype.
#' Se rapwhale::spesifikasjon for eksempeloppsett for ulike innstillinger.
#' Variabelnavn kan endres ved å inkludere en vektor med nye variabelnavn.
#' Aksepterte variabeltyper er: tekst, desimaltall, heltall, boolsk, dato,
#' dato_kl og kl.
#'
#' @param adresse en tekststreng som angir filplassering for csv-fil som
#' skal leses inn
#' @param spesifikasjon Tibble med tre kolonner. Inneholder varnavn_kilde,
#' varnavn_resultat og vartype.
#'
#' @export
les_csv_oqr = function(adresse, spesifikasjon) {
  # Kilde for disse valgene finnes foreløpig her. Vi venter på ny dokumentasjon fra HNIKT.
  # 1. - ***FJERNA-ADRESSE***
  # 2. - ***FJERNA-ADRESSE***

  # !fixme - Oppdatere kilde når ny dokumentasjon kommer fra HNIKT.
  # !fixme - Lage funksjon for å generere formatspek
  formatspek_oqr = lag_formatspek(
    skilletegn = ";", # (1)
    desimaltegn = ",", # (1) Skal være "." i følge dokumentasjon
    dato = "%Y-%m-%d", # (1) YYYY-MM-DD
    klokkeslett = "%H:%M", # (2) under tidsvariabel
    dato_kl = "%Y-%m-%d %H:%M:%OS", # (1) YYYY-MM-DD HH:MM:SS i følge dokumentasjon.
    tidssone = "Europe/Oslo", # (1) Se Timestamp
    tegnkoding = "UTF-8", # (1)
    boolsk_sann = 1, # (1)
    boolsk_usann = 0, # (1)
    na_verdier = ""
  ) # (1)

  les_csv_base(
    adresse = adresse,
    spesifikasjon = spesifikasjon,
    formatspek = formatspek_oqr
  )
}
