# Generelle funksjoner for utregning av sumskårer

# Funksjon som sjekker at variabelnavnene i d er identiske til variabelnavnene i skaaringstabell

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

# Funksjon som returnerer en dataramme med radnummer, variabelnavn og feilverdi for ugyldige verdier

finn_ugyldige_verdier = function(d, koblingstabell) {
  radnr_ugyldige = vector()
  variabler_ugyldige = vector()
  verdier_ugyldige = vector()
  for (var_d in names(d)) {
    verdier_d = d[[var_d]]
    verdier_koblingstabell = koblingstabell %>%
      filter(variabel == !!var_d) %>%
      pull(verdi)
    verdier_d_ugyldige = setdiff(verdier_d, verdier_koblingstabell)
    if (length(verdier_d_ugyldige) > 0) {
      radnr_ugyldige = append(radnr_ugyldige, which(verdier_d %in% verdier_d_ugyldige))
      variabler_ugyldige = append(variabler_ugyldige, rep(var_d, times = length(verdier_d_ugyldige)))
      verdier_ugyldige = append(verdier_ugyldige, verdier_d_ugyldige)
    }
  }
  oversikt_ugyldige = arrange(
    tibble(
      radnr = as.integer(radnr_ugyldige),
      variabel = as.character(variabler_ugyldige),
      feilverdi = as.numeric(verdier_ugyldige)
    ),
    radnr
  )
  oversikt_ugyldige
}


################################# Midlertidig kode ################################

sjekk_variabelverdier = function(d, koblingstabell, godta_manglende) {
  if (!(is.data.frame(koblingstabell) && all(hasName(koblingstabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }

  # variabelnavn_akt = names(d)[names(d)%in% koblingstabell$variabel]
  alle_ugyldige = vector()
  for (var_d in names(d)) {
    verdier_d = d[[var_d]]
    verdier_koblingstabell = koblingstabell %>%
      filter(variabel == !!var_d) %>%
      pull(verdi)

    if (godta_manglende) {
      verdier_koblingstabell = append(verdier_koblingstabell, NA)
    }
    verdier_d_ugyldige = setdiff(verdier_d, verdier_koblingstabell)
    if (length(verdier_d_ugyldige) > 0) {
      alle_ugyldige = append(alle_ugyldige, paste0(var_d, ": ", paste0(verdier_d_ugyldige, collapse = ", ")))
    }
  }
  alle_ugyldige = unique(alle_ugyldige)
  if (length(alle_ugyldige) > 0) {
    stop("Ugyldige verdier: ", paste0(alle_ugyldige, collapse = ", "))
  }
}

endre_variabelnavn = function(d, variabelnavn) {

}

regn_sumskaar = function(d, skaaringstabell) {

}
