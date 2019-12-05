# Generelle funksjoner for utregning av sumskårer


# Spesifikke funksjoner

endre_variabelnavn = function(d, variabelnavn) {

}

# Skal sjekke at variabelnavnene i d er identiske til variabelnavnene i skaaringstabell

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

sjekk_variabelverdier = function(d, koblingstabell, godta_manglende) {
  if (!(is.data.frame(koblingstabell) && all(hasName(koblingstabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }

  # variabelnavn_akt = names(d)[names(d)%in% koblingstabell$variabel]
  alle_ugyldige = vector()
  for (i in names(d)) {
    verdier_d = d[[i]]
    verdier_koblingstabell = koblingstabell %>%
      filter(variabel == !!i) %>%
      pull(verdi)

    if (godta_manglende) {
      verdier_koblingstabell = append(verdier_koblingstabell, NA)
    }
    verdier_d_ugyldig = setdiff(verdier_d, verdier_koblingstabell)
    if (length(verdier_d_ugyldig) > 0) {
      alle_ugyldige = append(alle_ugyldige, verdier_d_ugyldig)
    }
  }
  alle_ugyldige = unique(alle_ugyldige)
  if (length(alle_ugyldige) > 0) {
    stop("Ugyldige verdier: ", paste0(alle_ugyldige, collapse = ", "))
  }
}

regn_sumskaar = function(d, skaaringstabell) {

}
