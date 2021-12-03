

# Erstatt_ukjent

# Funksjonen erstatter utvalgte verdier med NA. Noen datasett
# (primært MRS-register) har variabler hvor manglende besvarelser får verdi -1,
# 99 eller andre mer eller mindre vilkårlige verdier for å indikere at spørsmålet
# ikke er besvart. Disse verdiene finnes i tillegg til *ekte* NA-verdier.
# Funksjonen tar inn et datasett som inneholder variabelen som skal endres.
# I tillegg tar den inn en na_vektor med alle verdiene som skal byttes til NA.

erstatt_ukjent = function(data, variabel, na_vektor) {
  if (!has_name(data, variabel)) {
    stop(paste0("'", variabel, "' mangler i inndata."))
  }

  d = data %>%
    mutate(across(all_of(variabel), ~ replace(., . %in% na_vektor, NA)))
  d
}
