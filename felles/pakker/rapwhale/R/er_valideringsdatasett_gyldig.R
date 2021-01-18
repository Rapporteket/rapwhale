er_valideringsdatasett_gyldig = function(d_vld) {
  gyldig = TRUE

  if (!(is.data.frame(d_vld) &&
    all(has_name(d_vld, c("vld_varnamn", "vld_vartype")))
  )) {
    gyldig = FALSE
  }

  gyldig
}
