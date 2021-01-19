er_valideringsdatasett_gyldig = function(d_vld) {
  gyldig = TRUE

  if (!(is.data.frame(d_vld) &&
    all(has_name(d_vld, c("vld_varnamn", "vld_vartype")))
  )) {
    gyldig = FALSE
  } else if (class(d_vld$vld_varnamn) != "character" ||
    class(d_vld$vld_vartype) != "character") {
    gyldig = FALSE
  } else if (anyNA(d_vld$vld_varnamn) ||
    anyNA(d_vld$vld_vartype) ||
    any(d_vld$vld_varnamn == "") ||
    any(d_vld$vld_vartype == "")
  ) {
    gyldig = FALSE
  }

  gyldig
}
