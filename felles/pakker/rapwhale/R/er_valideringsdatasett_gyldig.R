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
  } else {
    d_vld_intern_x = d_vld %>%
      select(starts_with("vld_verdi_intern_")) %>%
      names()

    d_vld_ekstern_x = d_vld_intern_x %>%
      str_replace("vld_verdi_intern_", "vld_verdi_ekstern_")

    d_vld_intern_class = d_vld[d_vld_intern_x] %>%
      map(class) %>%
      as.character()

    d_vld_ekstern_class = d_vld[d_vld_ekstern_x] %>%
      map(class) %>%
      as.character()

    if (any(d_vld_intern_class != d_vld_ekstern_class)) {
      gyldig = FALSE
    }
  }

  gyldig
}
