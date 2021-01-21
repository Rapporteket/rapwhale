er_valideringsdatasett_gyldig = function(d_vld) {
  gyldig = TRUE

  # Inndata må vera data.frame/tibble og må ha tekstkolonner vld_varnamn og vld_vartype

  if (!(is.data.frame(d_vld) &&
    all(has_name(d_vld, c("vld_varnamn", "vld_vartype")))
  )) {
    return(FALSE)
  }
  if (class(d_vld$vld_varnamn) != "character" ||
    class(d_vld$vld_vartype) != "character") {
    return(FALSE)
  }

  # vld_varnamn og vld_vartype kan ikkje ha NA-verdiar eller tomme tekstrengar

  if (anyNA(d_vld$vld_varnamn) ||
    anyNA(d_vld$vld_vartype) ||
    any(d_vld$vld_varnamn == "") ||
    any(d_vld$vld_vartype == "")
  ) {
    return(FALSE)
  }

  # vld_vartype må starta med ein bokstav, og berre innehalda bokstavar og/eller siffer

  if (!all(grepl("^[a-zæøå][a-zæøå0-9]", d_vld$vld_vartype, ignore.case = TRUE))) {
    return(FALSE)
  }


  # Viss vld_verdi_intern_x finst, finst også vld_verdi_ekstern_x, og vice versa

  d_vld_int_ekst_x = d_vld %>%
    select(starts_with("vld_verdi_intern_"), starts_with("vld_verdi_ekstern_")) %>%
    names()

  x = d_vld_int_ekst_x %>%
    str_replace("vld_verdi_intern_", "") %>%
    str_replace("vld_verdi_ekstern_", "") %>%
    unique()

  if (!all(map_chr(x, ~ glue::glue("vld_verdi_intern_{.x}")) %in% names(d_vld)) ||
    !all(map_chr(x, ~ glue::glue("vld_verdi_ekstern_{.x}")) %in% names(d_vld))) {
    return(FALSE)
  }

  # vld_verdi_intern_x og vld_verdi_ekstern_x skal vera same type/klasse

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
    return(FALSE)
  }

  # Kolonner med namn som vld_tull skal ikkje vera lov

  d_vld_namn = d_vld %>%
    select(starts_with("vld_")) %>%
    names()

  d_vld_int_ekst_namn = d_vld %>%
    select(starts_with("vld_verdi_intern_"), starts_with("vld_verdi_ekstern_")) %>%
    names()

  if (!all(d_vld_namn %in% c(d_vld_int_ekst_namn, "vld_varnamn", "vld_vartype"))) {
    return(FALSE)
  }

  # For kvar unike verdi x av vld_vartype så skal det finnast ein
  # variabel vld_verdi_intern_x og vld_verdi_ekstern_x

  vld_vartype_x = d_vld$vld_vartype %>%
    unique()

  if (!all(map_chr(vld_vartype_x, ~ glue::glue("vld_verdi_intern_{.x}")) %in% names(d_vld)) ||
    !all(map_chr(vld_vartype_x, ~ glue::glue("vld_verdi_ekstern_{.x}")) %in% names(d_vld))) {
    return(FALSE)
  }

  gyldig
}
