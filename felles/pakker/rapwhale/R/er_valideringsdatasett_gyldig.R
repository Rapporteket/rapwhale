#' Valider valideringsdatasett
#'
#' @description Tek inn eit valideringsdatasett og returnerer TRUE eller FALSE
#'              alt etter om det er på rett format eller ikkje.
#'
#' @param d_vld valideringsdatasett som skal sjekkast.
#'
#' @details
#' Funksjonen sjekkar om `d_vld` er på rett format, og returnerer så TRUE
#' eller FALSE.
#'
#' For å vera eit gyldig valideringsdatasett må `d_vld` vera ein data.frame
#' eller tibble og ha tekstkolonnar `vld_varnamn` og `vld_vartype`.
#' `vld_varnamn` og `vld_vartype` kan ikkje ha `NA`-verdiar eller tomme
#' tekststrengar. `vld_vartype` må starta med ein bokstav, og berre innehalda
#' bokstavar og/eller siffer. Kombinasjonen av verdiar til `vld_varnamn` og
#' variablar som ikkje startar med `vld_` skal vera unik for kvar rad. For
#' kvar unike verdi `x` av `vld_vartype` så skal det finnast ein variabel
#' `vld_verdi_intern_x` og `vld_verdi_ekstern_x`. Kolonnar med namn som
#' `vld_tull` er ikkje lov (`vld_` er reservert prefiks).
#'
#' @return TRUE/FALSE, alt etter om inndata er på rett format eller ikkje.
#' @export
#'
#' @examples
#' d_vld = tibble(
#'   pasid = 101, vld_varnamn = "vekt", vld_vartype = "tal",
#'   vld_verdi_intern_tal = 76,
#'   vld_verdi_ekstern_tal = as.numeric(NA)
#' )
#'
#' er_valideringsdatasett_gyldig(d_vld)
er_valideringsdatasett_gyldig = function(d_vld) {
  gyldig = TRUE

  # Oversikt over alle kolonnenamn og kolonnenamn for «vld_»-kolonnane,
  # for seinare bruk
  kolnamn_alle = names(d_vld)
  kolnamn_vld = stringr::str_subset(kolnamn_alle, "^vld")

  # Inndata må vera data.frame/tibble og må ha tekstkolonnar vld_varnamn og vld_vartype
  if (!(is.data.frame(d_vld) &&
    all(has_name(d_vld, c("vld_varnamn", "vld_vartype")))
  )) {
    return(FALSE)
  }
  if (!(typeof(d_vld$vld_varnamn) == "character" &&
    typeof(d_vld$vld_vartype) == "character")) {
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
  vartypar_er_gyldige = stringr::str_detect(
    d_vld$vld_vartype,
    "^[[:alpha:]][[:alnum:]]*$"
  )
  if (!all(vartypar_er_gyldige)) {
    return(FALSE)
  }

  # Datasett med kolonnar med namn som «vld_tull» skal reknast som
  # ugyldige («vld_» er reservert prefiks)
  regexp_gyldige_kolnamn = paste0(
    "^vld_varnamn|vld_vartype|",
    "vld_verdi_(intern|ekstern)_[[:alpha:]][[:alnum:]]*$"
  )
  gyldige_kolnamn = stringr::str_detect(kolnamn_vld, regexp_gyldige_kolnamn)
  if (!all(gyldige_kolnamn)) {
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

  # For kvar unike verdi x av vld_vartype så skal det finnast ein
  # variabel vld_verdi_intern_x og vld_verdi_ekstern_x
  vld_vartype_x = d_vld$vld_vartype %>%
    unique()
  if (!all(map_chr(vld_vartype_x, ~ glue::glue("vld_verdi_intern_{.x}")) %in% names(d_vld)) ||
    !all(map_chr(vld_vartype_x, ~ glue::glue("vld_verdi_ekstern_{.x}")) %in% names(d_vld))) {
    return(FALSE)
  }

  # Viss vld_vartype = x, så må vld_verdi_intern_y og
  # vld_verdi_ekstern_y vera tomme viss x != y
  ikkje_vld_vartype_x = x %>%
    map(~ .x == d_vld$vld_vartype) %>%
    map(~ which(!.x))

  x_er_NA = x %>%
    map(~ is.na(c(
      d_vld[[glue::glue("vld_verdi_intern_{.x}")]][ikkje_vld_vartype_x[[match(.x, x)]]],
      d_vld[[glue::glue("vld_verdi_ekstern_{.x}")]][ikkje_vld_vartype_x[[match(.x, x)]]]
    )))

  if (!all(x_er_NA %>%
    map_lgl(all))) {
    return(FALSE)
  }

  # Kvar kombinasjon av verdiar til vld_varnamn eller variablar som ikkje
  # startar med vld_ skal vera unike
  if (any(duplicated(select(d_vld, -starts_with("vld_"), vld_varnamn)))) {
    return(FALSE)
  }

  gyldig
}
