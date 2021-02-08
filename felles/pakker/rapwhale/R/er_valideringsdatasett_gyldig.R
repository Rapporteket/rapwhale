#' Valider valideringsdatasett
#'
#' @description Tek inn eit valideringsdatasett og returnerer `TRUE` eller
#'              `FALSE` dersom det er høvesvis gyldig eller ugyldig.
#'
#' @param d_vld Valideringsdatasett (dataramme/tibble).
#'
#' @details
#' Eit gyldig valideringsdatasett består av éin eller fleire
#' indeksvariablar, variablane `vld_varnamn` og `vld_vartype` og
#' to eller fleire verdivariablar.
#'
#' Eksempel på indeksvariablar er pasient- og/eller forløps-ID pluss ekstra
#' metadata som kjønn, sjukehus og operasjonstype. Saman med `vld_varnamn`
#' skal indeksvariablane unikt identifisera ei rad (observasjonen som skal
#' validerast).
#'
#' Variabelen `vld_varnamn` inneheld namnet på variabelen som rada
#' inneheld valideringsdata for, mens `vld_vartype` seier kva
#' type variabel det er, for eksempel tal, tekst eller dato.
#' Ein kan fritt velja kva verdiar ein brukar for `vld_vartype`
#' (for eksempel `"tal"`, `"tall"`, `"heiltal"` eller `"int32"`),
#' men verdien må starta med ein bokstav og berre innehalda
#' bokstavar og/eller siffer.
#'
#' For kvar unike `vld_vartype` skal det finnast verdivariablar –
#' variablar med prefiksa `vld_verdi_intern_` og `vld_verdi_ekstern_`.
#' Viss for eksempel ein `vld_vartype` er `"tal"`, skal det finnast
#' verdivariablar `vld_verdi_intern_tal` og `vld_verdi_ekstern_tal`.
#' Den første seier kva verdi variabelen i `vld_varnamn` har i den
#' *interne* kjelda, som typisk er eit kvalitetsregister, og den
#' andre seier kva verdi han har i den *eksterne* kjelda, som typisk
#' er ein pasientjournal eller eit anna fagsystem. Dei to variablane
#' skal naturlegvis ha tilsvarande variabeltype som `vld_vartype`,
#' for `"tal"` for eksempel' `double` eller `integer`.
#'
#' Det skal ikkje finnast andre kolonnar som startar med `vld_` enn
#' dei som er nemnde ovanfor (`vld_` er eit reservert prefiks).
#'
#' @return `TRUE` dersom datasettet er gyldig og `FALSE` dersom det
#'         ikkje er det.
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
  # Oversikt over alle kolonnenamn og kolonnenamn for «vld_»-kolonnane,
  # for seinare bruk
  kolnamn_alle = names(d_vld)
  kolnamn_vld = stringr::str_subset(kolnamn_alle, "^vld_")
  kolnamn_verdikol = stringr::str_subset(kolnamn_vld, "^vld_verdi_")
  kolnamn_indekskol = setdiff(kolnamn_alle, c(kolnamn_verdikol, "vld_vartype"))

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
  vartypar_i_verdikol = kolnamn_verdikol %>%
    stringr::str_replace("^vld_verdi_(intern|ekstern)_", "") %>%
    unique()
  lag_kolnamn_verdikol = function(vartypar) {
    if (length(vartypar) > 0) {
      c(
        paste0("vld_verdi_intern_", vartypar),
        paste0("vld_verdi_ekstern_", vartypar)
      )
    } else {
      character()
    }
  }
  kolnamn_som_skal_finnast = lag_kolnamn_verdikol(vartypar_i_verdikol)
  verdikol_finst = kolnamn_som_skal_finnast %in% kolnamn_verdikol
  if (!all(verdikol_finst)) {
    return(FALSE)
  }

  # vld_verdi_intern_x og vld_verdi_ekstern_x skal vera same type/klasse
  for (vartype in vartypar_i_verdikol) {
    kolnamn_intern = paste0("vld_verdi_intern_", vartype)
    kolnamn_ekstern = paste0("vld_verdi_ekstern_", vartype)
    klasse_intern = class(d_vld[[kolnamn_intern]])
    klasse_ekstern = class(d_vld[[kolnamn_ekstern]])
    if (!identical(klasse_intern, klasse_ekstern)) {
      return(FALSE)
    }
  }

  # For kvar unike verdi x av vld_vartype så skal det finnast ein
  # variabel vld_verdi_intern_x og vld_verdi_ekstern_x
  # (Obs: Dessverre litt kodeduplisering for tidlegare test på kolonnenamn,
  #       men kan ikkje enkelt bruka generell funksjon, sidan
  #       me skal returnera FALSE ved feil, men ikkje returnera elles.)
  vartypar_som_skal_finnast = unique(d_vld$vld_vartype)
  kolnamn_som_skal_finnast = lag_kolnamn_verdikol(vartypar_som_skal_finnast)
  verdikol_finst = kolnamn_som_skal_finnast %in% kolnamn_verdikol
  if (!all(verdikol_finst)) {
    return(FALSE)
  }

  # Viss vld_vartype = x, så må vld_verdi_intern_y og
  # vld_verdi_ekstern_y vera tomme viss x != y
  for (kolnamn in kolnamn_verdikol) {
    vartype_akt_kol = str_replace(kolnamn, "^vld_verdi_(intern|ekstern)_", "")
    radnr_ikkje_akt_vartype = which(d_vld$vld_vartype != vartype_akt_kol)
    verdiar = d_vld[[kolnamn]][radnr_ikkje_akt_vartype]
    if (!all(is.na(verdiar))) {
      return(FALSE)
    }
  }

  # Kvar kombinasjon av verdiar til indekskolonnane (dvs. til vld_varnamn
  # eller variablar som ikkje startar med «vld_») skal vera unike
  if (any(duplicated(d_vld[kolnamn_indekskol]))) {
    return(FALSE)
  }

  # Viss ingen tidlegare testar har slått ut (dvs. returnert FALSE),
  # er datasettet gyldig
  TRUE
}
