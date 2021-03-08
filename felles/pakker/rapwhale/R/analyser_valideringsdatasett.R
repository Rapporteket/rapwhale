#' Analyser valideringsdatasett ved å samanlikna verdiar
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Brukar ein samanlikningsfunksjon for å undersøka om interne og eksterne
#' verdiar i eit valideringsdatasett er «like» eller ikkje.
#'
#' @param d_vld Valideringsdatasett (dataramme/tibble). Sjå
#'   [er_valideringsdatasett_gyldig()] for definisjonen på eit (gyldig)
#'   valideringsdatasett. Det vert automatisk testa at datasettet er gyldig.
#' @param samanliknar Samanliknarfunksjon. Standard er [samanlikn_identisk()].
#'
#' @details
#' Funksjonen tek inn eit valideringsdatasett `d_vld` og ein samanliknarfunksjon
#' `samanliknar`, og returnerer datasettet med ein ekstra kolonne
#' `vld_verdiar_er_like` som for kvar rad seier om verdiane som skal
#' samanliknast, er «like»/ekvivalente (som definert av samanliknarfunksjonen).
#'
#' For kvar rad i `d_vld` der `vld_vartype` er for eksempel `"x"`,
#' brukar funksjonen `samanliknar`-funksjonen til å samanlikna verdiane
#' i kolonnane `vld_verdi_intern_x` og `vld_verdi_ekstern_x` (i `d_vld`).
#'
#' Argumentet `samanliknar` skal vera ein funksjon `f(verdi1, verdi2, varnamn)`
#' som tek inn tre like lange vektorar (eller siste argument kan vera `NULL`).
#' Dei to fyrste argumenta inneheld verdiane som skal samanliknast elementvis,
#' og den siste er ein tekstvektor som elementvis seier korleis samanlikninga
#' skal føregå. I praksis inneheld han namnet på variablane som verdiane i
#' valideringsdatasettet var henta frå.
#'
#' Argumentet `varnamn` i samanliknarfunksjonen kan vera `NULL` dersom han
#' støttar dette. Det er elles nødvendig å bruka ein samanliknarfunksjon
#' som forstår verdiane til `varnamn`.
#' Eit eksempel kan vera at ein har `varnamn`-element som `"vekt_gram"` og
#' `"temperatur"`, der samanliknarfunksjonen veit at at han skal ha større
#' slingringsmonn når han samanliknar to vektverdiar målt i gram enn to
#' temperaturmålingar målt i Celsius.
#'
#' @return Opphavleg datasett, men med ein ekstra kolonne
#'         `vld_verdiar_er_like` lagd til,
#'         som er `TRUE` for kvar rad der verdiane som skal samanliknast, er
#'         «like» i følgje `samanliknar`, og `FALSE` for kvar rad der dei
#'         ikkje er det.
#' @export
#'
#' @examples
#' # Eksempel på gyldig valideringsdatasett med både «like» og ikkje «like»
#' # verdiar i følgje standard samanliknar `samanlikn_identisk()`.
#' d_vld = tibble::tibble(
#'   pasient_id = c(5, 5, 5, 7),
#'   dato_operasjon = as.Date(c("2020-06-07", "2020-12-13", "2020-12-13", "2021-02-05")),
#'   kjonn = c("mann", "mann", "mann", "kvinne"),
#'   sjukehus = c("Bergen", "Bergen", "Førde", "Stavanger"),
#'   vld_varnamn = c("vekt", "vekt", "diabetes", "diabetes"),
#'   vld_vartype = c("tal", "tal", "logisk", "logisk"),
#'   vld_verdi_intern_tal = c(78, 88, NA, NA),
#'   vld_verdi_ekstern_tal = c(78, 90, NA, NA),
#'   vld_verdi_intern_logisk = c(NA, NA, TRUE, FALSE),
#'   vld_verdi_ekstern_logisk = c(NA, NA, TRUE, NA)
#' )
#' analyser_valideringsdatasett(d_vld)
analyser_valideringsdatasett = function(d_vld, samanliknar = samanlikn_identisk) {
  if (!er_valideringsdatasett_gyldig(d_vld)) {
    stop("Datasettet er ikkje på rett format")
  }


  # Gjev ut valideringsdatasettet med info i ekstrakolonne om verdiane
  # i kvar rad er «like»

  vartypar = unique(d_vld$vld_vartype)

  if (length(vartypar) == 0) {
    d_vld$ki_krit_teller = logical()
  }

  for (vartype in vartypar) {
    radnr_med_vartype = which(d_vld$vld_vartype == vartype)

    verdi_intern = glue::glue("vld_verdi_intern_{vartype}")
    verdi_ekstern = glue::glue("vld_verdi_ekstern_{vartype}")

    er_like = samanliknar(
      d_vld[[verdi_intern]][radnr_med_vartype],
      d_vld[[verdi_ekstern]][radnr_med_vartype],
      d_vld$vld_varnamn[radnr_med_vartype]
    )

    # Feilmelding viss samanliknar gjev ugyldige verdiar

    if (anyNA(er_like)) {
      stop("NA-verdiar frå samanliknaren")
    }

    if (class(er_like) != "logical") {
      stop("Ikkje logisk vektor frå samanliknaren")
    }

    antal_rader_vartype = nrow(d_vld[radnr_med_vartype, ])
    if (length(er_like) != antal_rader_vartype) {
      stop("Utdata frå samanliknaren har feil lengd")
    }

    d_vld[radnr_med_vartype, "ki_krit_teller"] = er_like
  }
  d_vld$ki_krit_nevner = TRUE
  d_vld
}
