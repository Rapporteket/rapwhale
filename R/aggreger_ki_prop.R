#' Regn ut kvalitetsindikator for andeler
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Regner ut kvalitetsindikator for andeler (teller delt på nevner)
#' basert på et standard datasett for binomiske data / andelsdata.
#' Gir også ut konfidensintervall for andelene.
#'
#' @param d_ki_ind
#' Datasett som gitt ut av en standard KI-funksjon
#' for binomiske data / andelsdata / proporsjonsdata.
#' Se detaljer nedenfor.
#' @param konf_niva
#' Konfidensnivå.
#' Standardverdi er 0.95, som tilsvarer et 95 %-konfidensintervall.
#' @param alfa
#' `r lifecycle::badge("deprecated")`
#' Utdatert og erstatta av `konf_niva` (tilsvarande `1 - alfa`).
#' Vert fjerna i neste versjon av pakken.
#'
#' @details
#' Inndatasettet må inneholde minst de to logiske variablene
#' `ki_krit_nevner` og `ki_krit_teller`, der `ki_krit_nevner`
#' sier om raden (for eksempel pasienten eller forløpet) oppfyller
#' kriteriet for å inngå i *nevneren* i indikatoren,
#' og `ki_teller` sier om den oppfyller kriteriet for å inngå i
#' *telleren* (dvs. skal regnes som en «suksess»). Se vignetten
#' for kvalitetsindikatorar
#' (\code{vignette("ki-funksjonar", package = "rapwhale")})
#' for mer detaljert informasjon, både om krav til gyldige inndata
#' og om bruk.
#'
#' Funksjonen gir ut aggregert datasett med teller, nevner, estimatet
#' teller/nevner og konfidensintervall for estimatet, alt bare for
#' de radene der `ki_krit_nevner` er sann. Dersom ingen `ki_krit_nevner`
#' er sann, vil estimatet og tilhørende konfidensgrenser være `NA`.
#' Hvis inndataene er gruppert, blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' @note
#' Funksjonen kan også brukes for teller/nevner-data som ikke oppfyller
#' antagelsen om binomiske data, men da kan man naturlegvis ikke stole på
#' konfidensintervallene som en får ut.
#'
#' @return
#' Ugruppert tibble eller `data.frame` (avhengig av inndataene) med følgende
#' kolonner:
#' \item{est}{Kvalitetsindikatoren, dvs. estimert andel (`ki_teller`/`ki_nevner`).}
#' \item{ki_teller}{Telleren i indikatoren (antall sanne `ki_krit_teller`
#'                  der `ki_krit_nevner` er sann).}
#' \item{ki_nevner}{Nevneren i indikatoren (antall sanne `ki_krit_nevner`).}
#' \item{konf_int_nedre}{Nedre konfidensgrense for `est`.}
#' \item{konf_int_ovre}{Øvre konfidensgrense for `est`.}
#' I tillegg vil det være kolonner for alle grupperingsvariablene.
#' @export
#' @examples
#' # Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' # Eksempeldata
#' d = tibble(
#'   pasid = 1:8,
#'   sykehus = rep(c("Haukeland", "Førde", "Voss"), times = c(3, 2, 3)),
#'   ki_krit_teller = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
#'   ki_krit_nevner = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
#' )
#'
#' # Utregnet kvalitetsindikator for hele datasettet
#' aggreger_ki_prop(d)
#'
#' # Eventuelt med 90 %-konfidensintervall
#' aggreger_ki_prop(d, konf_niva = 0.9)
#'
#' # Gruppert på sykehusnivå
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_prop()
#'
#' # Merk at sykehusene ovenfor blir vist i alfabetisk rekkefølge,
#' # siden grupperingsvariabelen «sykehus» var en tekstvariabel.
#' # Hvis du vil ha en annen rekkefølge, gjør den om til faktor først.
#' d = mutate(d, sykehus = factor(sykehus,
#'   levels = c("Haukeland", "Førde", "Voss")
#' ))
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_prop()
aggreger_ki_prop = function(d_ki_ind, konf_niva = 0.95, alfa = lifecycle::deprecated()) {
  # Åtvar viss nokon brukar det utdaterte «alfa»-argumentet
  if (lifecycle::is_present(alfa)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "aggreger_ki_prop(alfa)",
      with = "aggreger_ki_prop(konf_niva)",
      details = paste0(
        "`konf_niva` corresponds to 1 - `alfa`. ",
        "`alfa` will be completely dropped in the next version."
      )
    )
    konf_niva = 1 - alfa
  }

  # Teste inndata
  checkmate::assert_data_frame(d_ki_ind)
  checkmate::assert_names(names(d_ki_ind),
    must.include = c("ki_krit_teller", "ki_krit_nevner")
  )
  checkmate::assert_logical(d_ki_ind$ki_krit_teller)
  checkmate::assert_logical(d_ki_ind$ki_krit_nevner, any.missing = FALSE)
  if (!all(
    (d_ki_ind$ki_krit_teller %in% c(TRUE, FALSE, NA)) &
      ((d_ki_ind$ki_krit_teller %in% c(TRUE, FALSE) & d_ki_ind$ki_krit_nevner) |
        (d_ki_ind$ki_krit_teller %in% c(FALSE, NA) & !d_ki_ind$ki_krit_nevner))
  )) {
    stop(
      "«ki_krit_teller» må være TRUE eller FALSE hvis «ki_krit_nevner» ",
      "er TRUE, og FALSE eller NA hvis «ki_krit_nevner» er FALSE"
    )
  }
  checkmate::assert_integer(group_size(d_ki_ind), lower = 1)
  checkmate::assert_numeric(konf_niva, lower = 0, upper = 1)

  # Beregne utdata
  d_sammendrag = d_ki_ind |>
    summarise(
      ki_teller = sum(ki_krit_teller, na.rm = TRUE),
      ki_nevner = sum(ki_krit_nevner),
      est = ki_teller / ki_nevner,
      .groups = "keep"
    ) |>
    select(group_cols(d_ki_ind), est, ki_teller, ki_nevner) |>
    ungroup()

  # Legg til konfidensintervall
  konfint_robust = function() {
    konf = possibly(
      .f = \() {
        regn_konfint_bin(
          x = d_sammendrag$ki_teller,
          n = d_sammendrag$ki_nevner,
          konf_niva = konf_niva
        )
      },
      otherwise = data.frame(
        method = NA_character_,
        x = NA_integer_,
        n = NA_integer_,
        mean = NA_real_,
        lower = NA_real_,
        upper = NA_real_
      )
    )
    konf()
  }

  konfint = konfint_robust()
  d_sammendrag$konfint_nedre = konfint$lower
  d_sammendrag$konfint_ovre = konfint$upper

  # Sørg for at manglende estimat alltid blir returnert som NA
  # (og ikke for eksempel NaN, som vi får ved 0/0)
  d_sammendrag = d_sammendrag |>
    mutate(across(c(est, konfint_nedre, konfint_ovre),
      .fns = \(x) replace_na(x, replace = NA)
    ))

  d_sammendrag
}
