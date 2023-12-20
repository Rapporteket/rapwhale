#' Regn ut kvalitetsindikator for gjennomsnitt
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Regner ut kvalitetsindikator med gjennomsnitt av kontinuerlige data,
#' basert på et standard datasett for dette.
#' Gir også ut konfidensintervall for gjenomsnittet.
#'
#' @param d_ki_ind
#' Datasett som gitt ut av en standard KI-funksjon
#' for numeriske/kontinuerlige data.
#' Se detaljer nedenfor.
#' @param konf_niva
#' Konfidensnivå.
#' Standardverdi er 0.95, som tilsvarer et 95 %-konfidensintervall.
#'
#' @details
#' Inndatasettet må inneholde minst de to variablene
#' `ki_x` og `ki_aktuell`, der `ki_x` er den numeriske variabelen
#' det skal beregnes gjennomsnitt for og `ki_aktuell` en
#' logisk variabel som indikerer om observasjonen skal inkluderes i
#' beregningen eller ikke.
#' Se vignetten for kvalitetsindikatorar
#' (\code{vignette("ki-funksjonar", package = "rapwhale")})
#' for mer detaljert informasjon, både om krav til gyldige inndata
#' og om bruk.
#'
#' Funksjonen gir ut aggregert datasett med estimert gjennomsnitt,
#' konfidensintervall for estimatet, og antall observasjoner hvor `ki_aktuell`
#' er sann.
#' Dersom ingen `ki_aktuell` er sann eller konfidensintervallet
#' ikke kan regnes ut (for eksempel fordi det bare finnes én observasjon
#' i en gitt gruppe), vil konfidensgrensene være `NA`.
#' Hvis inndataene er gruppert, blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' @note
#' Konfidensintervallene blir regnet ut under forutsetning
#' at dataene er normalfordelt.
#' (De bruker den vanlege formelen basert på *t*-fordelingen.)
#' Man bør altså være forsiktig med å stole mye på dem
#' dersom de kommer fra andre fordelinger,
#' spesielt usymmetriske fordelinger,
#' med mindre antall observasjoner er så stort
#' at sentralgrenseteoremet slår inn.
#'
#' @return
#' Ugruppert tibble eller `data.frame` (avhengig av inndataene) med følgende
#' kolonner:
#' \item{est}{Kvalitetsindikatoren, dvs. gjennomsnittet
#' av `ki_x` for radene som har sanne `ki_aktuell`-verdier
#' (eventuelt per gruppe).}
#' \item{konfint_nedre}{Nedre konfidensgrense for `est`.}
#' \item{konfint_ovre}{Øvre konfidensgrense for `est`.}
#' \item{n_aktuell}{Antall observasjoner som er inkludert i
#' beregningen av `est` (antall sanne `ki_aktuell`).}
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
#'   ki_x = c(6.23, 3.05, 12.2, 4.8, 1.2, NA_real_, 1.9, 7.4),
#'   ki_aktuell = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
#' )
#'
#' # Utregnet kvalitetsindikator for hele datasettet
#' aggreger_ki_snitt(d)
#'
#' # Eventuelt med 90 %-konfidensintervall
#' aggreger_ki_snitt(d, konf_niva = 0.9)
#'
#' # Gruppert på sykehusnivå
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_snitt()
#'
#' # Merk at sykehusene ovenfor blir vist i alfabetisk rekkefølge,
#' # siden grupperingsvariabelen «sykehus» var en tekstvariabel.
#' # Hvis du vil ha en annen rekkefølge, gjør den om til faktor først.
#' d = mutate(d, sykehus = factor(sykehus,
#'   levels = c("Haukeland", "Førde", "Voss")
#' ))
#' d |>
#'   group_by(sykehus) |>
#'   aggreger_ki_snitt()
aggreger_ki_snitt = function(d_ki_ind, konf_niva = 0.95, alfa = lifecycle::deprecated()) {
  # Åtvar viss nokon brukar det utdaterte «alfa»-argumentet
  if (lifecycle::is_present(alfa)) {
    lifecycle::deprecate_warn(
      when = "0.6.0",
      what = "aggreger_ki_snitt(alfa)",
      with = "aggreger_ki_snitt(konf_niva)",
      details = paste0(
        "`konf_niva` corresponds to 1 - `alfa`. ",
        "`alfa` will be completely dropped in the next version."
      )
    )
    konf_niva = 1 - alfa
  }

  if (!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_x", "ki_aktuell"))))) {
    stop("Inndata må være tibble/data.frame med kolonnene «ki_x» og «ki_aktuell»")
  }
  if (any(is.na(d_ki_ind$ki_aktuell)) || (!(is.logical(d_ki_ind$ki_aktuell)))) {
    stop("«ki_aktuell» må være TRUE eller FALSE")
  }
  if (!(is.numeric(d_ki_ind$ki_x))) {
    stop("«ki_x» må være numerisk")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_x))) {
    stop("«ki_x» må være en numerisk verdi hvis «ki_aktuell» er TRUE")
  }
  if (!is.numeric(konf_niva) || konf_niva <= 0 || konf_niva >= 1) {
    stop("«konf_niva» må være et tall mellom 0 og 1")
  }

  konfint = possibly(
    \(x) t.test(x, conf.level = konf_niva)$conf.int,
    otherwise = c(NA_real_, NA_real_)
  )

  d_ki_ind |>
    summarise(
      est = mean(ki_x[ki_aktuell]) |>
        replace_na(NA), # Gjør NaN om til NA
      konfint = list(konfint(ki_x[ki_aktuell])),
      konfint_nedre = map_dbl(konfint, pluck, 1),
      konfint_ovre = map_dbl(konfint, pluck, 2),
      n_aktuell = sum(ki_aktuell),
      .groups = "drop"
    ) |>
    select(-konfint)
}
