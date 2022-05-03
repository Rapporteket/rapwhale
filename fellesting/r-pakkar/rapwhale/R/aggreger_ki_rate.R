#' Regn ut kvalitetsindikator for rater
#'
#' @description
#' `r lifecycle::badge("experimental)`
#'
#' Regner ut kvalitetsindikator for rate av numerisk data,
#' basert på et standard datasett for dette.
#' Gir også ut konfidensintervall for raten.
#'
#' @param d_ki_ind Datasett som gitt ut av en standard KI-funksjon for
#' ratedata.
#' Se detaljer nedenfor.
#' @param alfa Én minus nivået til konfidensintervallet.
#' Standardverdi er 0.05, som tilsvarer et 95 %-konfidensintervall.
#' @param multiplikator Heltallsverdi som mulitpliseres med raten for å angi
#' raten per valg av `multiplikator`.
#'
#' @details
#' Inndatasettet må inneholde minst de tre variablene `ki_antall`,
#' `ki_eksponering` og `ki_aktuell`,
#' der `ki_antall` er en numeriske variabel som angir hvor mange hendelser
#' som er blitt observert,
#' `ki_eksponering` er en numerisk variabel som angir eksponeringsforløpet
#' hvor hendelsene i `ki_antall` er blitt observert,
#' og `ki_aktuell` er en logisk variabel som indikerer om hendelsen skal
#' inkluderes i beregningen eller ikke.
#'
#' Funksjonen gir ut aggregert datasett med estimert rate og
#' konfidensintervall for estimatet.
#' Hvis inndataene er gruppert,
#' blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' Om man velger en ikke-standard verdi for `multiplikator`,
#' for eksempel `1000`,
#' får man ut estimat av rate og konfidensintervall per `1000`.
#' Dette kan være hendig om man ser på hendelser per innbygger i en stor
#' populasjon.
#'
#' @note
#' Funksjonen regner ut estimert rate ved å,
#' på gruppenivå,
#' finne det totale antall hendelser,
#' og dividere med det totale eksponeringsforløpet
#' (sum(`ki_antall`)/sum(`ki_eksponering`)*`multiplikator`).
#'
#' Om man heller er interessert i andelsdata,
#' bruk [aggreger_ki_prop()].
#' Ratedata er en måling av frekvensen en hendelse forekommer i et definert
#' eksponeringsforløp,
#' mens andelsdata er en sammenligning av en del av dataene med hele datasettet.
#'
#' Konfidensintervallene blir regnet ut under forutsetning at dataene er
#' Poissonfordelt,
#' og blir da beregnet ved profil-likelihood metoden.
#'
#' @return
#' Ugruppert `tibble` eller `data.frame`
#' (avhengig av inndataene)
#' med følgende kolonner:
#' \item{est} Estimert Kvalitetsindikatoren, dvs. estimert rate
#' (mean(`ki_antall`)/sum(`ki_eksponering`)).
#' \item{konfint_nedre} Nedre konfidensgrense for `est`.
#' \item{konfint_ovre} Øvre konfidensgrense for `est`.
#'
#' @export
#'
#' @example
#' #Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' # Eksempeldata
#' helseforetak = c(
#'   "Helse Vest",
#'   "Helse Sor Ost",
#'   "Helse Midt",
#'   "Helse Nord"
#' )
#' populasjon = c(1100000, 3100000, 700000, 480000),
#' antall_behandlinger = c(650000, 1670000, 450000, 350000)
#' d = tibble(
#'   helseforetak = helseforetak,
#'   ki_antall = antall_behandlinger,
#'   ki_eksponering = populasjon,
#'   ki_aktuell = c(TRUE, TRUE, FALSE, TRUE)
#' )
#'
#' # Utregnet kvalitetsindikator for hele datasettet
#' aggreger_ki_rate(d)
#'
#' # Eventuelt med 90 %-konfidensintervall
#' aggreger_ki_rate(d, alfa = 0.1)
#'
#' # Gruppert på helseforetak, per 1000
#' d %>%
#'   group_by(helseforetak) %>%
#'   aggreger_ki_rate(multiplikator = 1000)
#'
#' # Merk at helseforetakene ovenfor blir vist i alfabetisk rekkefølge,
#' # siden grupperingsvariabelen «helseforetak» var en tekstvariabel.
#' # Hvis du vil ha en annen rekkefølge, gjør den om til faktor først.
#' d = mutate(d, helsefortak = factor(helseforetak,
#'   levels = c("Helse Nord", "Helse Midt", "Helse Vest", "Helse Sor Ost")
#' ))
#' d %>%
#'   gruop_by(helseforetak) %>%
#'   aggreger_ki_rate()
