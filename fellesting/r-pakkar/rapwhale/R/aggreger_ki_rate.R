#' Regn ut kvalitetsindikator for rater
#'
#' @description
#' `r lifecycle::badge("experimental)`
#'
#' Regner ut kvalitetsindikator for rater (antall delt på eksponering),
#' basert på et standard datasett for ratedata/Poisson-data.
#' Gir også ut konfidensintervall for ratene.
#'
#' @param d_ki_ind Datasett som gitt ut av en standard KI-funksjon for
#' ratedata.
#' Se detaljer nedenfor.
#' @param alfa Én minus nivået til konfidensintervallet.
#' Standardverdi er 0.05, som tilsvarer et 95 %-konfidensintervall.
#' @param multiplikator Tallverdi som skal multipliseres med raten
#' (for å vise raten per `multiplikator` enheter i utdatene).
#'
#' @details
#' Inndatasettet må inneholde minst de tre variablene `ki_antall`,
#' `ki_eksponering` og `ki_aktuell`,
#' der `ki_antall` er en numeriske variabel som angir hvor mange hendelser
#' som er blitt observert
#' (for eksempel antall ganger pasienten har fått smertelindring
#' eller antall observerte bakteriekolonier),
#' `ki_eksponering` er en numerisk variabel som angir eksponeringen
#' som var grunnlag for hendelsene i `ki_antall`
#' (her for eksempel antall pasientdøgn
#' eller størrelsen på undersøkt område, målt i kvadratcentimeter),
#' og `ki_aktuell` er en logisk variabel
#' som indikerer om observasjonen/raden
#' skal inkluderes i beregningen eller ikke.
#'
#' Funksjonen gir ut aggregert datasett med estimert rate og
#' konfidensintervall for estimatet.
#' Hvis inndataene er gruppert,
#' blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' Om man velger en ikke-standard verdi for `multiplikator`,
#' får man ut estimat av rate og tilhørende konfidensintervall
#' per `multiplikator` av enheten til eksponeringen.
#' Dette kan være hendig om man for eksempel ser på sjeldne hendelser
#' per innbygger i en stor populasjon.
#' Da kan man sette `multiplikator` til `1000` eller `100000`
#' for å få ut raten per 1000 eller 100 000 innbyggere.
#' 
#' @note 
#' Konfidensintervallene blir regnet ut under forutsetning at dataene er
#' Poisson-fordelt,
#' og blir da beregnet ved profil-likelihood-metoden.
#'
#' @seeother
#' Om du heller er interessert i andelsdata (teller/nevner),
#' altså data som følger en binomisk fordeling,
#' bruk [aggreger_ki_prop()].
#'
#' @return
#' Ugruppert `tibble` eller `data.frame`
#' (avhengig av inndataene)
#' med følgende kolonner:
#' \item{est} Kvalitetsindikatoren, dvs. estimert rate
#' (sum(`ki_antall`)/sum(`ki_eksponering`)*`multiplikator`).
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
#'   "Helse Sør-Øst",
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
#'   levels = c("Helse Nord", "Helse Midt", "Helse Vest", "Helse Sør-Øst")
#' ))
#' d %>%
#'   group_by(helseforetak) %>%
#'   aggreger_ki_rate()
