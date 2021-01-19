#' @importFrom purrr possibly map_dbl pluck
#' @importFrom tidyr replace_na
#' @importFrom dplyr summarise mutate select groups
NULL
#' Regn ut kvalitetsindikator for gjennomsnitt
#'
#' Regner ut kvalitetsindikator for gjennomsnitt (sum av x delt på n)
#' hvor x er en numerisk indikatorvariabel og n er antall observasjoner. 
#' Gir også ut konfidensintervall for gjenomsnittet dersom det er mulig. 
#' 
#' @param d_ki_ind Datasett som gitt ut av en standard KI-funksjon
#'   for numeriske data. Se detaljer nedenfor.
#' @param alfa Én minus nivået til konfidensintervallet.
#'   Standardverdi er 0.05, som tilsvarer et 95 %-konfidensintervall.
#'
#' @details 
#' Inndatasettet må inneholde minst de to variablene
#' `ki_x` og `ki_aktuell`, der `ki_x` er den numeriske variabelen 
#' det skal beregnes gjennomsnitt for og `ki_aktuell` er en 
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
#' Dersom ingen `ki_aktuell` er sann eller hvis det kun er en observasjon 
#' i en gruppe, vil konfidensgrenser være `NA`.
#' Hvis inndataene er gruppert, blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#' 
#' @return 
#' Tibble eller `data.frame` (avhengig av inndataene) med følgende
#' kolonner:
#' \item{est}{Kvalitetsindikatoren, dvs. estimert gjennomsnitt 
#' (sum(`ki_x`)/sum(`ki_aktuell`)).}
#' \item{konfint_nedre}{Nedre konfidensgrense for `est`.}
#' \item{konfint_ovre}{Øvre konfidensgrense for `est`.}
#' \item{n_aktuell}{Antall observasjoner som er inkludert i 
#' beregningen av `est`(antall sanne `ki_aktuell`).}
#' I tillegg vil det være kolonner for alle grupperingsvariablene.
#' @export
#' @examples 
#' # Pakke for bruk av rør-tegnet
#' library(dplyr)
#' 
#' # Eksempeldata
#' d = tibble(pasid = 1:8,
#'            sykehus = rep(c("Haukeland", "Førde", "Voss"), times = c(3,2,3)),
#'            ki_x = c(6.23, 3.05, 12.2, 4.8, 1.2, NA_real_, 1.9, 7.4),
#'            ki_aktuell = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
#' 
#' # Viser resultat for alle rader i inndata 
#' aggreger_ki_snitt(d)
#' 
#' # Gruppert til sykehusnivå og konfidensintervall er justert til 90 % ved å 
#' endre på alfa. 
#' d %>% 
#' group_by(sykehus) %>% 
#' aggreger_ki_snitt(alfa = 0.1)
#' 
#' #' # Merk at sykehusene ovenfor blir vist i alfabetisk rekkefølge,
#' # siden grupperingsvariabelen «sykehus» var en tekstvariabel.
#' # Hvis du vil ha en annen rekkefølge, gjør de om til faktor først.
#' d = mutate(d, sykehus = factor(sykehus,
#'                                levels = c("Haukeland", "Førde", "Voss")))
#' d %>% 
#'   group_by(sykehus) %>% 
#'   aggreger_ki_prop(alfa = 0.1)
aggreger_ki_snitt <- function(d_ki_ind, alfa = 0.05){

  # Teste inndata
  if(!(is.data.frame(d_ki_ind) && all(hasName(d_ki_ind, c("ki_x", "ki_aktuell"))))){
    stop("Inndata må være tibble/data.frame med kolonnene 'ki_x' og 'ki_aktuell'")
  }
  if(any(is.na(d_ki_ind$ki_aktuell)) || (!(is.logical(d_ki_ind$ki_aktuell)))) {
    stop("'ki_aktuell' må være TRUE eller FALSE")
  }
  if(!(is.numeric(d_ki_ind$ki_x))) {
    stop("'ki_x' må være numerisk")
  }
  if(any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_x))){
    stop("'ki_x' må være en numerisk verdi hvis 'ki_aktuell' er TRUE")
  }

    # Husk å legge inn tester for dette tilfellet.
  konfint = function(x) {
    konfint_robust = possibly(
      ~t.test(.x)$conf.int, otherwise = c(NA_real_, NA_real_))
    konfint_robust(x)
  }

  d_ki_ind %>%
    summarise(est = mean(ki_x[ki_aktuell], na.rm = TRUE) %>%
                replace_na(NA), # Gjør NaN om til NA
              konfint = list(konfint(ki_x[ki_aktuell])),
              n_aktuell = sum(ki_aktuell, na.rm = TRUE)) %>%
    mutate(konfint_nedre = map_dbl(konfint, pluck, 1),
           konfint_ovre = map_dbl(konfint, pluck, 2)) %>%
    select(!!!groups(d_ki_ind), est, konfint_nedre, konfint_ovre, n_aktuell)
}

