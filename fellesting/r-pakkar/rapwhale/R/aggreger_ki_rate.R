#' Regn ut kvalitetsindikator for rater
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#' @seealso
#' Om du heller er interessert i andelsdata (teller/nevner),
#' altså data som følger en binomisk fordeling,
#' bruk [aggreger_ki_prop()].
#'
#' @return
#' Ugruppert `tibble` eller `data.frame`
#' (avhengig av inndataene)
#' med følgende kolonner:
#' \item{est}{Kvalitetsindikatoren, dvs. estimert rate
#' (sum(`ki_antall`)/sum(`ki_eksponering`)*`multiplikator`).}
#' \item{konfint_nedre}{Nedre konfidensgrense for `est`.}
#' \item{konfint_ovre}{Øvre konfidensgrense for `est`.}
#'
#' @export
#'
#' @examples
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
#' populasjon = c(1100000, 3100000, 700000, 480000)
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
aggreger_ki_rate = function(d_ki_ind, alfa = 0.05, multiplikator = 1) {
  # Teste inndata
  if (!(
    is.data.frame(d_ki_ind) &&
      all(hasName(d_ki_ind, c("ki_antall", "ki_eksponering", "ki_aktuell")))
  )) {
    stop(paste0(
      "Inndata må være tibble/data.frame med kolonnene «ki_antall», ",
      "«ki_eksponering» og «ki_aktuell»"
    ))
  }
  if (!(
    is.numeric(d_ki_ind$ki_antall) &&
      is.numeric(d_ki_ind$ki_eksponering) &&
      is.logical(d_ki_ind$ki_aktuell)
  )) {
    stop(paste0(
      "Kriterievariablen «ki_antall» og «ki_eksponering» må være numerisk, ",
      "mens «ki_aktuell» må være logisk"
    ))
  }
  if (!all(d_ki_ind$ki_aktuell %in% c(TRUE, FALSE))) {
    stop("«ki_aktuell» må være TRUE eller FALSE")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_antall))) {
    stop("«ki_antall» kan ikke være NA om «ki_aktuell» er TRUE")
  }
  if (any(d_ki_ind$ki_aktuell & is.na(d_ki_ind$ki_eksponering))) {
    stop("«ki_eksponering» kan ikke være NA om «ki_aktuell» er TRUE")
  }
  if (!all(
    (d_ki_ind$ki_aktuell & d_ki_ind$ki_antall >= 0) |
      (!d_ki_ind$ki_aktuell &
        (d_ki_ind$ki_antall >= 0) | is.na(d_ki_ind$ki_antall))
  )) {
    stop("«ki_antall» kan ikke være mindre enn 0")
  }
  if (!all(
    (d_ki_ind$ki_aktuell & d_ki_ind$ki_eksponering > 0) |
      (!d_ki_ind$ki_aktuell &
        (d_ki_ind$ki_eksponering > 0) | is.na(d_ki_ind$ki_eksponering))
  )) {
    stop("«ki_eksponering» kan ikke være mindre enn eller lik 0")
  }
  if (length(alfa) != 1) {
    stop("«alfa» må ha lengde 1")
  }
  if (length(multiplikator) != 1) {
    stop("«multiplikator» må ha lengde 1")
  }
  if (!is.numeric(alfa) | alfa <= 0 | alfa >= 1) {
    stop("«alfa» må være et tall mellom 0 og 1")
  }
  if (!is.numeric(multiplikator) | multiplikator <= 0 | is.na(multiplikator)) {
    stop("«multiplikator» må være et positivt tall")
  }
  if (any(group_size(d_ki_ind) == 0)) {
    warning("Det finnes grupper uten observasjoner i grupperingsvariabel")
  }

  konfint = function(antall, eksponering) {
    if (length(antall) == 0 | length(eksponering) == 0) {
      konfint_nedre = NA_real_
      konfint_ovre = NA_real_
    } else if (sum(antall) == 0) {
      konfint_nedre = 0
      konfint_ovre = -log(alfa) / sum(eksponering)
    } else {
      konfint = glm(
        sum(antall) ~ 1,
        family = "poisson",
        offset = log(sum(eksponering))
      ) %>%
        profile() %>%
        confint(level = 1 - alfa) %>%
        exp()
      konfint_nedre = konfint[[1]]
      konfint_ovre = konfint[[2]]
    }
    c(konfint_nedre, konfint_ovre)
  }

  d_ki_ind %>%
    dplyr::summarise(
      est = (sum(ki_antall[ki_aktuell], na.rm = TRUE) /
        sum(ki_eksponering[ki_aktuell], na.rm = TRUE)) %>%
        replace_na(NA), # Gjør NaN om til NA
      konfint = list(
        konfint(ki_antall[ki_aktuell], ki_eksponering[ki_aktuell])
      ),
      .groups = "keep"
    ) %>%
    dplyr::mutate(
      est = est * multiplikator,
      konfint_nedre = map_dbl(konfint, pluck, 1) * multiplikator,
      konfint_ovre = map_dbl(konfint, pluck, 2) * multiplikator
    ) %>%
    dplyr::select(group_cols(d_ki_ind), est, konfint_nedre, konfint_ovre) %>%
    dplyr::ungroup()
}
