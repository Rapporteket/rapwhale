#' @import dplyr
#' @importFrom tibble tibble rowid_to_column add_row
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider nest unnest
NULL
#' Skår spørreskjema ved hjelp av verditabell
#'
#' @description
#' Regner ut sumskår(er) og validerer spørreskjema-variablene i et
#' datasett.
#'
#' @param d Dataramme/tibble som inneholder spørreskjema-variabler +
#'     eventuelt andre variabler. Spørreskjema-variablene må være
#'     numeriske.
#' @param variabelnavn Navn på variabler i datasettet som ikke er
#'     identiske med de standardiserte navnene i skåringstabellen. Bruk
#'     syntaksen
#'     `c(std_navn_1 = "dd_navn_1", std_navn_2 = "dd_navn_2")`.
#'     Nye navn trenger kun oppgis for spørreskjema-variabler som har
#'     avvikende navn fra skåringstabellen. Hvis `NULL`, blir
#'     det antatt at alle navnene er i samsvar med skåringstabellen.
#' @param skaaringstabell Dataramme/tibble med som sier hvordan
#'     `d` skal skåres. Må ha fire kolonner,
#'     `delskala` (tekst), `variabel` (tekst), `verdi` (numerisk) og
#'     `koeffisient` (numerisk), og det kan bare finnes én rad
#'     per kombinasjon av av `delskala`, `variabel` og `verdi`.
#'     Se detaljer nedenfor.
#' @param godta_manglende Skal manglende verdier i
#'     spørreskjema-variablene i `d` godtas (som standard nei)?
#'     Hvis ikke, blir det gitt ut
#'     en feilmelding om det finnes manglende verdier.
#'     Se detaljer nedenfor.
#'
#' @details
#' Funksjonen regner ut vektede sumskårer ved hjelp av en skåringstabell.
#' Skåringstabellen sier hvordan de ulike svaralternativene for variablene
#' i `d` skal inngå i sumskårene. Et spørreskjema kan ha flere
#' sumskårer, som vi kaller *delskalaer*. (For eksempel kan et skjema for
#' livskvalitet ha én delskala for fysisk livskvalitet og én for psykisk
#' livskvalitet.) Sumskåren for delskalaen `delskala` er summen av de tallene
#' i `koeffisient` som svarer til svaralternativ `verdi` for variabelen
#' `variabel`.
#'
#' Merk at `verdi` også *kan* være `NA`. Da blir manglende svar
#' (dvs. `NA-verdier`) for tilhørende variabel og delskala regnet som et
#' gyldig svar, og har en tilhørende `koeffisient` (som ikke er `NA`).
#' Hvis argumentet `godta_manglende` er satt til `FALSE`, vil en få
#' feilmelding dersom det finnes `NA`-verdier i datasettet som *ikke* har
#' en `verdi` lik `NA` i skåringstabellen for alle delskalene.
#'
#' Funksjonen sjekker at alle verdier for variabler i `d` som skal skåres,
#' er gyldige. Det sjekkes også om `skaaringstabell` er gyldig. Feilmelding
#' blir gitt dersom noe er ugyldig.
#'
#' @return Datasett likt `d`, men med sumskår(er) lagt til, eventuelt
#'   erstattet. Nye sumskår-kolonner blir i utgangspunktet lagt til på
#'   slutten av av `d`, i samme rekkefølge som i `skaaringstabell$delskala`.
#'   Men hvis `d` alt innholder variabler med samme navn som en `delskala`,
#'   blir disse variablene stående der de er, men overskrevet med
#'   nyutregnede sumskårer. Det blir i så fall gitt ut en advarsel.
#' @export
#'
#' @examples
#' d_eks = tibble::tribble(
#'   ~pas_id, ~var_a, ~var_b, ~dato,
#'   146, 1, 2, "2020-05-15",
#'   211, NA, 3, "2020-08-17"
#' )
#'
#' skaaringstabell_eks = tibble::tribble(
#'   ~delskala, ~variabel, ~verdi, ~koeffisient,
#'   "total", "var_a", 1, 5,
#'   "total", "var_a", 2, 10,
#'   "total", "var_a", NA, 7.5,
#'   "total", "var_b", 1, 2,
#'   "total", "var_b", 2, 3,
#'   "total", "var_b", 3, 7,
#'   "psykisk", "var_a", 1, 0,
#'   "psykisk", "var_a", 2, 100,
#'   "psykisk", "var_a", NA, 50
#' )
#'
#' skaar_datasett(d_eks, skaaringstabell = skaaringstabell_eks)
skaar_datasett = function(d, variabelnavn = NULL, skaaringstabell,
                          godta_manglende = FALSE) {
  if (!is.null(variabelnavn)) {
    d_std_navn = rename(d, !!variabelnavn)
  } else {
    d_std_navn = d
  }
  sjekk_skaaringstabell(skaaringstabell)
  sjekk_variabelnavn(d_std_navn, variabelnavn = skaaringstabell$variabel)
  d_akt = d_std_navn %>%
    select(unique(skaaringstabell$variabel))
  sjekk_variabelverdier(d_akt,
    verditabell = select(skaaringstabell, variabel, verdi),
    godta_manglende = godta_manglende
  )
  d_sumskaarer = skaar_datasett_uten_validering(d_akt, skaaringstabell)
  d_orig_inkl_sumskaar = legg_til_eller_erstatt_kolonner(d, d_sumskaarer)
  d_orig_inkl_sumskaar
}

#' Sjekk om skåringstabellen er gyldig
#'
#' @description
#' Gir feilmelding hvis skåringstabellen ikke er gyldig.
#'
#' @param skaaringstabell Dataramme/tibble med fire kolonner
#'     (`delskala`, `variabel`, `verdi` og `koeffisient`).
#'
#' @details
#' Sjekker at `skaaringstabell` inneholder riktige kolonner og riktige
#' variabeltyper. Sjekker også at den ikke inneholder dupliserte verdier
#' for en variabel innenfor samme delskala og at koeffisient-kolonnen
#' ikke inneholder NA-verdier. Skal gi feilmelding hvis `skaaringstabell`
#' er ugyldig.
#'
#' @keywords internal
#'
#' @return `NULL`
sjekk_skaaringstabell = function(skaaringstabell) {
  if (!all(hasName(skaaringstabell, c(
    "delskala", "variabel", "verdi",
    "koeffisient"
  )))) {
    stop("Skåringstabellen må inneholde kolonnene 'delskala', 'variabel', 'verdi' og 'koeffisient'")
  }

  # Lager en oversikt over hvilke variabler som, innenfor delskala,
  # har dupliserte 'verdi'-er.
  d_med_ant_dupl = skaaringstabell %>%
    count(delskala, variabel, verdi, name = "n_rader")

  if (any(d_med_ant_dupl$n_rader != 1)) {
    stop("Skåringstabellen kan ikke inneholde dupliserte verdier for en variabel innenfor samme delskala")
  }

  if (any(is.na(skaaringstabell$koeffisient))) {
    stop("Koeffisient-kolonnen i skåringstabellen kan ikke inneholde NA-verdier")
  }

  if (!(is.character(skaaringstabell$delskala) &&
    is.character(skaaringstabell$variabel) &&
    is.numeric(skaaringstabell$verdi) &&
    is.numeric(skaaringstabell$koeffisient))) {
    stop("Delskala-kolonnen og variabel-kolonnen må være av typen tekst og verdi-kolonnen og koeffisient-kolonnen må være numeriske")
  }
}

#' Sjekk at oppgitte variabler finnes i datasettet
#'
#' @description
#' Skal ta inn et datasett og en vektor med variabelnavn. Funksjonen gir
#' feilmelding hvis datasettet ikke inneholder alle variabelnavnene i
#' skåringstabellen.
#'
#' @param d Dataramme/tibble.
#' @param variabelnavn Vektor med variabelnavn (som skal sjekkes om
#'     finnes i `d`).
#'
#' @details
#' Gir feilmelding hvis `d` ikke inneholder alle variabelnavn
#' i skåringstabellen. Funksjonen oppgir også hvilke variabelnavn som
#' ev. mangler i `d`.
#'
#' @keywords internal
#'
#' @return `NULL`
sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

#' Sjekk at verdier i datasettet er gyldige
#'
#' @description
#' Skal ta inn et datasett, en verditabell og et argument som bestemmer
#' om NA-verdier skal regnes som gyldige. Funksjonen gir feilmelding
#' hvis datasettet inneholder en eller flere ugyldige verdier.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `verditabell$variabel`.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel`
#'     og `verdi`) som sier hvilke verdier som er gyldige for hvilke
#'     variabler. Variabel-kolonnen må være av typen tekst og
#'     verdi-kolonnen må være numerisk.
#' @param godta_manglende Skal manglende verdier for
#'     spørreskjema-variablene i `d` godtas? Hvis ikke, blir det gitt ut
#'     en feilmelding om det finnes manglende verdier.
#'     Standardverdi: `FALSE`
#'
#' @details
#' Gir feilmelding hvis `d` inneholder verdier som ikke er numeriske.
#' Gir også feilmelding hvis `verditabell` ikke er tibble/data.frame
#' og/eller mangler en av / begge kolonnene `variabel` og `verdi`. Hvis
#' det finnes ugyldige verdier i `d` gir funksjonen ut oversikt over
#' antall ugyldige verdier, samt hvilke variabler og verdier dette
#' gjelder.
#'
#' @keywords internal
#'
#' @return `NULL`
sjekk_variabelverdier = function(d, verditabell, godta_manglende) {
  if (!all(sapply(d, is.numeric))) {
    stop("Datasettet inneholder verdier som ikke er numeriske")
  }

  if (!(is.data.frame(verditabell) &&
    all(hasName(verditabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }

  d_ugyldige_verdier = finn_ugyldige_verdier(d, verditabell)

  if (godta_manglende) {
    d_ugyldige_verdier = filter(d_ugyldige_verdier, !is.na(feilverdi))
  }

  oppsummering = oppsummer_ugyldige_verdier(d_ugyldige_verdier)

  if ((nrow(d_ugyldige_verdier) > 0)) {
    stop(oppsummering)
  }
}

#' Finn ugyldige verdier i datasettet
#'
#' @description
#' Gir ut en tibble med oversikt over alle ugyldige verdier i datasettet
#' som blir tatt inn. Hvis datasettet ikke inneholder noen ugyldige
#' verdier, gir funksjonen ut en tibble med null rader.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `verditabell$variabel`. Alle kolonnene må inneholde
#'     numeriske verdier.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel` og
#'     `verdi`) som sier hvilke verdier som er gyldige for hvilke
#'     variabler. Variabel-kolonnen må være av typen tekst og
#'     verdi-kolonnen må være numerisk.
#'
#' @details
#' Ved bruk av den overordnede funksjonen [skaar_datasett()] kalles
#' denne funksjonen på etter at skåringstabellen og variabelnavnene er
#' validert og alle variabler som har navn som ikke finnes i
#' `verditabell$variabel` er filtrert vekk.
#'
#' @keywords internal
#'
#' @return Tibble som inneholder tre kolonner (`radnr`, `variabel` og
#'     `feilverdi`). Sortert etter radnummer og så rekkefølge i `d`.
#'     Hvis `d` ikke inneholder noen ugyldige verdier, vil tibble-en ha
#'     null rader.
#' @seealso [oppsummer_ugyldige_verdier]
finn_ugyldige_verdier = function(d, verditabell) {
  radnr_ugyldige = integer()
  variabler_ugyldige = character()
  verdier_ugyldige = numeric()
  for (var_d in names(d)) {
    verdier_d = d[[var_d]]
    mulige_verdier = verditabell %>%
      filter(variabel == !!var_d) %>%
      pull(verdi)
    verdier_d_ugyldige = setdiff(verdier_d, mulige_verdier)
    if (length(verdier_d_ugyldige) > 0) {
      indeks_ugyldige = which(verdier_d %in% verdier_d_ugyldige)
      radnr_ugyldige = append(radnr_ugyldige, indeks_ugyldige)
      variabler_ugyldige = append(
        variabler_ugyldige,
        rep(var_d, times = length(indeks_ugyldige))
      )
      verdier_ugyldige = append(
        verdier_ugyldige,
        verdier_d[indeks_ugyldige]
      )
    }
  }
  oversikt_ugyldige = arrange(
    tibble(
      radnr = radnr_ugyldige,
      variabel = variabler_ugyldige,
      feilverdi = verdier_ugyldige
    ),
    radnr
  )
  oversikt_ugyldige
}

#' Presenter ugyldige verdier i datasettet på en god måte
#'
#' @description
#' Gir ut en oversiktlig fremstilling av tibble-en som returneres av
#' [finn_ugyldige_verdier()].
#'
#' @param d_ugyldige Tibble på formatet gitt ut av
#'     [finn_ugyldige_verdier()].
#'
#' @keywords internal
#'
#' @return Tekststreng som inneholder variabelnavn og tilhørende
#'     feilverdier (sortert alfabetisk etter variabelnavn og så
#'     rekkefølge i `d_ugyldige$radnr`). Hvis det ikke finnes ugyldige
#'     verdier returneres tekststrengen "Alle verdiene er gyldige".
oppsummer_ugyldige_verdier = function(d_ugyldige) {
  if (nrow(d_ugyldige) > 0) {
    oppsummert = d_ugyldige %>%
      group_by(variabel) %>%
      summarise(feil_verdier = paste0(feilverdi, collapse = ", ")) %>%
      summarise(feil_variabler_verdier = paste0(variabel, ": ",
        feil_verdier,
        collapse = "\n"
      )) %>%
      summarise(feiltekst = paste0(
        "Fant ", nrow(d_ugyldige),
        " ugyldige verdier:\n",
        feil_variabler_verdier
      ))
    oppsummert = pull(oppsummert, feiltekst)
  } else {
    oppsummert = "Alle verdiene er gyldige"
  }
  oppsummert
}

#' Regn sumskår
#'
#' @description
#' Skårer et datasett og gir ut en dataramme/tibble som inneholder en
#' eller flere kolonner med sumskårer.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `skaaringstabell$variabel`. Alle kolonnene må
#'     inneholde numeriske verdier.
#' @param skaaringstabell Dataramme/tibble med fire kolonner
#'     (`delskala`, `variabel`, `verdi` og `koeffisient`).
#'     Delskala-kolonnen og variabel-kolonnen må være av typen tekst og
#'     verdi-kolonnen og koeffisient-kolonnen må være numeriske.
#'
#' @details
#' Funksjonen regner ut sumskårer ved hjelp av en skåringstabell. Hvert
#' svaralternativ for hvert spørsmål i hver delskala har en tilhørende
#' koeffisient i skåringstabellen. Aktuelle koeffisienter blir summert
#' sammen for å finne sumskåren. Ved bruk av den overordnede funksjonen
#' [skaar_datasett()] kalles denne funksjonen på etter at
#' skåringstabellen, variabelnavnene og variabelverdiene er validert
#' (ved hjelp av [sjekk_skaaringstabell()], [sjekk_variabelnavn()] og
#' [sjekk_variabelverdier()]) og alle variabler som har navn som ikke
#' finnes i `skaaringstabell$variabel` er filtrert vekk.
#'
#' @keywords internal
#'
#' @return Dataramme/tibble som inneholder en eller flere kolonner med
#'     sumskårer. Rekkefølgen på sumskår-kolonnene bestemmes av
#'     rekkefølgen i `skaaringstabell$delskala` og rekkefølgen på radene
#'     er lik som i `d`. Hvis en variabel i `d` har `NA-verdier` (uten
#'     tilhørende koeffisient i skåringstabellen), blir sumskåren(e) for
#'     tilhørende delskala(er) satt lik `NA`.
skaar_datasett_uten_validering = function(d, skaaringstabell) {
  # Gjer om til éi rad per svar, med ein person-ID
  # som seier kva rad svaret opphavleg kom frå
  d_svar = d %>%
    rowid_to_column("person_id") %>%
    pivot_longer(-person_id, names_to = "variabel", values_to = "verdi")

  # Legg til rader med NA-verdiar i skåringstabellen, slik at
  # datasettrader med manglande verdiar for ein variabel
  # automatisk får sumskår lik NA for sumskårane som brukar variabelen
  skaaringstabell = legg_til_na_i_skaaringstabell(skaaringstabell)

  # For kvart svar, legg til tilhøyrande koeffisientar
  # (kan vera fleire per svar, dersom det finst fleire delskalaar)
  d_med_koeff = d_svar %>%
    left_join(skaaringstabell, by = c("variabel", "verdi"))

  # Rekn ut sumskår for alle delskalaane, per pasient
  d_med_skaarar = d_med_koeff %>%
    group_by(person_id, delskala) %>%
    summarise(skaar = sum(koeffisient)) %>%
    ungroup() %>%
    pivot_wider(names_from = "delskala", values_from = "skaar")

  # For sikkerheits skuld, sorter etter opphavleg radnummer,
  # og fjern så radnummeret
  d_med_skaarar = d_med_skaarar %>%
    arrange(person_id) %>%
    select(-person_id)

  # Inndata med 0 rader må handterast spesielt
  if (nrow(d) == 0) {
    # Brukar pivot_wider() også her, for å sikra at rekkjefølgja
    # på sumskårkolonnane vert lik som for datasett *med* svar
    # (jf. pivot_wider() si handtering av faktorvariablar)
    d_med_skaarar = skaaringstabell %>%
      distinct(delskala, .keep_all = TRUE) %>%
      select(delskala, koeffisient) %>% # Treng «koeffisient» for å få riktig variabeltype
      pivot_wider(names_from = "delskala", values_from = koeffisient) %>%
      head(0)
  }

  # Rekkefølgen til sumskår-kolonnene skal tilsvare
  # rekkefølgen i delskala-kolonnen i skåringstabellen
  d_med_skaarar = d_med_skaarar[, unique(skaaringstabell$delskala)]

  # Returner ferdig skåra datasett
  d_med_skaarar
}

#' Legg til NA-rader i skåringstabell
#'
#' @description
#' Legg til rader med både `verdi` og `koeffisient` lik `NA`, for
#' lettare skåring av datasett som har manglande verdiar.
#'
#' @param skaaringstabell Vanleg skåringstabell.
#'
#' @details
#' For kvar eksisterande kombinasjon av `delskala` og `variabel` vert
#' det lagt til ei rad med både `verdi` og `koeffisient` sett til `NA`.
#' Dette vert berre gjort dersom ikkje alt finst ein `verdi` lik `NA`
#' (uavhengig av kva tilhøyrande `koeffisient` er).
#'
#' Dette er berre ein intern hjelpefunksjon for å forenkla koden
#' for skåring av datasett. Med slike `NA`-rader vert skåren utrekna
#' med [skaar_datasett_uten_validering()] automatisk `NA` dersom
#' minst éin av variablane som inngår, manglar verdi (altså er `NA`).
#'
#' @keywords internal
#'
#' @return Skåringstabellen med `NA`-rader lagde til.
legg_til_na_i_skaaringstabell = function(skaaringstabell) {
  # Legg til NA-rad viss det ikkje finst frå før
  legg_til_na_rad = function(df) {
    if (all(!is.na(df$verdi))) {
      df = add_row(df, verdi = NA, koeffisient = NA)
    }
    df
  }
  skaaringstabell %>%
    nest(data = c(verdi, koeffisient)) %>%
    mutate(data = map(data, legg_til_na_rad)) %>%
    unnest(cols = c(data))
}

#' Legg til / erstatt variabler i ett datasett med variabler i et annet
#'
#' @description
#' Legger variabler fra et datasett til et annet datasett (med
#' overskriving av variabler som finnes i begge).
#'
#' @param d_orig Originalt datasett (dataramme/tibble).
#' @param d_ekstrakol Dataramme/tibble som inneholder en eller flere
#'     kolonner og samme antall rader som `d_orig`.
#'
#' @details
#' Variabler som finnes i `d_orig` fra før blir erstattet, nye blir lagt
#' til helt til høyre. Det kommer advarsel hvis en eller flere variabler
#' finnes fra før.
#'
#' @keywords internal
#'
#' @return Originalt datasett med ekstra kolonner lagt til / erstattet.
legg_til_eller_erstatt_kolonner = function(d_orig, d_ekstrakol) {
  navn_finst = intersect(names(d_ekstrakol), names(d_orig))
  navn_finst_tekst = paste0(navn_finst, collapse = ", ")

  if (length(navn_finst) > 0) {
    warning(
      "Følgende kolonne(r) i datasettet har blitt overskrevet: ",
      navn_finst_tekst
    )
  }

  d_orig[names(d_ekstrakol)] = d_ekstrakol
  d_orig
}
