#' Overordnet sumskår-funksjon
#'
#' Skal ta inn et datasett, en vektor med variabelnavn og en skåringstabell.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#' @param skaaringstabell Skåringstabell med tre kolonner (variabel, verdi, koeffisient).
#'
#' @return \code{d} utvidet med en eller flere kolonner som inneholder sumskår(er).
#'     fixme: skal bare sumskår(er) returneres?
#'     skal skaaringstabell være et argument i denne funksjonen?

skaar_datasett = function(d, variabelnavn, skaaringstabell) {
  # d = endre_variabelnavn(d, variabelnavn)
  sjekk_skaaringstabell(skaaringstabell)
  sjekk_variabelnavn(d, variabelnavn = skaaringstabell$variabel)
  sjekk_variabelverdier(d, verditabell = select(skaaringstabell, variabel, verdi))
  skaar_datasett_uten_validering(d, skaaringstabell)
}

#' Funksjon for å endre variabelnavn
#'
#' Skal ta inn et datasett og en vektor med variabelnavn.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor med gamle og nye variabelnavn. fixme: stemmer dette?
#'
#' @return \code{d} med nøyaktig samme variabelnavn som i skåringstabellen. fixme: stemmer dette?

endre_variabelnavn = function(d, variabelnavn) {
  colnames(d) = variabelnavn
  d
}

#' Funksjon for å sjekke variabelnavn
#'
#' Skal ta inn et datasett og en vektor som inneholder variabelnavnene i skåringstabellen.
#' Funksjonen gir feilmelding hvis datasettet inneholder variabelnavn som ikke finnes i skåringstabellen.
#'
#' @param d Dataramme/tibble med en kolonne for hvert spørsmål.
#' @param variabelnavn Vektor som inneholder variabelnavnene i skåringstabellen.
#'
#' @return Skal gi feilmelding hvis \code{d} inneholder variabelnavn som ikke
#'     finnes i skåringstabellen. Sumskår blir da ikke regnet ut. Funksjonen
#'     oppgir også hvilke variabelnavn som er ugyldige.

sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

#' Funksjon for å sjekke variabelverdier
#'
#' Skal ta inn et datasett, en verditabell og et argument som bestemmer om NA-verdier skal
#' regnes som gyldige. Funksjonen gir feilmelding hvis verditabellen ikke er satt opp
#' riktig og/eller hvis datasettet inneholder verdier som ikke finnes i verditabellen.
#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i 'variabel'-kolonnen i \code{verditabell}.
#' @param verditabell Dataramme/tibble med to kolonner ('variabel' og 'verdi'), som sier hvilke verdier
#'     som er gyldige for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gyldige (selv om de ikke er nevnt i \code{verditabell}).
#'
#' @return Skal gi feilmelding hvis \code{verditabell} ikke er tibble/data.frame
#'     og/eller mangler en av / begge kolonnene 'variabel' og 'verdi'. Hvis alle verdiene
#'     i \code{d} er gyldige skal funksjonen gi beskjed om dette, og hvis det finnes
#'     ugyldige verdier i \code{d} skal funksjonen gi beskjed om hvilke variabler og
#'     verdier dette gjelder. Sumskår blir da ikke regnet ut.
#'     fixme: sjekk_variabelverdier må utvides slik at den gir beskjed om at alle verdier
#'     er gyldige dersom dette er tilfellet og at den tar hensyn til om manglende verdier
#'     skal bli godtatt eller ikke.

sjekk_variabelverdier = function(d, verditabell, godta_manglende) {
  if (!(is.data.frame(verditabell) &&
    all(hasName(verditabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }
  d_ugyldige_verdier = finn_ugyldige_verdier(d, verditabell, godta_manglende)
  if (nrow(d_ugyldige_verdier) > 0) {
    oppsummering = oppsummer_ugyldige_verdier(d_ugyldige_verdier)
    stop(oppsummering)
  }
}

#' Funksjon for å finne ugyldige verdier
#'
#' Gir ut oversikt over ugyldige verdier i et datasett.
#'
#' @param d Dataramme/tibble med nøyaktig samme variabelnavn som finnes i 'variabel'-kolonnen i \code{verditabell}.
#' @param verditabell Dataramme/tibble med to kolonner ('variabel' og 'verdi'), som sier hvilke verdier som er gyldige
#'     for hvilke variabler.
#' @param godta_manglende Om NA-verdier skal regnes som gyldige (selv om de ikke er nevnt i \code{verditabell}).
#'
#' @return Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de ugyldige verdiene.
#'     Sortert etter radnummer og så rekkefølge i \code{d}.

finn_ugyldige_verdier = function(d, verditabell, godta_manglende) {
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
      variabler_ugyldige = append(variabler_ugyldige, rep(var_d, times = length(indeks_ugyldige)))
      verdier_ugyldige = append(verdier_ugyldige, verdier_d[indeks_ugyldige])
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

#' Funksjon for å presentere ugyldige verdier på en god måte
#'
#' Gir ut en oversiktlig fremstilling av ugyldige verdier i et datasett.
#'
#' @param d_ugyldige Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de
#'     ugyldige verdiene. \code{d_ugyldige} blir returnert av funksjonen \code{finn_ugyldige_verdier()}.
#'
#' @return Tekststreng som inneholder variabelnavn og tilhørende feilverdier (sortert alfabetisk
#'     etter variabelnavn). Hvis det ikke finnes ugyldige verdier returneres tekststrengen
#'     "Alle verdiene er gyldige".

oppsummer_ugyldige_verdier = function(d_ugyldige) {
  if (nrow(d_ugyldige) > 0) {
    oppsummert = d_ugyldige %>%
      group_by(variabel) %>%
      summarise(feil_verdier = paste0(feilverdi, collapse = ", ")) %>%
      summarise(feil_variabler_verdier = paste0(variabel, ": ", feil_verdier, collapse = "\n")) %>%
      summarise(feiltekst = paste0("Fant ", nrow(d_ugyldige), " ugyldige verdier:\n", feil_variabler_verdier))
    pull(oppsummert, feiltekst)
  } else {
    "Alle verdiene er gyldige"
  }
}

skaar_datasett_uten_validering = function(d, skaaringstabell) {
  # Gjer om til éi rad per svar, med ein person-ID
  # som seier kva rad svaret opphavleg kom frå
  d_svar = d %>%
    tibble::rowid_to_column("person_id") %>%
    tidyr::pivot_longer(-person_id, names_to = "variabel", values_to = "verdi")

  # Legg til rader med NA-verdiar i skåringstabellen, slik at
  # datasettrader med manglande verdiar for ein variabel
  # automatisk får sumskår lik NA for sumskårane som brukar variabelen
  skaaringstabell = legg_til_na_i_skaaringstabell(skaaringstabell)

  # For kvart svar, legg til tilhøyrande koeffisientar
  # (kan vera fleire per svar, dersom det finst fleire delskalaar)
  d_med_koeff = d_svar %>%
    dplyr::left_join(skaaringstabell, by = c("variabel", "verdi"))

  # Rekn ut sumskår for alle delskalaane, per pasient
  d_med_skaarar = d_med_koeff %>%
    group_by(person_id, delskala) %>%
    summarise(skaar = sum(koeffisient)) %>%
    ungroup() %>%
    tidyr::pivot_wider(names_from = "delskala", values_from = "skaar")

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
      dplyr::distinct(delskala, .keep_all = TRUE) %>%
      select(delskala, koeffisient) %>% # Treng «koeffisient» for å få riktig variabeltype
      tidyr::pivot_wider(names_from = "delskala", values_from = koeffisient) %>%
      head(0)
  }

  # Returner ferdig skåra datasett
  d_med_skaarar
}



#' Legg til NA-rader i skåringstabell
#'
#' @description
#' Legg til rader med både `verdi` og `koeffisient` lik `NA`, for lettare
#' skåring av datasett som har manglande verdiar.
#'
#' @param skaaringstabell Vanleg skåringstabell.
#'
#' @details
#' For kvar eksisterande kombinasjon av `delskala` og `variabel` vert det lagt til
#' ei rad med både `verdi` og `koeffisient` sett til `NA`.
#' Dette vert berre gjort dersom ikkje alt finst ein `verdi` lik `NA` (uavhengig
#' av kva tilhøyrande `koeffisient` er).
#'
#' Dette er berre ein intern hjelpefunksjon for å forenkla koden
#' for skåring av datasett. Med slike `NA`-rader vert skåren utrekna
#' med [rapwhale::skaar_datasett_uten_validering()] automatisk `NA` dersom
#' minst éin av variablane som inngår, manglar verdi (altså er `NA`).
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



sjekk_skaaringstabell = function(skaaringstabell) {
  # Sjekker om skåringstabellen inneholder riktige kolonner/kolonnenavn.
  if (!all(hasName(skaaringstabell, c("delskala", "variabel", "verdi", "koeffisient")))) {
    stop("Skåringstabellen må inneholde kolonnene 'delskala', 'variabel', 'verdi' og 'koeffisient'")
  }

  # Lager en oversikt over hvilke variabler som, innenfor delskala,
  # har dupliserte 'verdi'-er.
  d_med_ant_dupl = skaaringstabell %>%
    dplyr::count(delskala, variabel, verdi, name = "n_rader")

  # Hvis kolonnen 'ant_dupl' inneholder en eller flere verdier som er ulik 0
  # skal funksjonen stoppe og skrive ut en feilmelding.
  if (any(d_med_ant_dupl$n_rader != 1)) {
    stop("Skåringstabellen kan ikke inneholde dupliserte verdier for en variabel innenfor samme delskala")
  }

  # Hvis koeffisient-kolonnen i skåringstabellen inneholder NA-verdier
  # skal funksjonen stoppe og skrive ut en feilmelding.
  if (any(is.na(skaaringstabell$koeffisient))) {
    stop("Koeffisient-kolonnen i skåringstabellen kan ikke inneholde NA-verdier")
  }

  # Hvis kolonnene i skåringstabellen ikke har riktig format skal funksjonen
  # stoppe og skrive ut en feilmelding.
  if (!(is.numeric(skaaringstabell$verdi) &&
    is.numeric(skaaringstabell$koeffisient) &&
    is.character(skaaringstabell$variabel))) {
    stop("Verdi-kolonnen og koeffisient-kolonnen må bare inneholde numeriske variabler og variabel-kolonnen må bare inneholde tekst-variabler")
  }
}
