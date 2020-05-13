#' Overordnet sumskår-funksjon
#'
#' @description
#' Validerer og regner ut sumskår(er) for spørreskjema-variablene i et datasett.
#'
#' @param d Dataramme/tibble som inneholder spørreskjema-variaber + ev. andre variabler.
#' @param variabelnavn Vektor med nye og gamle variabelnavn.
#' @param skaaringstabell Skåringstabell med fire kolonner (`delskala`, `variabel`, `verdi` og `koeffisient`).
#' @param godta_manglende Bestemmer om manglende verdier i `d` skal godtas eller ikke.
#'
#' @details
#' Funksjonen sjekker at variabler og verdier tilknyttet spørreskjemaet som skal skåres er
#' gyldige. Det sjekkes også at `skaaringstabell` er gyldig. Funksjonen gir ut feilmelding hvis
#' noe er ugyldig. Hvis `d` ikke inneholder noen sumskår-kolonner med identisk navn som i `skaaringstabell$delskala`,
#' legges de til i alfabetisk rekkefølge til slutt i `d`. Hvis `d` inneholder en eller flere sumskår-kolonner med
#' identisk navn som i `skaaringstabell$delskala` blir disse kolonnene stående samme sted, men sumskårene
#' regnes ut på nytt. Sumskår-kolonner som ikke allerede finnes legges til i alfabetisk rekkefølge til slutt i `d`.
#'
#' @return `d` med sumskår(er).

skaar_datasett = function(d, variabelnavn = NULL, skaaringstabell, godta_manglende = FALSE) {
  if (!is.null(variabelnavn)) {
    d_navn_ok = rename(d, !!variabelnavn)
  } else {
    d_navn_ok = d
  }
  sjekk_skaaringstabell(skaaringstabell)
  sjekk_variabelnavn(d_navn_ok, variabelnavn = skaaringstabell$variabel)
  d_akt = d_navn_ok %>%
    select(unique(skaaringstabell$variabel)) %>%
    mutate_if(is.character, as.numeric)
  sjekk_variabelverdier(d_akt, verditabell = select(skaaringstabell, variabel, verdi), godta_manglende = godta_manglende)
  d_sumskaarer = skaar_datasett_uten_validering(d_akt, skaaringstabell)
  legg_til_eller_erstatt_kolonner(d, d_sumskaarer)
}

#' Funksjon for å sjekke variabelnavn
#'
#' @description
#' Skal ta inn et datasett og en vektor med gyldige variabelnavn. Funksjonen gir
#' feilmelding hvis datasettet ikke inneholder alle variabelnavn i skåringstabellen.
#'
#' @param d Dataramme/tibble som inneholder spørreskjema-variaber + ev. andre variabler.
#' @param variabelnavn Vektor som inneholder alle unike variabelnavn i `skaaringstabell$variabel`.
#'
#' @return Skal gi feilmelding hvis `d` ikke inneholder alle variabelnavn
#'     i skåringstabellen. Sumskår blir da ikke regnet ut. Funksjonen
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
#' @description
#' Skal ta inn et datasett, en verditabell og et argument som bestemmer om NA-verdier skal
#' regnes som gyldige. Funksjonen gir feilmelding hvis datasettet inneholder en eller flere
#' ugyldige verdier.
#'
#' @param d Dataramme/tibble som kun inneholder variabler med identiske navn som i `verditabell$variabel`.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel` og `verdi`) som sier hvilke verdier
#'     som er gyldige for hvilke variabler.
#' @param godta_manglende Bestemmer om manglende verdier i `d` skal godtas eller ikke.
#'
#' @return Skal gi feilmelding hvis `verditabell` ikke er tibble/data.frame
#'     og/eller mangler en av / begge kolonnene `variabel` og `verdi`. Hvis det finnes
#'     ugyldige verdier i `d` skal funksjonen gi beskjed om hvilke variabler og
#'     verdier dette gjelder. Sumskår blir da ikke regnet ut.

sjekk_variabelverdier = function(d, verditabell, godta_manglende) {
  if (!all(sapply(d, class) %in% c("character", "numeric"))) {
    stop("Datasettet inneholder verdier som ikke er tekstverdier eller numeriske verdier")
  }

  if (!(is.data.frame(verditabell) &&
    all(hasName(verditabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }
  d_ugyldige_verdier = finn_ugyldige_verdier(d, verditabell)

  if (godta_manglende) {
    d_ugyldige_verdier = na.omit(d_ugyldige_verdier)
  }

  oppsummering = oppsummer_ugyldige_verdier(d_ugyldige_verdier)

  # if (any(is.na(d_ugyldige_verdier$feilverdi))) {
  #  stop("Det mangler verdier for en eller flere variabler\n", oppsummering)
  # }

  if ((nrow(d_ugyldige_verdier) > 0)) {
    stop(oppsummering)
  }
}

#' Funksjon for å finne ugyldige verdier
#'
#' @description
#' Gir ut oversikt over alle ugyldige verdier i datasettet som blir tatt inn. Hvis datasettet ikke
#' inneholder noen ugyldige verdier gir funksjonen ut en dataramme/tibble som kun inneholder
#' kolonnenavnene `radnr`, `variabel` og `feilverdi`.
#'
#' @param d Dataramme/tibble som kun inneholder variabler med identiske navn som i `verditabell$variabel`.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel` og `verdi`) som sier hvilke verdier
#'     som er gyldige for hvilke variabler.
#'
#' @details
#' Ved bruk av den overordnede funksjonen [skaar_datasett()] kalles denne funksjonen på etter at alle
#' variabler som ikke inngår i sumskår-beregningene er filtrert vekk og variabelnavnene og skåringstabellen
#' er validert.
#'
#' @return Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de ugyldige verdiene.
#'     Sortert etter radnummer og så rekkefølge i `d`. Hvis datasettet ikke inneholder noen ugyldige
#'     verdier gir funksjonen ut en dataramme/tibble som kun inneholder kolonnenavnene `radnr`, `variabel`
#'     og `feilverdi`.

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
#' @description
#' Gir ut en oversiktlig fremstilling av datarammen som returneres av [finn_ugyldige_verdier()].
#'
#' @param d_ugyldige Dataramme/tibble som inneholder radnummer, variabelnavn og feilverdi for de ugyldige
#'     verdiene. Sortert etter radnummer og så rekkefølge i `d`. Hvis datasettet ikke inneholder noen
#'     ugyldige verdier skal det være en dataramme/tibble som kun inneholder kolonnenavnene `radnr`,
#'     `variabel` og `feilverdi`.
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

#' Funksjon for å regne ut sumskår
#'
#' @description
#' Gir ut et datasett som inneholder en eller flere kolonner med sumskårer.
#'
#' @param d Dataramme/tibble som kun inneholder variabler med identiske navn som i `skaaringstabell$variabel`.
#' @param skaaringstabell Skåringstabell med fire kolonner (`delskala`, `variabel`, `verdi` og `koeffisient`).
#'
#' @details
#' Ved bruk av den overordnede funksjonen [skaar_datasett()] kalles denne funksjonen på etter at alle
#' variabler som ikke inngår i sumskår-beregningene er filtrert vekk og variabelnavnene og skåringstabellen
#' er validert.
#'
#' @return Et datasett som inneholder en eller flere kolonner med sumskårer. Rekkefølgen på sumskår-kolonnene
#'     bestemmes av rekkefølgen på sumskårene i `skaaringstabell$delskala`. Hvis en rad i `d` mangler verdier
#'     for en eller flere variabler, og manglende verdier skal godtas, blir sumskåren for raden `NA`.

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

  # Rekkefølgen til sumskår-kolonnene skal tilsvare
  # rekkefølgen i delskala-kolonnen i skåringstabellen
  d_med_skaarar = d_med_skaarar[, unique(skaaringstabell$delskala)]

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
#' med [skaar_datasett_uten_validering()] automatisk `NA` dersom
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


#' Funksjon for å sjekke skåringstabellen
#'
#' @description
#' Gir feilmelding hvis skåringstabellen er ugyldig.
#'
#' @param skaaringstabell Skåringstabell med fire kolonner (`delskala`, `variabel`, `verdi` og `koeffisient`).
#'
#' @details
#' Sjekker at skåringstabellen inneholder riktige kolonner og riktige variabeltyper. Sjekker også
#' at den ikke inneholder dupliserte verdier for en variabel innenfor samme delskala og at
#' koeffisient-kolonnen ikke inneholder NA-verdier.
#'
#' @return Skal gi feilmelding hvis skåringstabellen er ugyldig. Sumskår blir da ikke regnet ut.

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


# Legger til og/eller erstatter kolonner i datasettet som blir tatt inn
legg_til_eller_erstatt_kolonner = function(d_orig, d_ekstrakol) {
  navn_finst = intersect(names(d_ekstrakol), names(d_orig))
  navn_finst_tekst = paste0(navn_finst, collapse = ", ")

  if (length(navn_finst) > 0) {
    warning("Følgende kolonne(r) i datasettet har blitt overskrevet: ", navn_finst_tekst) # legge til hvilke kolonner det gjelder
  }

  d_orig[names(d_ekstrakol)] = d_ekstrakol

  d_orig
}
