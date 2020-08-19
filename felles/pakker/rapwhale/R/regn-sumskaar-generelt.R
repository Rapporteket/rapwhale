#' @import dplyr
#' @importFrom tibble tibble rowid_to_column add_row
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer pivot_wider nest unnest
NULL
#' Skår spørreskjema basert på skåringstabell
#'
#' @description
#' Regner ut sumskår(er) i et datasett basert på en skåringstabell.
#' Sjekker også at alle verdiene i datasettet er i samsvar med
#' skåringstabellen.
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
#' @param skaaringstabell Dataramme/tibble som sier hvordan
#'     `d` skal skåres. Må ha fire kolonner,
#'     `delskala` (tekst), `variabel` (tekst), `verdi` (numerisk) og
#'     `koeffisient` (numerisk), og det kan bare finnes én rad
#'     per kombinasjon av av `delskala`, `variabel` og `verdi`.
#'     Se detaljer nedenfor.
#' @param godta_manglende Skal manglende verdier (`NA`-verdier) i
#'     spørreskjema-variablene i `d` godtas (som standard nei)?
#'     Hvis ikke, blir det gitt ut
#'     en feilmelding om det finnes manglende verdier.
#'     Se detaljer nedenfor.
#'
#' @details
#' Funksjonen regner ut vektede sumskårer ved hjelp av en skåringstabell.
#' Skåringstabellen sier hvordan de ulike svaralternativene skal inngå i
#' sumskårene. Et spørreskjema kan ha flere
#' sumskårer, som vi kaller *delskalaer*. (For eksempel kan et skjema for
#' livskvalitet ha én delskala for fysisk livskvalitet og én for psykisk
#' livskvalitet.) For hver rad i `d` og hver `delskala` i `skaaringstabell`
#' blir sumskåren `delskala` i `d` lik summen av de tallene i `koeffisient`
#' der `verdi` for variabelen `variabel` i skåringstabellen er lik
#' verdien av tilhørende variabel i `d`.
#'
#' Merk at `verdi` også *kan* være `NA`. Da blir manglende svar
#' (dvs. `NA-verdier`) for tilhørende variabel og delskala regnet som
#' gyldig svar og har en tilhørende `koeffisient` (som ikke er `NA`).
#' Dette kan være nyttig i tilfeller hvor svaralternativ som «Vet ikke»
#' og manglende svar begge blir kodet som `NA` (men slik koding er
#' generelt en dårlig idé!).
#'
#' Hvis argumentet `godta_manglende` er satt til `FALSE`, vil en få
#' feilmelding dersom det finnes `NA`-verdier i datasettet som *ikke* har
#' en `verdi` lik `NA` i skåringstabellen for alle delskalene.
#'
#' Funksjonen gir feilmelding dersom noen av verdiene i `d` ikke er i
#' samsvar med skåringstabellen eller dersom skåringstabellen er ugyldig.
#'
#' @return Datasett likt `d`, men med sumskår(er) lagt til, eventuelt
#'   erstattet. Nye sumskår-kolonner blir i utgangspunktet lagt til på
#'   slutten av av `d`, i samme rekkefølge som i `skaaringstabell$delskala`.
#'   Hvis `d` imidlertid alt innholder en variabel med samme navn som en
#'   `delskala`, blir denne denne stående der den er, men overskrevet med
#'   nyutregnet sumskår. Det blir i så fall gitt ut en advarsel.
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

  skaaringstabell = ungroup(skaaringstabell) # Gruppering kan føra til feil i utrekningane
  sjekk_skaaringstabell(skaaringstabell)

  d_std_navn = d_std_navn %>%
    ungroup() %>% # Gruppering kan føra til feil i utrekningane nedanfor
    # (men blir likevel bevart i utdataa,
    # jf. legg_til_eller_erstatt_kolonner()-kallet)
    select(unique(skaaringstabell$variabel))
  sjekk_variabelnavn(d_std_navn, variabelnavn = skaaringstabell$variabel)
  sjekk_variabelverdier(d_std_navn,
    verditabell = select(skaaringstabell, variabel, verdi),
    godta_manglende = godta_manglende
  )

  d_sumskaarer = skaar_datasett_uten_validering(d_std_navn, skaaringstabell)
  d_orig_inkl_sumskaar = legg_til_eller_erstatt_kolonner(d, d_sumskaarer)
  d_orig_inkl_sumskaar
}

#' Sjekk om skåringstabell er gyldig
#'
#' @description
#' Gir feilmelding hvis skåringstabellen ikke er gyldig.
#'
#' @param skaaringstabell Dataramme/tibble (skåringstabell).
#'
#' @details
#' Sjekker at `skaaringstabell` inneholder riktige kolonner og riktige
#' variabeltyper. En gyldig skåringstabell skal ha fire kolonner,
#' `delskala` (tekst), `variabel` (tekst), `verdi` (numerisk) og
#' `koeffisient` (numerisk), og det skal bare finnes én rad
#' per kombinasjon av av `delskala`, `variabel` og `verdi`.
#' Kolonnen `koeffisient` kan ikke ha `NA`-verdier.
#'
#' Gir ut feilmelding hvis `skaaringstabell` er ugyldig.
#'
#' @keywords internal
#'
#' @return `NULL` (usynlig).
sjekk_skaaringstabell = function(skaaringstabell) {
  if (!is.data.frame(skaaringstabell)) {
    stop("Skåringstabellen må være en dataramme")
  }

  if (!all(hasName(skaaringstabell, c(
    "delskala", "variabel", "verdi",
    "koeffisient"
  )))) {
    stop("Skåringstabellen må inneholde kolonnene 'delskala', 'variabel', 'verdi' og 'koeffisient'")
  }

  if (!(is.character(skaaringstabell$delskala) &&
    is.character(skaaringstabell$variabel) &&
    is.numeric(skaaringstabell$verdi) &&
    is.numeric(skaaringstabell$koeffisient))) {
    stop("'delskala' og 'variabel' må være tekstvariabler og 'verdi' og 'koeffisient' må være numeriske")
  }

  # Antall dubletter for hver kombinasjon av 'delskala', 'variabel' og 'verdi'
  d_med_ant_dupl = skaaringstabell %>%
    count(delskala, variabel, verdi, name = "n_rader")
  if (any(d_med_ant_dupl$n_rader != 1)) {
    stop("Skåringstabellen kan ikke inneholde dupliserte verdier for en variabel innenfor samme delskala")
  }

  # fixme (QA):
  #
  # sjekk_skaaringstabell() godtek at ein variabel manglar
  # oppføringar for enkelte verdiar i ein delskala som han
  # *har* verdiar for i ein annan delskala. Dette er ein feil.
  # Slike skåringstabellar skal ikkje reknast som gyldige.
  # Enkelt eksempel:
  #
  #   stab = tribble(~delskala, ~variabel, ~verdi, ~koeffisient,
  #                  "fys", "var_a", 1, 0,
  #                  "fys", "var_a", 2, 0,
  #                  "psyk", "var_a", 1, 0,
  #                  "pysk", "var_b", 3, 0)
  #
  #
  # Variabelen `var_a` kan ta verdiane 1 og 2 for delskalaen
  # `fys`. Då må han òg kunna ta begge verdiane for andre delskalaar
  # *som inneheld han*, her altså `psyk`. Men for `psyk` kan
  # han berre ta verdien 1, så skåringstabellen skal reknast
  # som ugyldig.
  #
  # Men merk at variabelen `var_b`, som kan ta verdien 3
  # for `psyk`, *ikkje* treng å ta verdien `3` for `fys`,
  # sidan han ikkje er *definert* for denne delskalaen.
  #
  # Verdiane `NA` er spesialtilfelle. At ein variabel kan ta
  # verdien `NA` for éin delskala, skal ikkje vera til hinder
  # for at han *ikkje* skal kunna ta han for andre delskalaar.
  # Viss for eksempel verdien 2 ovanfor vart erstatta med `NA`,
  # skal skåringstabellen reknast som gyldig.
  # (Viss ein observasjon har `var_a` lik `NA`, vil sumskåren for `psyk`
  # naturlegvis verta `NA`.)

  if (any(is.na(skaaringstabell$koeffisient))) {
    stop("Koeffisient-kolonnen i skåringstabellen kan ikke inneholde NA-verdier")
  }

  invisible()
}

#' Sjekk at oppgitte variabler finnes i datasettet
#'
#' @description
#' Skal ta inn et datasett og en vektor med variabelnavn. Funksjonen gir
#' feilmelding hvis datasettet ikke inneholder alle variabelnavnene i
#' datasettet.
#'
#' @param d Dataramme/tibble.
#' @param variabelnavn Vektor med variabelnavn (som skal sjekkes om
#'     finnes i `d`).
#'
#' @details
#' Gir feilmelding hvis `d` ikke inneholder alle variabelnavnene
#' i `variabelnavn`. Feilmeldingen oppgir hvilke variabelnavn som
#' eventuelt mangler.
#'
#' @keywords internal
#'
#' @return `NULL` (usynlig).
sjekk_variabelnavn = function(d, variabelnavn) {
  var_mangler = unique(variabelnavn[!(variabelnavn %in% names(d))])
  var_mangler_tekst = paste0(var_mangler, collapse = ", ")
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

#' Sjekk at verdier i et datasett er gyldige
#'
#' @description
#' Gir feilmelding dersom verdiene i datasettet ikke er i samsvar
#' med en verditabell som sier hvile verdier som er gyldige for
#' hvilke variabler.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `verditabell$variabel`.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel`
#'     og `verdi`) som sier hvilke verdier som er gyldige for hvilke
#'     variabler. Variabel-kolonnen må være av typen tekst og
#'     verdi-kolonnen må være numerisk.
#' @param godta_manglende Skal manglende verdier i
#'     i `d` godtas (standard `FALSE`)?
#'     Hvis ikke, blir det gitt ut
#'     en feilmelding dersom det finnes manglende verdier (`NA`-verdier)
#'     som ikke har tilhørende oppføring i `verditabell`.
#'
#' @details
#' Gir feilmelding hvis `d` inneholder en verdi som ikke har
#' tilhørende oppføring av `variabel` og `verdi` i `verditabell`.
#' Feilmeldingen inneholder oversikt over antall ugyldige verdier,
#' samt hvilke variabler og verdier dette gjelder.
#'
#' Gir også feilmelding hvis `verditabell` ikke er `tibble`/`data.frame`
#' og/eller mangler minst én av kolonnene `variabel` og `verdi`,
#' eller hvis `d` inneholder verdier som ikke er numeriske.
#'
#' @keywords internal
#'
#' @return `NULL` (usynlig).
sjekk_variabelverdier = function(d, verditabell, godta_manglende) {
  if (!(is.data.frame(verditabell) &&
    all(hasName(verditabell, c("variabel", "verdi"))))) {
    stop("Inndata må være tibble/data.frame og inneholde kolonnene 'variabel' og 'verdi'")
  }

  if (!all(sapply(d, is.numeric))) {
    stop("Datasettet inneholder verdier som ikke er numeriske")
  }

  d_ugyldige_verdier = finn_ugyldige_verdier(d, verditabell)
  if (godta_manglende) {
    d_ugyldige_verdier = filter(d_ugyldige_verdier, !is.na(feilverdi))
  }
  oppsummering = oppsummer_ugyldige_verdier(d_ugyldige_verdier)
  if ((nrow(d_ugyldige_verdier) > 0)) {
    stop(oppsummering)
  }

  invisible()
}

#' Finn ugyldige verdier i et datasett
#'
#' @description
#' Gir ut en tibble med oversikt over alle ugyldige verdier i datasettet
#' som blir tatt inn.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `verditabell$variabel`. Alle kolonnene må inneholde
#'     numeriske verdier.
#' @param verditabell Dataramme/tibble med to kolonner (`variabel` og
#'     `verdi`) som sier hvilke verdier som er gyldige for hvilke
#'     variabler. Kolonnen `variabel` må være av typen tekst og
#'     `verdi` må være numerisk.
#'
#' @details
#' Blir brukt av [sjekk_variabelverdier()], men kan også
#' brukes separat. Resultatene kan oppsummeres til en
#' tekststreng med [oppsummer_ugyldige_verdier()].
#'
#' @keywords internal
#'
#' @return Tibble som inneholder tre kolonner, `radnr`, `variabel` og
#'     `feilverdi`. Sortert etter radnummer og så rekkefølge i `d`.
#'     Hvis `d` ikke inneholder noen ugyldige verdier, vil tibble-en ha
#'     null rader.
#' @seealso [sjekk_variabelverdier()], [oppsummer_ugyldige_verdier()]
finn_ugyldige_verdier = function(d, verditabell) {
  d %>%
    rowid_to_column("radnr") %>%
    pivot_longer(-radnr, names_to = "variabel", values_to = "feilverdi") %>%
    anti_join(verditabell, by = c("variabel", feilverdi = "verdi")) %>%
    arrange(radnr)
}

#' Presenter ugyldige verdier i et datasett på en god måte
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
#'     verdier, returneres tekststrengen `"Alle verdiene er gyldige"`.
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

#' Skår garantert gyldig spørreskjema ved hjelp av verditabell
#'
#' @description
#' Skårer et datasett som må være i samsvar med oppgitt verditabell og
#' gir ut en dataramme/tibble som bare inneholder sumskårene.
#'
#' @param d Dataramme/tibble som inneholder kun kolonner med identiske
#'     navn som i `skaaringstabell$variabel`. Alle kolonnene må
#'     inneholde numeriske verdier.
#' @inheritParams skaar_datasett
#'
#' @details
#' Se [skaar_datasett()] for informasjon om hvordan `d` blir skåret
#' og om hvordan `skaaringstabell` skal defineres.
#' Forskjellen fra den funksjonen er at:
#'
#' - `d` skal *bare* inneholde variablene nevnt i `skaaringstabell$variabel`
#'   (ikke ekstravariabler)
#' - `d` *må* være i samsvar med `skaaringstabell`
#' - `skaaringstabell` *må* være gyldig
#' - utdata fra funksjonen inneholder bare sumskårene, ikke de opphavlige
#'   variablene
#'
#' Ved bruk av den overordnede funksjonen
#' [skaar_datasett()] kalles denne funksjonen på etter at
#' variabler har fått nye namn basert på `variabelnavn`-argumentet
#' i den funksjonen,
#' skåringstabellen, variabelnavnene og variabelverdiene er validert
#' (ved hjelp av [sjekk_skaaringstabell()], [sjekk_variabelnavn()] og
#' [sjekk_variabelverdier()]) og alle variabler som ikke
#' finnes i `skaaringstabell$variabel` er filtrert vekk.
#'
#' Dette er altså funksjonen som gjør *hovedarbeidet* med skåring av
#' et datasett, etter at alt «bokholderiet» er unnagjort.
#'
#' @keywords internal
#'
#' @return Tibble som inneholder én eller flere kolonner med
#'     sumskårer. Rekkefølgen på sumskår-kolonnene bestemmes av
#'     rekkefølgen i `skaaringstabell$delskala` og rekkefølgen på radene
#'     er lik som i `d`. Hvis en variabel i `d` har `NA-verdier` uten
#'     tilhørende koeffisient i skåringstabellen, blir sumskåren(e) for
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

  # Rekn ut sumskår for alle delskalaane, per person
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
#' @param skaaringstabell Vanleg skåringstabell. Sjå definisjon
#'   i [skaar_datasett()].
#'
#' @details
#' For kvar eksisterande kombinasjon av `delskala` og `variabel` vert
#' det lagt til ei rad med både `verdi` og `koeffisient` sett til `NA`.
#' Dette vert berre gjort dersom ikkje alt finst ein `verdi` lik `NA`
#' (uavhengig av kva tilhøyrande `koeffisient` er).
#'
#' Dette er berre ein intern hjelpefunksjon for å forenkla koden
#' for skåring av datasett. Med slike `NA`-rader vert skåren som vert
#' utrekna med [skaar_datasett_uten_validering()] automatisk til `NA` dersom
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
#' Legger variabler fra ett datasett til et annet datasett (med
#' overskriving av variabler som finnes i begge).
#'
#' @param d_orig Originalt datasett (dataramme/tibble).
#' @param d_ekstrakol Dataramme/tibble som inneholder én eller flere
#'     kolonner og samme antall rader som `d_orig`.
#'
#' @details
#' Variabler som finnes i `d_orig` fra før blir erstattet (med advarsel),
#' og nye blir lagt til helt til høyre.
#'
#' @keywords internal
#'
#' @return Originalt datasett med ekstra kolonner lagt til / erstattet.
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
