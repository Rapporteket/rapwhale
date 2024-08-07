#' Skår spørreskjema basert på skåringstabell
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Regner ut sumskår(er) i et datasett basert på en skåringstabell.
#' Sjekker også at alle verdiene i datasettet er i samsvar med
#' skåringstabellen.
#'
#' @param d Dataramme/tibble som inneholder spørreskjema-variabler +
#'     eventuelt andre variabler. Spørreskjema-variablene må være
#'     numeriske.
#' @param skaaringstabell Dataramme/tibble som sier hvordan
#'     `d` skal skåres. Må ha fire kolonner,
#'     `delskala` (tekst), `variabel` (tekst), `verdi` (numerisk) og
#'     `koeffisient` (numerisk), og det kan bare finnes én rad
#'     per kombinasjon av `delskala`, `variabel` og `verdi`.
#'     Se detaljer nedenfor.
#' @param variabelnavn Navn på variabler i datasettet som ikke er
#'     identiske med de standardiserte navnene i skåringstabellen. Bruk
#'     syntaksen
#'     `c(std_navn_1 = "dd_navn_1", std_navn_2 = "dd_navn_2")`.
#'     Nye navn trenger kun oppgis for spørreskjema-variabler som har
#'     avvikende navn fra skåringstabellen. Hvis `NULL`, blir
#'     det antatt at alle navnene er i samsvar med skåringstabellen.
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
#' verdien av tilhørende variabel i `d`,
#' pluss et eventuelt konstantledd.
#'
#' For å legge til et konstantledd for en sumskår legger man til en rad i
#' `skaaringstabell`.
#' I raden skal `delskala` være sumskåren konstantleddet skal gjelde for,
#' `variabel` og `verdi` begge være `NA`,
#' og `koeffisient` være verdien til konstantleddet.
#'
#' Merk at `verdi` også *kan* være `NA` utenom konstantleddtilfellet.
#' Da blir manglende svar
#' (dvs. `NA-verdier`) for tilhørende variabel og delskala regnet som
#' gyldig svar og har en tilhørende `koeffisient` (som ikke er `NA`).
#' Dette kan være nyttig i tilfeller hvor svaralternativ som «Vet ikke»
#' og manglende svar begge blir kodet som `NA` og skal bidra til
#' sumskåren. Men merk at slik koding er en dårlig idé. Alle gyldige
#' svaralternativ bør ha eksplisitte verdier.
#'
#' Hvis argumentet `godta_manglende` er satt til `FALSE`, vil en få
#' feilmelding dersom det finnes noen `NA`-verdier i datasettet som *ikke*
#' har en `verdi` lik `NA` i skåringstabellen (for alle delskalene
#' som variabelen inngår i).
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
#' library(tibble)
#'
#' d_eks = tribble(
#'   ~pas_id, ~var_a, ~var_b, ~dato,
#'   146, 1, 2, "2020-05-15",
#'   211, NA, 3, "2020-08-17"
#' )
#'
#' skaaringstabell_eks = tribble(
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
skaar_datasett = function(d, skaaringstabell, variabelnavn = NULL,
                          godta_manglende = FALSE) {
  if (!is.null(variabelnavn)) {
    d_std_navn = rename(d, !!variabelnavn)
  } else {
    d_std_navn = d
  }

  skaaringstabell = ungroup(skaaringstabell) # Gruppering kan føra til feil i utrekningane
  sjekk_skaaringstabell(skaaringstabell)

  d_std_navn = d_std_navn |>
    ungroup() |> # Gruppering kan føra til feil i utrekningane nedanfor
    # (men blir likevel bevart i utdataa,
    # jf. legg_til_eller_erstatt_kolonner()-kallet)
    select(unique(na.omit(skaaringstabell$variabel)))
  sjekk_variabelnavn(d_std_navn, variabelnavn = na.omit(skaaringstabell$variabel))
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
#' `r lifecycle::badge("maturing")`
#'
#' Gir feilmelding hvis skåringstabellen ikke er gyldig.
#'
#' @param skaaringstabell Dataramme/tibble (skåringstabell).
#'
#' @details
#' Sjekker at `skaaringstabell` inneholder riktige kolonner og riktige
#' variabeltyper. En gyldig skåringstabell skal ha fire kolonner,
#' `delskala` (tekst), `variabel` (tekst), `verdi` (numerisk) og
#' `koeffisient` (numerisk), og det skal bare finnes én rad
#' per kombinasjon av `delskala`, `variabel` og `verdi`.
#' Kolonnene `delskala` og `koeffisient` kan ikke ha `NA`-verdier.
#' Viss en rad har verdien `NA` i kolonnen `variabel`,
#' *må* den også ha verdien `NA` i kolonnen `verdi`.
#'
#' I tillegg må hver variabel som inngår i en gitt delskala,
#' ha oppføringer for alle ikke-`NA`-verdiene som variabelen
#' kan ta i de *andre* delskalaene den inngår i
#' (det vil si, har minst én ikke-`NA`-verdi i).
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
  d_med_ant_dupl = count(skaaringstabell, delskala, variabel, verdi, name = "n_rader")
  if (any(d_med_ant_dupl$n_rader != 1)) {
    stop("Skåringstabellen kan ikke inneholde dupliserte verdier for en variabel innenfor samme delskala")
  }

  # Kombinasjoner av 'delskala' og 'variabel' som mangler
  # 'verdi'-er som variabelen har for *andre* delskalaer
  skaaringstabell_uten_na = filter(skaaringstabell, !is.na(verdi))
  d_delskala_var = distinct(skaaringstabell_uten_na, delskala, variabel)
  d_var_verdi = distinct(skaaringstabell_uten_na, variabel, verdi)
  d_komb_mangl = d_delskala_var |>
    full_join(d_var_verdi, by = "variabel", relationship = "many-to-many") |>
    anti_join(skaaringstabell, by = c("delskala", "variabel", "verdi"))
  if (nrow(d_komb_mangl) > 0) {
    oversikt_komb_mangl =
      paste0(
        "Finnes variabler som mangler koeffisienter for enkelte verdier\n",
        "(som de har koeffisienter for i andre delskalaer):\n",
        paste0(d_komb_mangl$delskala, ": ",
          d_komb_mangl$variabel, ": ",
          d_komb_mangl$verdi,
          collapse = "\n"
        )
      )
    stop(oversikt_komb_mangl)
  }

  if (any(is.na(skaaringstabell$variabel) & !is.na(skaaringstabell$verdi))) {
    stop("Kan ikke ha verdien NA i 'variabel' uten at 'verdi' også er NA")
  }

  if (anyNA(skaaringstabell$koeffisient)) {
    stop("Koeffisient-kolonnen i skåringstabellen kan ikke inneholde NA-verdier")
  }

  if (anyNA(skaaringstabell$delskala)) {
    stop("Delskala-kolonnen i skåringstabellen kan ikke inneholde NA-verdier")
  }

  invisible()
}

#' Sjekk at oppgitte variabler finnes i datasettet
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Tar inn et datasett og en vektor med variabelnavn. Gir
#' feilmelding hvis datasettet ikke inneholder alle variabelnavnene i
#' vektoren.
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
  var_mangler_tekst = str_flatten_comma(var_mangler)
  if (length(var_mangler) > 0) {
    stop("Mangler kolonner: ", var_mangler_tekst)
  }
}

#' Sjekk at verdier i et datasett er gyldige
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
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

  if (!all(map_lgl(d, is.numeric))) {
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
#' `r lifecycle::badge("stable")`
#'
#' Gir ut en tibble med oversikt over alle ugyldige verdier i datasettet,
#' basert på en oversikt over mulige gyldige verdier.
#'
#' @param d Dataramme/tibble som kun inneholder kolonner med identiske
#'     navn som i `verditabell$variabel`. Alle kolonnene må inneholde
#'     numeriske verdier.
#' @param verditabell Dataramme/tibble med to kolonner,
#'     `variabel` og `verdi`,
#'     som sier hvilke verdier som er gyldige for hvilke
#'     variabler. Kolonnen `variabel` må være av typen tekst og
#'     `verdi` må være numerisk.
#'
#' @details
#' Blir brukt av [sjekk_variabelverdier()], men kan også
#' brukes separat. Resultatene kan oppsummeres til en
#' tekststreng med [oppsummer_ugyldige_verdier()].
#'
#' @note
#' Verdien `NA` blir *ikke* håndtert på noen spesiell måte.
#' Hvis denne skal regnes som gyldig,
#' må den altså eksplisitt listes opp i `verditabell`
#' (for hver `variabel`).
#'
#' @keywords internal
#'
#' @return Tibble med én rad for hver ugyldige verdi i `d`.
#'     Inneholder tre kolonner, `radnr`, `variabel` og `feilverdi`,
#'     som tilsvarer radnummer, kolonnenavn og celleverdi i `d`.
#'     Radene er sortert etter radnummer og så kolonnerekkefølge i `d`.
#'     Hvis `d` ikke inneholder noen ugyldige verdier, vil tibble-en ha
#'     null rader.
#'
#' @seealso [sjekk_variabelverdier()], [oppsummer_ugyldige_verdier()]
#'
#' @examples
#' # To ugyldige verdier
#' d = data.frame(kjonn = c(2, 1, 1, 3), livskvalitet = c(0, 10, NA, 5))
#' verditabell = data.frame(
#'   variabel = c(rep("kjonn", 2), rep("livskvalitet", 3)),
#'   verdi = c(1, 2, 0, 5, 10)
#' )
#' rapwhale:::finn_ugyldige_verdier(d, verditabell)
#'
#' # Berre gyldige verdier
#' rapwhale:::finn_ugyldige_verdier(d[1:2, ], verditabell)
finn_ugyldige_verdier = function(d, verditabell) {
  d |>
    rowid_to_column("radnr") |>
    pivot_longer(-radnr, names_to = "variabel", values_to = "feilverdi") |>
    anti_join(verditabell, by = c("variabel", feilverdi = "verdi")) |>
    arrange(radnr)
}

#' Presenter ugyldige verdier i et datasett på en god måte
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
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
    oppsummert = d_ugyldige |>
      group_by(variabel) |>
      summarise(feil_verdier = str_flatten_comma(feilverdi)) |>
      summarise(feil_variabler_verdier = paste0(variabel, ": ",
        feil_verdier,
        collapse = "\n"
      )) |>
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
#' `r lifecycle::badge("maturing")`
#'
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
#'     rekkefølgen i `skaaringstabell$delskala`, og rekkefølgen på radene
#'     er lik som i `d`. Hvis en variabel i `d` har `NA-verdier` uten
#'     tilhørende koeffisient i skåringstabellen, blir sumskåren(e) for
#'     tilhørende delskala(er) satt lik `NA`.
skaar_datasett_uten_validering = function(d, skaaringstabell) {
  # Gjer om til éi rad per svar, med ein person-ID
  # som seier kva rad svaret opphavleg kom frå
  d_svar = d |>
    rowid_to_column("person_id") |>
    pivot_longer(-person_id, names_to = "variabel", values_to = "verdi")

  # Legg til rader med NA-verdiar i skåringstabellen, slik at
  # datasettrader med manglande verdiar for ein variabel
  # automatisk får sumskår lik NA for sumskårane som brukar variabelen
  skaaringstabell = legg_til_na_i_skaaringstabell(skaaringstabell)

  # For kvart svar, legg til tilhøyrande koeffisientar
  # (kan vera fleire per svar, dersom det finst fleire delskalaar)
  d_med_koeff = left_join(d_svar, skaaringstabell,
    by = c("variabel", "verdi"),
    relationship = "many-to-many"
  )

  # Legg til eventuelle konstantledd
  skaaringstabell_konstantledd = skaaringstabell |>
    filter(is.na(variabel)) |>
    select(delskala, konstantledd = koeffisient)
  d_med_koeff = d_med_koeff |>
    left_join(skaaringstabell_konstantledd,
      by = "delskala",
      relationship = "many-to-one"
    ) |>
    mutate(
      konstantledd = if_else(is.na(konstantledd), 0, konstantledd)
    )

  # Rekn ut sumskår for alle delskalaane, per person
  d_med_skaarar = d_med_koeff |>
    group_by(person_id, delskala, konstantledd) |>
    summarise(skaar = sum(koeffisient), .groups = "drop") |>
    mutate(skaar = skaar + konstantledd) |>
    select(-konstantledd) |>
    pivot_wider(names_from = "delskala", values_from = "skaar")

  # For sikkerheits skuld, sorter etter opphavleg radnummer,
  # og fjern så radnummeret
  d_med_skaarar = d_med_skaarar |>
    arrange(person_id) |>
    select(-person_id)

  # Inndata med 0 rader må handterast spesielt
  if (nrow(d) == 0) {
    # Brukar pivot_wider() også her, for å sikra at rekkjefølgja
    # på sumskårkolonnane vert lik som for datasett *med* svar
    # (jf. pivot_wider() si handtering av faktorvariablar)
    d_med_skaarar = skaaringstabell |>
      distinct(delskala, .keep_all = TRUE) |>
      select(delskala, koeffisient) |> # Treng «koeffisient» for å få riktig variabeltype
      pivot_wider(names_from = "delskala", values_from = koeffisient) |>
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
#' `r lifecycle::badge("maturing")`
#'
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
    if (!anyNA(df$verdi)) {
      df = add_row(df, verdi = NA, koeffisient = NA)
    }
    df
  }
  nest(skaaringstabell, data = c(verdi, koeffisient)) |>
    mutate(data = map(data, legg_til_na_rad)) |>
    unnest(cols = c(data))
}

#' Legg til / erstatt variabler i ett datasett med variabler i et annet
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
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
  navn_finst_tekst = str_flatten_comma(navn_finst)

  if (length(navn_finst) > 0) {
    warning(
      "Følgende kolonne(r) i datasettet har blitt overskrevet: ",
      navn_finst_tekst
    )
  }

  d_orig[names(d_ekstrakol)] = d_ekstrakol
  d_orig
}
