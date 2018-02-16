# Skript for å hente ut tilfeldige celler fra datasett til validering.

#' Lag valideringsdatasett frå datarammer
#'
#' @description
#' Eit sett funksjonar for å henta ut (tilfeldige) data
#' frå eit registerdatasett og lagra desse som SPSS-filer ein kan
#' bruka til ekstern validering av registeret.
#'
#' Kort sagt plukkar funksjonen ut tilfeldige
#' celler i dataramma. Ein kan velja kor mange celler (variablar)
#' som skal plukkast ut for kvar rad (pasient/forløp).
#'
#' @return
#' Ei liste med valideringsdatasett, eitt for kvart sjukehus.
#' Elementa i lista har namn etter sjukehus, laga ved
#' å fletta saman variablane referert til i \code{sjukehus_var}
#' med \code{_}-teikn. Desse namna kan òg brukast som
#' filnamn ved lagring av valideringsdatasetta til disk.
#'
#' @param df Datasettet ein ønskjer å laga valideringsdatasett for
#'   (ei \code{\link[base]{data.frame}} eller ein \code{\link[tibble]{tibble}}).
#' @param sjukehus_var Tekstvektor med namna på variablane
#'   som unikt identifiserer sjukehuset/avdelingen som rada tilhøyrer,
#'   typisk sjukehusnamn og/eller RESH-ID. Verdiane vert brukte til
#'   å gruppera valideringsdatasettet i fleire grupper.
#' @param indeks_var Tekstvektor med namna på indeksvariablane,
#'   dvs. variablane som \emph{unikt} identifiserer ei rad
#'   (typisk pasient-ID og/eller forløps-ID). Desse
#'   vert tekne med i valideringsdatasettet som eigne kolonnar,
#'   for å identifisera rader i originaldatasettet. Dei svarar til
#'   primærnøklar i databasetabellar og kan \emph{ikkje} vera gjenstand
#'   for å verta trekte ut som valideringsvariablar. Rekkjefølgja
#'   i valideringsdatasetta vert lik rekkjefølgja oppgitt her
#'   (som kan vera forskjellig frå rekkjefølgja i \code{df}).
#' @param ekstra_var Tekstvektor med namna på ekstra variablar
#'   som òg skal takast med i alle rader i valideringsdatasettet,
#'   men som typisk er avleidde av \code{indeks_var}, og som
#'   potensielt òg \emph{kan} vera gjenstand for å plukkast ut som
#'   valideringsvariablar. Dei er med fordi dei kan gjera det lettare
#'   eller raskare å finna fram til rett pasient/forløp i pasientjournalen.
#'   Typiske eksempel er fødselsdato, kjønn og operasjonsdato.
#'   Rekkjefølgja i valideringsdatasetta vert lik rekkjefølgja oppgitt her
#'   (som kan vera forskjellig frå rekkjefølgja i \code{df}).
#' @param data_var Tekstvektor med namna på datavariablane
#'   i registeret som kan plukkast ut som tilfeldige variablar.
#'   Viss denne er \code{NULL} (standard), vert alle variablar i \code{df}
#'   som ikkje er med i \code{indeks_var} brukte.
#'   I valideringsdatasettet vert kvar tilfeldig utplukka celle
#'   til ei rad, og radene vert sorterte i rekkjefølgja som
#'   variabelnamna er oppgitt i \code{data_var}.
#' @param nvar Talet på datavariablar som skal plukkast tilfeldig frå
#'   kvar rad i \code{df}. Set denne til lengda av \code{data_var}
#'   dersom du ikkje ønskjer tilfeldig utplukk men heller vil
#'   kontrollera \emph{alle} datavariablane for kvar kjelderad.
lag_valideringsdata = function(df, sjukehus_var, indeks_var, ekstra_var = NULL,
                               data_var = NULL, nvar = 5) {

  # Elementær datasjekk -----------------------------------------------------

  # Me sjekkar ikkje alt mogleg, men dei viktigaste tinga,
  # der det er lett å blingsa. For andre ting dukkar det
  # gjerne opp feilmeldingar automatisk (for eksempel viss
  # 'nvar' har meir enn eitt element).

  # Avgrupper dataramma, for å unngå potensielt mange problem ...
  df = dplyr::ungroup(df)

  # Variablane som faktisk finst i datasettet
  df_var = names(df)

  # Viss ein ikkje har valt datavariablar,
  # bruk alle som ikkje er indeksvariablar
  data_vars = setdiff(df_var, indeks_var)

  # Sjekk at alle variabelsetta faktisk finst i datasettet
  stopifnot(
    all(indeks_var %in% df_var),
    all(ekstra_var %in% df_var),
    all(sjukehus_var %in% df_var),
    all(data_var %in% df_var)
  )

  # Sjekk at ingen av variabellistene har duplikatverdiar
  # (kunne føra til problem seinare, ikkje minst for testane)
  stopifnot(
    !anyDuplicated(indeks_var),
    !anyDuplicated(ekstra_var),
    !anyDuplicated(sjukehus_var),
    !anyDuplicated(data_var)
  )

  # Quoting av dei ulike variablane våre, for seinare bruk
  indeks_var_q = syms(indeks_var)
  ekstra_var_q = syms(ekstra_var)
  sjukehus_var_q = syms(sjukehus_var)
  data_var_q = syms(data_var)

  # Sjekk at indeks_var utgjer ein ekte primærnøkkel
  if (any(duplicated(dplyr::select(df, !!!indeks_var_q)))) {
    stop("'indeks_var' identifiserer ikkje radene unikt")
  }

  # Rams opp alle elementa i ein vektor,
  # skilde med komma og med ' rundt seg
  # fixme: flytta denne ut av funksjonen, til ein
  #        global, *ikkje-eksportert* funksjon?
  lag_liste = function(x) {
    paste0(paste0("'", x, "'"), collapse = ", ")
  }

  # Sjekk at indeksvariablar ikkje blir brukt andre plassar
  indeks_i_ekstra = intersect(indeks_var, ekstra_var)
  if (length(indeks_i_ekstra) > 0) {
    stop(
      "'indeks_var' har variablar som òg finst i 'ekstra_var':\n  ",
      lag_liste(indeks_i_ekstra)
    )
  }
  indeks_i_data = intersect(indeks_var, data_var)
  if (length(indeks_i_data) > 0) {
    stop(
      "'indeks_var' har variablar som òg finst i 'data_var':\n  ",
      lag_liste(indeks_i_data)
    )
  }

  if (nvar > length(data_vars)) {
    stop(
      "Kan ikkje plukka ut fleire tilfeldige variablar (", nvar, ") ",
      "enn det finst datavariablar (", length(data_vars), ")."
    )
  }

  # fixme: Sjekk at alle oppgitt variabellister har lenged >= 1 (men berre om det er nødvendig!)
  # fixme: Lag testar (test_that()) for alle feilmeldingane våre.
  # fixme: Lag testar for at ting fungerer *riktig*. :)
  # fixme: ha med 'valideringsgruppenummer' i resultatdatasettet.
  # fixme: handtering av blinding (vising/gøyming av 'reg_*'-kolonnane)
  # fixme: gje ut berre maks_eitelleranna pasientar/opphald i utdatarammene.
  # fixme: sjekk at det funkar å ha same variablar i både ekstra_vars og data_vars
  # fixme: del funksjonen opp i éin funksjon for å laga valideringsdatasetta og éin funksjon for å lagra til SPSS-format.
  # fixme: sjekk at det ikkje finst duplikatnamn i dataramma (er det mogleg?)


  # Uthenting av valideringsdata --------------------------------------------

  # Hentar berre ut aktuelle kolonnar, og i typisk rekkjefølgje,
  # slik at datasettet vårt vert meir oversiktleg
  # (og kanskje raskare å arbeida med?)
  alle_var_q = unique(c(indeks_var_q, ekstra_var_q, sjukehus_var_q, data_var_q))
  df = df %>%
    dplyr::select(!!!alle_var_q)

  # Me skal maks ha 'nvar' rader for kvart sjukehus, og det er
  # greiast å gjera det alt no. Merk at nokre sjukehus kan
  # ha mindre enn 'nvar' rader å plukka. Her er ein (litt
  # uelegant) måte å løysa dette på.
  df = df %>%
    group_by(!!!sjukehus_var_q) %>%
    mutate(n_rader = n()) %>%
    do(sample_n(., min(nvar, .$n_rader[1]))) %>%
    ungroup()

  # Plukk ut tilfeldige datakolonnar for kvar rad og lagra
  # namnet på kolonnane i ein eigen variabel.
  #
  # Dette kan gjerast på mange måtar (slå deg laus). Det viktige er
  # berre at me får éi rad for kvar tilfeldig valde celle, rada inneheld
  # alle tilhøyrande indeks-, ekstra- og datavariablar og at namnet på
  # cella vert lagra i kolonnen «varnamn».
  # fixme: Korleis handtera det dersom 'varnamn' eller 'res_kol', 'reg_tal' osv. finst som kolonnar frå før?
  #        Køyra ein kontroll på dette tidlegare oppe?
  df$varnamn = map(1:nrow(df), ~ sample(data_var, nvar))
  res = df %>%
    unnest(varnamn)


  # Sjekk dataformat og lag klar utvariablar --------------------------------

  # Oversikt over kva variabeltypar me handterer og
  # kva norske variabelnamn dei skal få
  # For å gjera ting lettare for brukaren, slår me
  # òg saman nokre variabeltypar, for eksempel
  # heiltal og desimaltal (fixme: vurder å endra dette?)
  d_moglege_vartypar = tribble(
    ~var_rklasse, ~vartype, ~var_rklasse_ut,
    "integer", "tal", "numeric",
    "numeric", "tal", "numeric",
    "Date", "dato", "Date"
  )

  # Lag oversikt over kva variabeltypar me faktisk har blant datavariablane våre
  res_data = res %>%
    dplyr::select(!!!data_var_q)
  d_vartypar = tibble(
    varnamn = names(res_data),
    var_rklasse = map(res_data, class) %>%
      map_chr(first) # Nokre variablar kan fleire klassar, og me brukar då første
  )

  # Finn eventuelle variabeltypar (i datavariablane) som me *ikkje* støttar
  d_vartypar_ikkjeok = d_vartypar %>%
    anti_join(d_moglege_vartypar, by = "var_rklasse")
  if (nrow(d_vartypar_ikkjeok) > 0) {
    stop(
      "Finst variablar som verken er tal eller dato (førebels ikkje støtta):\n",
      lag_liste(d_vartypar_ikkjeok$varnamn)
    )
  }

  # Legg til info om kva norske namn kvar variabeltype skal få
  d_vartypar = d_vartypar %>%
    left_join(d_moglege_vartypar, by = "var_rklasse")

  # For kvar uttrekte variabel, legg til info om kva kolonne
  # resultatet skal lagrast i
  res = res %>%
    left_join(d_vartypar, by = "varnamn") %>%
    mutate(res_kol = paste0("reg_", vartype))

  # Legg til aktuelle resultatkolonnar, med rett variabelklasse (tal, dato &c.)
  # Merk at me berre gjer dette for dei variabeltypane som faktisk finst i datasettet
  d_vartypar_rformat = res %>%
    dplyr::select(vartype, var_rklasse_ut) %>%
    distinct()
  for (i in 1:nrow(d_vartypar_rformat)) {
    vartype = d_vartypar_rformat %>%
      pluck("vartype", i)
    rklasse = d_vartypar_rformat %>%
      pluck("var_rklasse_ut", i)
    prefiksverdiar = c("reg_", "epj_")
    for (prefiks in prefiksverdiar) {
      varnamn = paste0(prefiks, vartype)
      res[[varnamn]] = NA
      class(res[[varnamn]]) = rklasse
    }
  }


  # Restrukturering av valideringsdataa --------------------------------------

  # Funksjon for å flytta dataverdiane til rett kolonne
  # (er laga for å køyrast på datarammer med *éin unik* verdi for res_kol)
  #
  # (Det går raskast å flytta alle verdiane for kvar variabel i éin jafs.
  # Derfor gjer me det slik.)
  flytt_resultat = function(df) {
    df[[df$res_kol[1]]] = df[[df$varnamn[1]]]
    df
  }

  # For kvar variabel, flytt verdiane til rett kolonne
  res = res %>%
    group_by(varnamn) %>%
    do(flytt_resultat(.)) %>% # Ev. bruka purr-funksjonar til dette?
    ungroup()

  # Fjern dei gamle datakolonnane og andre hjelpekolonnar.
  # Men me kan ikkje fjerna datakolonnane direkte, sidan
  # nokre av dei òg kan vera ekstrakolonnar. Me hentar
  # derfor heller ut indekskolonnane, ekstrakolonnane,
  # sjukehuskolonnane og dei innregistreringskolonnane som
  # me treng
  innreg_kol = unique(res$res_kol)
  innreg_kol = c(rbind(innreg_kol, str_replace(innreg_kol, "^reg_", "epj_"))) # Stygt triks for å fin rekkefølgje ...
  innreg_kol_q = syms(innreg_kol)
  res = res %>%
    dplyr::select(!!!sjukehus_var_q, !!!indeks_var_q, !!!ekstra_var_q, varnamn, !!!innreg_kol_q)


  # Fornuftig sortering og gruppering ---------------------------------------

  # Resultata skal sorterast etter sjukehus, med tilfeldig rekkjefølgje
  # på pasientane innan sjukehus, men rader med samme pasientnummer (e.l.) skal
  # komme etter hverandre, og desse radene skal stå i same rekkefølgje
  # som data_vars-variablane vart oppgitt i, slik at dei vert lett å bruka.
  res = res %>%
    mutate(
      sjukehus_indeks = make.names(str_c(!!!sjukehus_var_q, sep = "_")),
      rekkefolge = apply(res[indeks_var], 1, . %>% paste0(collapse = "___")),
      rekkefolge = factor(rekkefolge, levels = sample(unique(rekkefolge)))
    ) %>%
    arrange(sjukehus_indeks, rekkefolge, match(varnamn, !!data_var_q)) %>%
    dplyr::select(-rekkefolge)

  # Gjer om til ei liste med eitt element per sjukehus
  # og med namn lik sjukehusindeksen (skal/kan brukast som filnamn)
  splitvar = res$sjukehus_indeks
  res$sjukehus_indeks = NULL
  res_l = res %>%
    split(splitvar)
  res_l

  # Returner resultatet
  res_l
}


# Eksporter data til kvalitetsserveren som en SPSS fil (.sav)
# for hvert sykehus
d = tibble::tribble(
  ~sjukehus, ~avdeling, ~pas_id, ~opphald_id, ~kjonn, ~alder, ~reg_dato, ~diag_kode, ~hogd, ~vekt,
  "Haukeland", "Post A", 101, 34, 1, 23, as.Date("2011-12-18"), 3, 181, 82,
  "Haukeland", "Post B", 101, 52, 1, 23, as.Date("2012-01-03"), 4, 181, 88,
  "Haukeland", "Post A", 103, 76, 2, 47, as.Date("2017-02-28"), 3, 160, 56,
  "St. Olavs", "Lungeavd.", 201, 88, 1, 52, as.Date("2014-03-10"), 11, 200, 104,
  "St. Olavs", "Lungeavd.", 202, 104, 2, 33, as.Date("2014-03-11"), 8, 170, 80,
  "St. Olavs", "Lungeavd.", 203, 105, 2, 45, as.Date("2016-03-11"), 3, 100, 150,
  "Narvik", "Post A", 404, 2, 2, 78, as.Date("2018-02-04"), 7, 179, 61
)
d_valid = lag_valideringsdata(d,
  sjukehus_var = c("sjukehus"),
  indeks_var = c("pas_id", "opphald_id"),
  ekstra_var = c("alder", "kjonn"),
  data_var = c("reg_dato", "diag_kode", "hogd", "vekt"),
  nvar = 2
)


eksporter_valideringsdata = function(df, utmappe) {
  # Opprett mappe for utfilene
  dir.create(utmappe, showWarnings = FALSE, recursive = TRUE)

  # Eksporter data for sjukehus til kvar si fil
  filadresser = paste0(utmappe, "/", names(df), ".sav")
  pwalk(
    list(
      data = df,
      path = filadresser
    ),
    write_sav
  )

  # Opna valideringsdatamappa (for å sjekka at alt ser OK ut)
  shell.exec(utmappe)
}

eksporter_valideringsdata(d_valid, "h:/tmp/rtest/")

# tester funksjonen under
#
# # Oversikt over namn på indeksvariablar og datavariablar
# indeks_vars_soreg=c("PasientID", "Fodselsdato", "PasientAlder", "PasientKjonn",
#            "OpererendeRESH", "ForlopsID", "OperasjonsID", "OperererendeSykehus")
# # mappe for valideringsdata
# vmappe = paste0(grunnmappe, "..\\valideringsdata\\", Sys.Date(), "\\")
#
# # variabler man ønsker å validere
# # husk at nvars må være >= antall valideringsvariabler (kolonner)
# soreg_vars = names(d)
#
# # kjør funksjonen
# hent_validering_data(d,
#                      nvars = 10,
#                      indeks_vars = indeks_vars_soreg,
#                      valid_vars = soreg_vars,
#                      sjukehusvar = "OperererendeSykehus",
#                      vdatamappe = vmappe)
