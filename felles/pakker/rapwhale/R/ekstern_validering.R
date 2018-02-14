# Skript for å hente ut tilfeldige celler fra datasett til validering.

#' Lag valideringsdatasett frå datarammer.
#'
#' Eit sett funksjonar for å henta ut (tilfeldige) data
#' frå eit registerdatasett og lagra desse som SPSS-filer ein kan
#' bruka til ekstern validering av registeret.
#'
#' Kort sagt plukkar \code{lag_valideringsdatasett} ut tilfeldige
#' celler i dataramma. Ein kan velja kor mange celler (variablar)
#' som skal plukkast ut for kvar rad (pasient/forløp).
#'
#' @param df Datasettet ein ønskjer å laga valideringsdatasett for
#'   (ei \code{\link{data.frame}} eller ein \code{\link{tibble}}).
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
#' @param sjukehus_var Tekstvektor med namna på variablane
#'   som unikt identifiserer sjukehuset/avdelingen som rada tilhøyrer,
#'   typisk sjukehusnamn og/eller RESH-ID. Verdiane vert brukte til
#'   å namngje utdatafilene.
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
#' @param utmappe Mappa valideringsfilene skal lagrast i.
#'   fixme: Skil ut som eigen funksjon.
hent_validering_data = function(df, indeks_var, ekstra_var = NULL, sjukehus_var,
                                data_var = NULL, nvar = 5) {

  # Elementær datasjekk -----------------------------------------------------

  # Me sjekkar ikkje alt mogleg, men dei viktigaste tinga,
  # der det er lett å blingsa. For andre ting dukkar det
  # gjerne opp feilmeldingar automatisk (for eksempel viss
  # 'nvar' har meir enn eitt element).

  # Variablane som faktisk finst i datasettet
  df_var = names(df)

  # Viss ein ikkje har valt datavariablar,
  # bruk alle som ikkje er indeksvariablar
  data_vars = setdiff(df_vars, indeks_var)

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

  # Sjekk at indeks_var utgjer ein ekte primærnøkkel
  indeks_var_q = syms(indeks_var)
  if (any(duplicated(select(df, !!!indeks_var_q)))) {
    stop("'indeks_var' identifiserer ikkje radene unikt")
  }

  # Rams opp alle elementa i ein vektor,
  # skilde med komma og med ' rundt seg
  # fixme: flytta denne ut av funksjonen, til ein
  #        global, *ikkje-eksportert* funksjon?
  lag_liste = function(x) {
    paste0(paste0("'", indeks_i_ekstra, "'"), collapse = ", ")
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

  # fixme: Lag testar (test_that()) for alle feilmeldingane våre.
  # fixme: Lag testar for at ting fungerer *riktig*. :)
  # fixme: ha med 'valideringsgruppenummer' i resultatdatasettet.
  # fixme: handtering av blinding (vising/gøyming av 'reg_*'-kolonnane)
  # fixme: gje ut berre maks_eitelleranna pasientar/opphald i utdatarammene.
  # fixme: sjekk at sjukehus_var med lengd > 1 fungerer
  # fixme: reformater kjeldekoden (innrykk og sånt)
  # fixme: del funksjonen opp i éin funksjon for å laga valideringsdatasetta og éin funksjon for å lagra til SPSS-format.



  # quote-sjukehusvariabel
  sjukehusvar = sym(sjukehusvar)

  # henter ut aktuelle kolonner
  d = d %>%
    select(indeks_vars, valid_vars)

  # Plukk ut tilfeldige datakolonnar for kvar rad og lagra
  # namnet på kolonnane i ein eigen variabel.
  #
  # Dette kan gjerast på mange måtar (slå deg laus). Det viktige er
  # berre at me får éi rad for kvar tilfeldig valde celle, rada inneheld
  # alle tilhøyrande indeks- og datavariablar og at namnet på
  # cella vert lagra i kolonnen «varnamn».
  d$varnamn = map(1:nrow(d), ~ sample(data_vars, nvars))
  res = d %>%
    unnest(varnamn)

  # Legg til info om kva kolonne resultatet skal lagrast i
  vartypar = res[data_vars] %>%
    map_chr(class)
  if (!all(vartypar %in% c("integer", "numeric", "Date"))) {
    stop("Finst variabel som ikkje er verken tal eller dato.")
  }
  vartypar = vartypar %>%
    recode(integer = "reg_tal", numeric = "reg_tal", Date = "reg_dato")
  res$res_kol = vartypar[match(res$varnamn, names(res[data_vars]))]

  # Behold en variabel som indikerer hvilken type variabel hver variabel er
  res = res %>%
    mutate(vartype = recode(res_kol, "reg_tal" = "tal", "reg_dato" = "dato"))

  # Legg til aktuelle resultatkolonnar,
  # med rett variabelklasse (tal, dato &c.)
  res = res %>%
    mutate(
      reg_tal = NA_real_, epj_tal = NA_real_,
      reg_dato = NA_real_, epj_dato = NA_real_
    )
  class(res$reg_dato) = "Date"
  class(res$epj_dato) = "Date"


  # Funksjon for å flytta dataverdiane til rett kolonne
  # (er laga for å funka på datarammer med éin unik verdi for res_kol)
  flytt_resultat = function(df) {
    df[[df$res_kol[1]]] = df[[df$varnamn[1]]]
    df
  }

  # For kvar variabel, flytt verdiane til rett kolonne
  res = res %>%
    group_by(varnamn) %>%
    do(flytt_resultat(.))

  # Fjern dei gamle datakolonnane
  res = res[!(names(res) %in% data_vars)] %>%
    select(-res_kol)

  # Resultatene skal ryddes slik at det er sortert etter sykehus, tilfeldig rekkefølge på pasientene,
  # men rader med samme pasientnummer skal komme etter hverandre, og variablene skal
  # stå i en rekkefølge som er lett å bruke.
  res = res %>%
    ungroup() %>%
    mutate(rekkefolge = factor(PasientID, levels = sample(unique(PasientID)))) %>%
    arrange(!!sjukehusvar, rekkefolge, PasientAlder, match(varnamn, data_vars)) %>%
    select(-rekkefolge)
  res

  # Eksporter data til kvalitetsserveren som en SPSS fil (.sav)
  # for hvert sykehus
  dir.create(vdatamappe, showWarnings = FALSE, recursive = TRUE)

  # Del datasettet etter sjukehus
  res_sjukehus = res %>%
    mutate(filadresse = paste0(vdatamappe, !!sjukehusvar, ".sav")) %>%
    nest(-filadresse)

  # Eksporter data for sjukehus til kvar si fil
  pwalk(
    list(
      data = res_sjukehus$data,
      path = res_sjukehus$filadresse
    ),
    write_sav
  )

  # Opna valideringsdatamappa (for å sjekka at alt ser OK ut)
  shell.exec(vdatamappe)
}

df = as.tibble(mice::selfreport)
indeks_var = c("id", "edu")
ekstra_var = c("age", "sex")
data_var = c("hr", "wr", "etn", "br")
sjukehus_var = c("etn", "web")
utmappe = "h:/tmp/greier"


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
