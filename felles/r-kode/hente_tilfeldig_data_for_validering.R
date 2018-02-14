# Skript for å hente ut tilfeldige celler fra datasett til validering.

# Nødvendige pakkar
library(tidyverse)
library(haven)
library(rlang)

#' Lag valideringsdatasett frå datarammer.
#'
#' Dette er eit sett funksjonar for å henta ut (tilfeldige) data
#' frå eit registerdatasett og lagra desse som SPSS-filer ein kan
#' bruka til ekstern validering av registeret.
#'
#' Kort sagt plukkar \code{lag_valideringsdatasett} ut tilfeldige
#' celler i dataramma. Ein kan velja kor mange celler (variablar)
#' som skal plukkast ut for kvar rad (pasient/forløp).
#'
#' @param df Datasettet ein ønskjer å laga valideringsdatasett for
#'   (ei \code{\link{data.frame}} eller ein \code{\link{tibble}}).
#' @param indeks_var Ein tekstvektor med namna på indeksvariablane,
#'   dvs. variablane som \emph{unikt} identifiserer ei rad
#'   (typisk pasient-ID og/eller forløps-ID). Desse
#'   vert tekne med i valideringsdatasettet som eigne kolonnar,
#'   for å identifisera rader i originaldatasettet. Dei svarar til
#'   primærnøklar i databasetabellar og kan \emph{ikkje} vera gjenstand
#'   for å verta trekte ut som valideringsvariablar. Rekkjefølgja
#'   i valideringsdatasetta vert lik rekkjefølgja oppgitt her
#'   (som kan vera forskjellig frå rekkjefølgja i \code{df}).
#' @param ekstra_var Ein tekstvektor med namna på ekstra variablar
#'   som òg skal takast med i alle rader i valideringsdatasettet,
#'   men som typisk er avleidde av \code{indeks_var}, og som
#'   potensielt òg \emph{kan} vera gjenstand for å plukkast ut som
#'   valideringsvariablar. Dei er med fordi dei kan gjera det lettare
#'   eller raskare å finna fram til rett pasient/forløp i pasientjournalen.
#'   Typiske eksempel er fødselsdato, kjønn og operasjonsdato.
#'   Rekkjefølgja i valideringsdatasetta vert lik rekkjefølgja oppgitt her
#'   (som kan vera forskjellig frå rekkjefølgja i \code{df}).
#' @param data_var Ein tekstvektor med namna på datavariablane
#'   i registeret som kan plukkast ut som tilfeldige variablar.
#'   Viss denne er \code{NULL} (standard), vert alle variablar i \code{df}
#'   som ikkje er med i \code{indeks_var} brukte.
#'   I valideringsdatasettet vert kvar tilfeldig utplukka celle
#'   til ei rad, og radene vert sorterte i rekkjefølgja som
#'   variabelnamna er oppgitt i \code{data_var}.
#' @param sjukehus_var Ein tekstvektor med namna på variablane
#'   som unikt identifiserer sjukehuset/avdelingen som rada tilhøyrer,
#'   typisk sjukehusnamn og/eller RESH-ID. Verdiane vert brukte til
#'   å namngje utdatafilene.
#' @param nvar Talet på datavariablar som skal plukkast tilfeldig frå
#'   kvar rad i \code{df}. Set denne til lengda av \code{data_var}
#'   dersom du ikkje ønskjer tilfeldig utplukk men heller vil
#'   kontrollera \emph{alle} datavariablane for kvar kjelderad.
#' @param utmappe Mappa valideringsfilene skal lagrast i.
#'   fixme: Skil ut som eigen funksjon.
hent_validering_data = function(df, indeks_var, ekstra_var = NULL, valid_vars, sjukehusvar, vdatamappe, nvar = 5) {

  # fixme: sjekk at indeks_var unikt identifiserer rader
  # fixme: sjekk at indeks_var ikkje er med i data_var eller ekstra_var
  # fixme: ha med 'valideringsgruppenummer' i resultatdatasettet.
  # fixme: sjekk at nvars <= length(data_vars).
  # fixme: sjekk at indeks_var og ekstra_var ikkje har overlapp (ev. handtera dette).
  # fixme: handtering av blinding
  # fixme: gje ut berre maks_eitelleranna pasientar/opphald i utdatarammene.
  # fixme: fungerer sjukehus_var med lengd > 1
  # fixme: sjekk at alle obligatoriske variablar faktisk finst og handter dupliserte verdiar (via unique()).
  # fixme: reformater kjeldekoden (innrykk og sånt)
  # fixme: del funksjonen opp i éin funksjon for å laga valideringsdatasetta og éin funksjon for å lagra til SPSS-format.

  # Sjekk at alle indeksvariablane faktisk finst i datasettet
  stopifnot(all(indeks_vars %in% names(d)))
  data_vars = valid_vars %>%
    setdiff(indeks_vars)

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

# tester funksjonen under

# Oversikt over namn på indeksvariablar og datavariablar
indeks_vars_soreg = c(
  "PasientID", "Fodselsdato", "PasientAlder", "PasientKjonn",
  "OpererendeRESH", "ForlopsID", "OperasjonsID", "OperererendeSykehus"
)
# mappe for valideringsdata
vmappe = paste0(grunnmappe, "..\\valideringsdata\\", Sys.Date(), "\\")

# variabler man ønsker å validere
# husk at nvars må være >= antall valideringsvariabler (kolonner)
soreg_vars = names(d)

# kjør funksjonen
hent_validering_data(d,
  nvars = 10,
  indeks_vars = indeks_vars_soreg,
  valid_vars = soreg_vars,
  sjukehusvar = "OperererendeSykehus",
  vdatamappe = vmappe
)
