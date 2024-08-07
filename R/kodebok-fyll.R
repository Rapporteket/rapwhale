# Funksjon for å hekta variabeltekstar på kategoriske variablar basert på kodebok
#
# Sjå fila kodebok-fyll-kravspek.pdf for detaljert kravspek for implementasjon

#' Hent variabeltekst for kategoriske variabler
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Funksjon for å hente variabeltekstar på kategoriske variablar
#' basert på kodebok.
#' Genererer nye variabler med oppgitt suffiks
#' og returnerer datasettet med de nye variablene inkludert.
#' For en variabel som er kodet med en vilkårlig verdi vil denne funksjonen
#' hente direkte fra kodebok hvilken gruppe de forskjellige
#' verdiene representerer.
#'
#' @param df Datasett som inkluderer de kategoriske variablene.
#' @param kb Kodebok for registeret.
#' @param ... Ytterligere argumenter.
#' @param .suffiks
#' Suffiks for variabelnavn som kobles på det opprinnelige variabelnavnet.
#' Default er "_tekst".
#' @export
#' @examples
#' # Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' # eksempel kodebok
#' kb = tribble(
#'   ~variabel_id, ~verdi, ~verditekst,
#'   "kjonn", 1, "mann",
#'   "kjonn", 2, "kvinne",
#'   "med", 1, "antibac",
#'   "med", 2, "insulin",
#'   "med", 3, "ibux",
#'   "med", 4, "globoid",
#'   "gensp", 1, "Misfornøyd",
#'   "gensp", 2, "Både og",
#'   "gensp", 3, "Fornøyd"
#' )
#'
#' # Eksempeldatasett
#' d = tribble(
#'   ~pasid, ~kjonn, ~alder, ~med, ~prem,
#'   101, 2, 18, 3, 2,
#'   102, 1, 37, 4, 2,
#'   103, 1, 17, 1, 3
#' )
#'
#' # Fyll in verditekst for alle variabler i kodebok
#' kb_fyll(d, kb)
#'
#' # Hvis det kun skal hentes verditekst for en gitt variabel
#' kb_fyll(d, kb, kjonn)
#'
#' # Variabler kan kobles mot variabel_id med annet navn i kodebok
#' kb_fyll(d, kb, kjonn, prem = "gensp")
#'
#' # Suffiks kan endres fra default "_tekst"
#' kb_fyll(d, kb, med, .suffiks = "_hei")
kb_fyll = function(df, kb, ..., .suffiks = "_tekst") {
  # Stopp viss kodeboka ikkje inneheld dei tre nødvendige kolonnane
  if (!all(c("variabel_id", "verdi", "verditekst") %in% names(kb))) {
    stop("Ugyldig kodebok. Obligatoriske kolonnar er 'variabel_id', 'verdi' og 'verditekst'.")
  }

  # Sjå vidare berre på kategoriske variablar (dersom kodeboka
  # har informasjon om kva som er kategoriske variablar)
  if (rlang::has_name(kb, "variabeltype")) {
    kb = filter(kb, variabeltype == "kategorisk")
  }

  # Stopp viss nokre av dei tre nødvendige kolonnane har ugyldige verdiar
  # eller viss dei er faktorar
  for (kol in c("variabel_id", "verdi", "verditekst")) {
    if (anyNA(kb[[kol]])) {
      stop("Ugyldig kodebok. Kolonnen '", kol, "' har NA-verdi(ar).")
    }
    if (any(is.factor(kb[[kol]]))) {
      stop("Ugyldig kodebok. Kolonnen '", kol, "' er faktor.")
    }
  }

  # Namn på variablar som skal fyllast ut
  arg = rlang::quos(...)
  vnamn_d = rlang::quos_auto_name(arg) |>
    names() # Namn i datasettet
  # Viss ein ikkje har valt variablar, bruk alle som finst i kodeboka
  if (length(vnamn_d) == 0) {
    vnamn_d = intersect(names(df), kb$variabel_id)
    vnamn_kb = vnamn_d # Tilsvarande namn i kodeboka
    if (length(vnamn_d) == 0) {
      warning("Kodeboka inneheld ingen variablar som finst i datasettet.")
    }
  } else {
    vnamn_kb = map_chr(arg, rlang::quo_name) # Tilsvarande namn i kodeboka
  }

  # Feilmeldingar eller åtvaringar dersom datasettet og/eller
  # kodeboka og/eller funksjonskallet inneheld feil

  # Viss ein ber om variablar som ikkje finst i kodeboka
  lag_liste = function(x) {
    str_c("'", x, "'", collapse = ", ")
  } # Kjed saman tekststrengar
  berre_kb = setdiff(vnamn_kb, kb$variabel_id)
  n_feil = length(berre_kb)
  if (n_feil > 0) {
    stop(str_c(
      ifelse(n_feil > 1, "Variablar", "Variabel"),
      " finst ikkje i kodeboka: ", lag_liste(berre_kb)
    ))
  }

  # Viss ein ber om variablar som ikkje finst i datasettet
  berre_d = setdiff(vnamn_d, names(df))
  n_feil = length(berre_d)
  if (n_feil > 0) {
    stop(str_c(
      ifelse(n_feil > 1, "Variablar", "Variabel"),
      " finst ikkje i datasettet: ", lag_liste(berre_d)
    ))
  }


  # Gå gjennom kvar variabel og legg til verditekstar
  for (i in seq_along(vnamn_d)) {
    # Namn på variabelen (i datasettet)
    vnamn = vnamn_d[i]

    # Delen av kodeboka som gjeld den aktuelle variabelen
    koder = filter(kb, variabel_id %in% vnamn_kb[i])

    # Det nye namnet på variabelen
    nytt_namn = str_c(vnamn, .suffiks)

    # Åtvaring hvis variablene med tallverdier blir overskrevet av
    # variabler med tekstverdier
    if (nytt_namn %in% names(df)) {
      warning("Overskriv variabel: '", nytt_namn, "'")
    }

    # Åtvaring og NA-verdi viss datasettet inneheld verdiar
    # som aktuell variabel ikkje har i kodeboka
    # (fixme: Vurderingssak: Bør dette heller gje feilmelding enn åtvaring?)
    manglar_i_kb = na.omit(setdiff(df[[vnamn]], koder$verdi))
    if (length(manglar_i_kb) > 0) {
      warning(str_c(
        "Variabelen ", lag_liste(vnamn), " har ",
        ifelse(length(manglar_i_kb) == 1, "ugyldig verdi", "ugyldige verdiar"),
        " (vart gjort om til NA): ", lag_liste(sort(manglar_i_kb))
      ))
    }

    # Stopp viss verdi- eller verditekst-variablane har dupliserte verdiar
    for (kol in c("verdi", "verditekst")) {
      if (anyDuplicated(koder[[kol]]) > 0) {
        stop("Ugyldig kodebok. Variabelen '", vnamn, "' har dupliserte verdiar i kolonnen '", kol, "'.")
      }
    }

    # Hent verditekster fra kodebok og legg til i datasettet
    df[[nytt_namn]] = factor(df[[vnamn]],
      levels = koder$verdi,
      labels = koder$verditekst
    )

    # Plasser den nye variabelen på rett sted i datasettet
    ind_opp = which(names(df) == vnamn)
    ind_nye = which(names(df) == nytt_namn)
    ind_venstre = unique(c(1:ind_opp, ind_nye))
    df = df[c(ind_venstre, setdiff(seq_along(df), ind_venstre))]
  }

  # Returner oppdatert datasett
  df
}
