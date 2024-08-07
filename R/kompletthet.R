#' Erstatt ukjent verdi
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Erstatter utvalgte 'ukjente' verdier med NA. Noen datasett har variabler
#' hvor manglende besvarelser får verdi -1, 99 eller andre verdier for å
#' indikere at spørsmålet ikke er besvart. I tillegg kan det også finnes
#' ekte NA-verdier, slik at beregning av kompletthet kompliseres.
#' Funksjonen tar inn en na_vektor som indikerer hvilke verdier som
#' skal erstattes med NA, slik at beregning av NA kan inkludere
#' disse "ukjente" besvarelsene.
#'
#' @param data tibble/data.frame som inneholder variabel hvor
#' ukjente verdier skal erstattes.
#' @param variabel tekststreng med navn på variabel hvor ukjente verdier skal
#' erstattes.
#' @param na_vektor vektor med verdier som skal erstattes med NA.
#'
#' @return
#' Returnerer opprinnelig datasett hvor verdier i 'na_vektor' funnet i
#' 'variabel' er erstattet med NA.
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' d = tibble(
#'   pas_id = c(1, 2, 3, 4, 5, 6),
#'   var_1 = c(1, 2, -1, 99, NA, 5)
#' )
#' erstatt_ukjent(data = d, variabel = "var_1", na_vektor = c(-1, 99))
erstatt_ukjent = function(data, variabel, na_vektor) {
  if (!rlang::has_name(data, variabel)) {
    stop("'", variabel, "' mangler i inndata")
  }

  mutate(data, across(all_of(variabel), ~ replace(., . %in% na_vektor, NA)))
}


#' beregn kompletthet
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Regner ut antall og andel manglende observasjoner for en variabel. Tar inn
#' et datasett og en variabel det skal beregnes kompletthet for.
#'
#' @param data Tibble/data.frame som inneholder variabel det skal beregnes
#' kompletthet for.
#' @param variabel Tekststreng med navn på variabel det skal beregnes kompletthet for.
#'
#' @return
#' Returnerer en tibble med følgende kolonner: \cr
#' grupperingsvariabel - Hvis inndata er gruppert vil grupperingsvariabel være
#' med i utdata. \cr
#' Variabel - Navn på variabel det er beregnet kompletthet for. \cr
#' Totalt_antall - Totalt antall observasjoner per gruppe i inndata  \cr.
#' Antall_na - Antall observasjoner som er NA i inndata. \cr
#' Andel_na - Andel observasjoner som er NA i inndata.
#'
#' @export
#'
#' @examples
#' # Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' # Ugrupperte inndata:
#' d = tibble(
#'   pas_id = c(1, 2, 3, 4, 5, 6),
#'   sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
#'   var_1 = c(1, 2, -1, 99, NA, 5)
#' )
#' beregn_kompletthet(data = d, variabel = "var_1")
#'
#' # Grupperte inndata:
#' d = tibble(
#'   pas_id = c(1, 2, 3, 4, 5, 6),
#'   sykehus = c("HUS", "HUS", "SVG", "SVG", "SVG", "OUS"),
#'   var_1 = c(1, 2, -1, 99, NA, 5)
#' ) |>
#'   group_by(sykehus)
#'
#' beregn_kompletthet(data = d, variabel = "var_1")
beregn_kompletthet = function(data, variabel) {
  if (!rlang::has_name(data, variabel)) {
    stop("'", variabel, "' mangler i inndata")
  }

  data_ut = data |>
    summarise(
      variabel = variabel,
      totalt_antall = n(),
      antall_na = sum(is.na(!!rlang::sym(variabel))),
      andel_na = sum(is.na(!!rlang::sym(variabel))) / n(),
      .groups = "drop"
    ) |>
    arrange(desc(totalt_antall))

  data_ut
}


#' Beregn kompletthet med ukjent
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Beregner antall og andel NA, både med og uten ukjente verdier.
#' Ukjente verdier defineres i na_vektor. Tanken er at besvarelser som
#' "Velg Verdi", "Ukjent", "Ikke besvart" eller lignende kan inkluderes her.
#'
#' @param data Tibble/data.frame som inneholder variabel det skal beregnes
#' kompletthet for.
#' @param variabel Streng med navn på variabel det skal beregnes kompletthet for.
#' @param na_vektor Vektor som angir hvilke verdier i 'variabel' som skal
#' erstattes med NA.
#'
#' @return
#' Returnerer et aggregert datasett med kolonnene: \cr
#' Grupperingsvariabel - hvis inndata er gruppert.\cr
#' Variabel - Navn for variabel det er beregnet kompletthet for.\cr
#' Totalt_antall - Antall rader per gruppe.\cr
#' Antall_na - Antall NA i opprinnelig data.\cr
#' Andel_na - Andel NA i opprinnelig data.\cr
#' Antall_na_med_ukjent - Antall NA når verdier fra NA-vektor er erstattet med NA.\cr
#' Andel_na_med_ukjent - Andel NA når verdier er NA-vektor er erstattet med NA.\cr
#' @export
#'
#' @examples
#' library(tibble)
#'
#' d = tibble(
#'   pas_id = c(1, 2, 3, 4, 5, 6),
#'   sykehus = c("A", "A", "B", "B", "B", "C"),
#'   var_1 = c(1, 2, -1, 99, NA, 5)
#' )
#' beregn_kompletthet_med_ukjent(data = d, variabel = "var_1", na_vektor = c(-1, 99))
beregn_kompletthet_med_ukjent = function(data, variabel, na_vektor) {
  d_na = beregn_kompletthet(data, variabel = variabel)
  data_uten_ukjent = erstatt_ukjent(data, variabel = variabel, na_vektor = na_vektor)
  d_na_ukjent = beregn_kompletthet(data_uten_ukjent, variabel = variabel) |>
    rename(
      antall_na_med_ukjent = antall_na,
      andel_na_med_ukjent = andel_na
    )

  data_ut = d_na |>
    left_join(d_na_ukjent,
      by = c(group_vars(data), "variabel", "totalt_antall"),
      relationship = "one-to-one"
    ) |>
    arrange(desc(totalt_antall))

  data_ut
}


# beregn_kompletthet_datasett ---------------------------------------------

#' Beregn kompletthet for datasett
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tar inn et datasett det skal beregnes kompletthet for og returnerer en
#' tibble med oversikt over kompletthet for samtlige variabler. Tar ikke
#' hensyn til Ukjent/ikke besvart etc.
#'
#' @param data Datasett det skal beregnes kompletthet for.
#'
#' @return
#' Returnerer en tibble med oversikt over antall observasjoner, antall NA og
#' andel NA for hver variabel.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' # Datasett det skal beregnes kompletthet for
#' d_eksempel = tibble(
#'   pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
#'   sykehus = c("HUS", "HUS", "SUS", "SUS", "SUS", "OUS"),
#'   vekt = c(60L, NA_integer_, 100L, NA_integer_, 99L, -1L),
#'   vekt_2 = c(55L, NA_integer_, 99L, -1L, NA_integer_, 50L),
#'   hoyde = c(1.52, NA_real_, 1.89, 2.15, NA_real_, 99.9),
#'   symptom = c("svett", "klam", NA_character_, "trøtt", "vet ikke", "Ukjent"),
#'   test_logisk = c(TRUE, FALSE, NA, NA, FALSE, TRUE)
#' )
#'
#' beregn_kompletthet_datasett(data = d_eksempel)
beregn_kompletthet_datasett = function(data) {
  variabel = names(data)
  d_na = tibble()

  for (i in seq_along(variabel)) {
    d = beregn_kompletthet(data[variabel[i]], variabel[i])


    d_na = bind_rows(d_na, d)
  }

  d_na
}

# Erstatt_ukjent_for_datasett -------------------------------------------------

#' Erstatt ukjent for datasett
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Enkelte variabler har egne verdier for å indikere manglende besvarelse
#' i tillegg til NA-verdier, for eksempel verdien -1 for
#' verditekst "Velg Verdi" eller "Ukjent verdi".
#' Funksjonen erstatter disse verdiene med NA for å beregne andel manglende
#' besvarelser inkludert disse ukjente verdiene.
#' Blir brukt av funksjonen \code{beregn_kompletthet_datasett_med_ukjent()}.
#'
#' @param data Tibble/data.frame som inneholder variablene det skal
#' erstattes ukjente verdier med NA for.
#' @param ukjent_datasett Tibble med variabelnavn og oversikt over hvilke
#' verdier som skal erstattes med NA. Se eksempel for hvilken struktur
#' ukjent_datasett må ha.
#'
#' @return
#' Returnerer opprinnelig datasett men verdier oppgitt i *ukjent_datasett* er
#' erstattet med NA for de aktuelle variablene.
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' d_eksempel = tibble(
#'   pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
#'   sykehus = c("HUS", "HUS", "SUS", "SUS", "SUS", "OUS"),
#'   vekt = c(60L, NA_integer_, 100L, NA_integer_, 99L, -1L),
#'   vekt_2 = c(55L, NA_integer_, 99L, -1L, NA_integer_, 50L),
#'   hoyde = c(1.52, NA_real_, 1.89, 2.15, NA_real_, 99.9),
#'   symptom = c("svett", "klam", NA_character_, "trøtt", "vet ikke", "Ukjent"),
#'   test_logisk = c(TRUE, FALSE, NA, NA, FALSE, TRUE)
#' )
#'
#' ukjent_datasett_eksempel = tibble(
#'   variabel = c(
#'     "pas_id",
#'     "sykehus", rep("vekt", 2),
#'     rep("vekt_2", 2),
#'     "hoyde", rep("symptom", 2),
#'     "test_logisk"
#'   ),
#'   ukjent_verdi_integer = c(
#'     NA_integer_, NA_integer_, 99,
#'     -1, 99, -1, NA_integer_, NA_integer_, NA_integer_, NA_integer_
#'   ),
#'   ukjent_verdi_real = c(
#'     NA_real_, NA_real_, NA_real_, NA_real_,
#'     NA_real_, NA_real_, 99.9, NA_real_, NA_real_, NA_real_
#'   ),
#'   ukjent_verdi_tekst = c(
#'     NA_character_, NA_character_,
#'     NA_character_, NA_character_, NA_character_, NA_character_,
#'     NA_character_, "vet ikke", "Ukjent", NA_character_
#'   )
#' )
#'
#' erstatt_ukjent_for_datasett(
#'   data = d_eksempel,
#'   ukjent_datasett = ukjent_datasett_eksempel
#' )
erstatt_ukjent_for_datasett = function(data, ukjent_datasett) {
  data_uten_ukjent = tibble::tibble_row()

  hjelpefunksjon_na_vektor =
    possibly(~ (filter(.x, variabel == .y) |>
      select(
        where(~ !all(is.na(.x))),
        -variabel
      ) |>
      pull()), otherwise = NULL)

  for (i in seq_len(ncol(data))) {
    variabel_i = names(data)[i]
    na_vektor_i = hjelpefunksjon_na_vektor(
      ukjent_datasett,
      variabel_i
    )

    d = erstatt_ukjent(
      data = select(data, !!variabel_i),
      variabel = variabel_i,
      na_vektor = na_vektor_i
    )

    data_uten_ukjent = bind_cols(data_uten_ukjent, d)
  }
  data_uten_ukjent
}

# beregn_kompletthet_datasett_med_ukjent ----------------------------------

#' Beregn kompletthet med og uten ukjente verdier
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjonen beregner antall og andel missing med og uten ukjente verdier.
#' Først beregnes antall og andel missing NA-verdier direkte.
#' I tillegg erstattes ukjente verdier oppgitt i ukjent_datasett med NA
#' før antall og andel missing beregnes på nytt.
#' Resultat for begge beregninger returneres i utdata.
#'
#' @param data Datasett det skal beregnes kompletthet for.
#' @param ukjent_datasett Datasett med oversikt over hvilke verdier og
#' variabler som skal erstattes med NA. Se eksempel for detaljer om
#' struktur på ukjent_datasett.
#'
#' @return
#' Tibble med antall observasjoner, antall og andel NA, både med og uten
#' erstatning av ukjente verdier for hver variabel i inndata.
#'
#' @export
#'
#' @examples
#' library(tibble)
#'
#' # Datasett det skal beregnes kompletthet for
#' d_eksempel = tibble(
#'   pas_id = c(1L, 2L, 3L, 4L, 5L, 6L),
#'   sykehus = c("HUS", "HUS", "SUS", "SUS", "SUS", "OUS"),
#'   vekt = c(60L, NA_integer_, 100L, NA_integer_, 99L, -1L),
#'   vekt_2 = c(55L, NA_integer_, 99L, -1L, NA_integer_, 50L),
#'   hoyde = c(1.52, NA_real_, 1.89, 2.15, NA_real_, 99.9),
#'   symptom = c("svett", "klam", NA_character_, "trøtt", "vet ikke", "Ukjent"),
#'   test_logisk = c(TRUE, FALSE, NA, NA, FALSE, TRUE)
#' )
#'
#' # Oversikt over hvilke verdier som skal erstattes med NA for hvilke variabler.
#' ukjent_datasett = tibble(
#'   variabel = c(
#'     "pas_id", "sykehus", rep("vekt", 2), rep("vekt_2", 2),
#'     "hoyde", rep("symptom", 2), "test_logisk"
#'   ),
#'   ukjent_verdi_integer = c(
#'     NA_integer_, NA_integer_, 99, -1, 99, -1,
#'     NA_integer_, NA_integer_, NA_integer_, NA_integer_
#'   ),
#'   ukjent_verdi_real = c(
#'     NA_real_, NA_real_, NA_real_, NA_real_,
#'     NA_real_, NA_real_, 99.9, NA_real_, NA_real_, NA_real_
#'   ),
#'   ukjent_verdi_tekst = c(
#'     NA_character_, NA_character_, NA_character_,
#'     NA_character_, NA_character_, NA_character_, NA_character_,
#'     "vet ikke", "Ukjent", NA_character_
#'   )
#' )
#'
#' # Beregner kompletthet
#' beregn_kompletthet_datasett_med_ukjent(
#'   data = d_eksempel,
#'   ukjent_datasett = ukjent_datasett
#' )
beregn_kompletthet_datasett_med_ukjent = function(data, ukjent_datasett) {
  # Beregner kompletthet for datasett ekskludert ukjente
  d_na = beregn_kompletthet_datasett(data = data)

  data_uten_ukjent = erstatt_ukjent_for_datasett(data = data, ukjent_datasett = ukjent_datasett)

  # regner ut kompletthet inkludert ukjente verdier
  d_na_ukjent = beregn_kompletthet_datasett(data_uten_ukjent) |>
    rename(
      antall_na_med_ukjent = antall_na,
      andel_na_med_ukjent = andel_na
    )

  data_ut = d_na |>
    left_join(d_na_ukjent, by = join_by(variabel, totalt_antall)) |>
    arrange(desc(totalt_antall))

  data_ut
}
