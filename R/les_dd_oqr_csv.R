#' Les inn variabelnavn fra datafil
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{les_varnavn()} henter ut første rad fra en datafil. Bruker filplassering som argument.
#'
#' @details
#' Returnerer en vektor med variabelnavn slik de er i datafilen.
#'
#' @param adresse
#' Filplassering for datafil som skal leses inn. Henter kun ut variabelnavn.
#' @param formatspek
#' Liste med formatspesifikasjon.
#' @export
les_varnavn = function(adresse, formatspek) {
  stopifnot(is.character(adresse) & length(adresse) == 1)

  varnavn = read_delim(adresse,
    delim = formatspek$skilletegn,
    n_max = 1,
    col_names = FALSE,
    na = formatspek$na_verdier,
    locale = locale(encoding = formatspek$tegnkoding),
    show_col_types = FALSE
  ) |>
    unlist(use.names = FALSE) |>
    replace_na("")

  varnavn
}

#' Konverter en variabel til logisk variabel
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Konverterer en vektor med verdier til en vektor som kun inneholder
#' TRUE, FALSE eller NA
#'
#' @param x Vektor med verdier som skal konverteres til en logisk vektor.
#' @param boolsk_usann Hvilke verdier skal konverteres til FALSE? Kan inneholde flere verdier.
#' @param boolsk_sann Hvilke verdier skal konverteres til TRUE? Kan inneholde flere verdier.
#' @param na_verdier Hvilke verdier skal betraktes som NA? Kan inneholde flere verdier.
#'
#' @details
#' Tar inn en vektor som skal konverteres, i tillegg til vektorer som
#' beskriver hva som skal regnes som TRUE, FALSE og NA.
#'
#' @return
#'   En vektor som kun inneholder TRUE, FALSE eller NA.
#'
#' @export
#'
#' @examples
#' tallvektor = c(-1, 0, 1)
#' konverter_boolske(tallvektor,
#'   boolsk_usann = 0, boolsk_sann = 1,
#'   na_verdier = -1
#' )
#'
#' tekstvektor = c("Ukjent", "Nei", "Ja")
#' konverter_boolske(tekstvektor,
#'   boolsk_usann = "Nei",
#'   boolsk_sann = "Ja", na_verdier = "Ukjent"
#' )
#'
#' tekstvektor_flere_alt = c(NA, "Nei", "Ja", "-1", "0", "false")
#' konverter_boolske(tekstvektor_flere_alt,
#'   boolsk_usann = c("Nei", "0", "false"),
#'   boolsk_sann = c("Ja"), na_verdier = c(NA, "-1")
#' )
konverter_boolske = function(x, boolsk_usann, boolsk_sann, na_verdier = NA) {
  # Sjekk først at det berre er gyldige verdiar
  mulige_verdier = c(boolsk_usann, boolsk_sann, na_verdier)
  er_gyldig = (x %in% mulige_verdier)
  er_ugyldig = x[!er_gyldig]
  overlapp = intersect(boolsk_usann, boolsk_sann)

  if (!rlang::is_empty(overlapp)) {
    stop("boolsk_sann og boolsk_usann kan ikke inneholde samme verdier")
  }
  if (!all(er_gyldig)) {
    ugyldige = paste0(er_ugyldig, collapse = ",")
    mulige = paste0(mulige_verdier, collapse = ",")
    stop(
      "Det finnes ugyldige verdier for en boolsk variabel: ", ugyldige,
      "\nMulige verdier er: ", mulige
    )
  }

  # Konverterer til logisk vektor
  x_boolsk = rep(NA, length(x))
  x_boolsk[x %in% boolsk_usann] = FALSE
  x_boolsk[x %in% boolsk_sann] = TRUE
  x_boolsk
}

#' Konverter variabeltype fra standard format til readrformat
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{std_koltype_til_readr_koltype()} tar inn en vektor med variabeltyper oppgitt på vårt standardformat.
#' Returnerer en sammenslått tekststreng med gyldige coltypes for bruk i \code{read_*()}-funksjoner fra readr-pakken.
#' Se \code{read_delim()}
#'
#' @param vartype Vektor med variabeltyper oppgitt på standardformat.
#' Mulige typer er: tekst, desimalltall, heltall, boolsk, dato, dato_kl og kl.
#'
#' @keywords internal
std_koltype_til_readr_koltype = function(vartype) {
  if (length(vartype) == 0) {
    return("")
  }
  if (anyNA(vartype)) {
    stop("Variabeltype må defineres for alle variabler")
  }

  koblingstabell = tribble(
    ~vartype, ~koltype,
    "tekst", "c",
    "desimaltall", "d",
    "heltall", "i",
    "boolsk", "c",
    "dato", "D",
    "dato_kl", "c",
    "kl", "t"
  )

  ind = match(vartype, koblingstabell$vartype)

  if (anyNA(ind)) {
    vartype_mangler = paste0("'", vartype[is.na(ind)], "'", collapse = ", ")
    stop("Ukjent variabeltype: ", vartype_mangler)
  }

  koltype = str_c(koblingstabell$koltype[ind], collapse = "")
  koltype
}

# Setter inn NA for 'na_verdi':
erstatt_med_na = function(x, na_verdi) {
  x[x %in% na_verdi] = NA
  x
}

#' Les inn csv-fil
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{les_csv_base()} leser inn en csv-fil fra en filplassering.
#' Spesifisering av format og datastruktur gjøres i argumentene formatspek og spesifikasjon.
#' Se rapwhale::formatspek og rapwhale::spesifikasjon for eksempeloppsett for
#' ulike innstillinger.
#' Variabelnavn kan endres ved å inkludere en vektor med nye variabelnavn.
#' Aksepterte variabeltyper er: tekst, desimaltall, heltall, boolsk, dato, dato_kl og klokkeslett.
#'
#' @param adresse
#' Filplassering for datadump som skal leses inn.
#' @param spesifikasjon
#' Tibble med tre kolonner.
#' Inneholder varnavn_kilde, varnavn_resultat og vartype.
#' @param formatspek
#' Liste som inneholder informasjon om hvordan csv-fil er spesifisert.
#' Gjelder desimaltegn, datoformat, klokkeslettformat,
#' tidssone, boolsk_sann, boolsk_usann.
#'
#' @keywords internal
les_csv_base = function(adresse, spesifikasjon, formatspek) {
  # Lager en kolonnenavn streng basert på innmat. Sorterer variabelrekkefølge etter datadump
  kolnavn_fil = les_varnavn(adresse, formatspek)
  kolnavn_spek = spesifikasjon$varnavn_kilde
  testthat::expect_identical(sort(kolnavn_fil), sort(kolnavn_spek))
  radnr = match(kolnavn_fil, kolnavn_spek)
  stopifnot(!anyNA(radnr))
  spesifikasjon = spesifikasjon[radnr, ]

  d = read_delim(adresse,
    delim = formatspek$skilletegn, skip = 1,
    na = formatspek$na_verdier,
    locale = locale(
      decimal_mark = formatspek$desimaltegn,
      date_format = formatspek$dato,
      time_format = formatspek$klokkeslett,
      tz = formatspek$tidssone,
      encoding = formatspek$tegnkoding
    ),
    col_types = std_koltype_til_readr_koltype(spesifikasjon$vartype),
    col_names = spesifikasjon$varnavn_resultat
  )

  readr::stop_for_problems(d)

  # Konverter dato_kl
  # fixme: Fjern denne og oppdater std_koltype_til_readr_koltype() når
  #        https://github.com/tidyverse/readr/issues/642 er fiksa
  varnavn_dato_kl = spesifikasjon$varnavn_resultat[spesifikasjon$vartype == "dato_kl"]
  d = mutate(d, across(all_of(varnavn_dato_kl),
    .fns = \(dato_kl_vektor) {
      readr::stop_for_problems(
        readr::parse_datetime(dato_kl_vektor,
          format = formatspek$dato_kl,
          na = formatspek$na_verdier
        )
      )
    }
  ))

  # Konverter boolske
  varnavn_boolske = spesifikasjon$varnavn_resultat[spesifikasjon$vartype == "boolsk"]
  d = mutate(d, across(all_of(varnavn_boolske),
    .fns = \(x) {
      konverter_boolske(x,
        boolsk_usann = formatspek$boolsk_usann,
        boolsk_sann = formatspek$boolsk_sann
      )
    }
  ))

  d
}

#' Lag formatspek
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \code{lag_formatspek()} er en hjelpefunksjon som skal sikre at alle argumenter
#' i formatspek angis i riktig format og med riktig navn. Formatspek spesifiserer
#' hvilket format inndata er på og brukes i diverse innlesningsfunksjoner i rapwhale.
#'
#' @param skilletegn Angir hvilket tegn som brukes for å skille mellom kolonner i datafil
#' @param desimaltegn Angir hvilket tegn som brukes som desimaltegn? Må være ',' eller '.'
#' @param dato Angir hvilket dato-format som er brukt for datokolonner i inndata
#' @param klokkeslett Angir hvilket format som er brukt for klokkeslett
#' @param dato_kl Angir hvilket format som er brukt for variabler med både dato og klokkeslett
#' @param tidssone Angir hvilken tidssone som brukes
#' @param tegnkoding Angir tegnkodingen som brukes i inndata
#' @param boolsk_sann Angir hvilke verdier som tolkes som TRUE for boolske variabler
#' @param boolsk_usann Angir hvilke verdier som tolkes som FALSE for boolske variabler
#' @param na_verdier Angir hvilke verdier som tolkes som NA
#'
#' @keywords internal
lag_formatspek = function(skilletegn, desimaltegn, dato, klokkeslett, dato_kl,
                          tidssone, tegnkoding, boolsk_sann, boolsk_usann,
                          na_verdier) {
  stopifnot(
    is.character(skilletegn),
    nchar(skilletegn) == 1,
    is.character(desimaltegn),
    nchar(desimaltegn) == 1,
    desimaltegn %in% c(",", "."),
    is.character(dato),
    is.character(klokkeslett),
    is.character(dato_kl),
    is.character(tidssone),
    is.character(tegnkoding),
    rlang::is_empty(intersect(boolsk_sann, boolsk_usann))
  )

  formatspek = list(
    skilletegn = skilletegn,
    desimaltegn = desimaltegn,
    dato = dato,
    klokkeslett = klokkeslett,
    dato_kl = dato_kl,
    tidssone = tidssone,
    tegnkoding = tegnkoding,
    boolsk_sann = boolsk_sann,
    boolsk_usann = boolsk_usann,
    na_verdier = na_verdier
  )

  formatspek
}
