#' @importFrom stringr str_detect str_c str_sub str_split_fixed
NULL
#' Sjekk om fødselsnummmer eller liknande er gyldige
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn vektor med `nummer` som skal sjekkast og `gyldige_typar`,
#' som definerer kva type nummer desse potensielt kan vera
#' (fødselsnummer, H-nummer, D-nummer og liknande).
#' Gjev ut logisk vektor med `TRUE` for gyldige nummer og
#' `FALSE` for ugyldige nummer.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#' @param gyldige_typar Tekstvektor med kva nummertypar verdiane
#'  i `nummer` skal sjekkast mot. Må vera éin eller fleire av
#'  `c("FNR", "H", "D", "FH")`.
#'  Som standard vert nummera sjekka mot alle desse.
#'
#' @details
#' Dei moglege verdiane i `gyldige_typar` tyder:
#' - `"FNR"`: Fødselsnummer.
#'   Vanleg ellevesifra fødselsnummer
#'   som inneheld fødselsdato (seks siffer) etterfølgd av personnummer
#'   (fem siffer, der dei to siste er sjekksiffer).
#' - `"D"`: D-nummer.
#'   Ellevesifra nummer,
#'   som fødselsnummer,
#'   men modifisert ved at det er lagt til 4 på det fyrste sifferet.
#' - `"H"`: H-nummer (hjelpenummer).
#'   Ellevesifra nummer,
#'   som fødselsnummer,
#'   men modifisert ved at det er lagt til 4 på det tredje sifferet.
#' - `"FH"`: FH-nummer (felles hjelpenummer).
#'   Ellevesifra nummer der fyrste siffer er 8 eller 9,
#'   dei neste åtte siffera er tilfeldige,
#'   og dei to siste er kontrollsiffer.
#'
#' Du kan lesa meir om dei ulike nummertypane på desse nettsidene:
#'   - \href{https://no.wikipedia.org/wiki/F%C3%B8dselsnummer}{Wikipedia: Fødselsnummer}
#'   - \href{https://lovas.info/2013/12/01/identitetsnummer-i-norge/}{Identitetsnummer i Norge}
#'
#' @return Logisk vektor med `TRUE` eller `FALSE` for kvart element
#' i `nummer`, alt ettersom det høvesvis er eit gyldige nummer
#' av typane i `gyldige_typar` eller ikkje.
#' @export
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "70019950032", # Gyldig D-nummer
#'   "01410199935", # Gyldig H-nummer
#'   "88888888831", # Gyldig FH-nummer
#'   "98019800546"
#' ) # Ugyldig nummer generelt
#'
#' fnr_er_gyldig(nummer)
#'
#' fnr_er_gyldig(nummer, gyldige_typar = c("FNR", "H"))
fnr_er_gyldig = function(nummer,
                         gyldige_typar = c("FNR", "H", "D", "FH")) {
  stopifnot(all(gyldige_typar %in% c("FNR", "H", "D", "FH")))
  stopifnot(is.character(nummer))

  gyldig = logical(length(nummer))

  if ("FNR" %in% gyldige_typar) {
    gyldig[!gyldig] = er_gyldig_f_nummer(nummer[!gyldig])
  }
  if ("D" %in% gyldige_typar) {
    gyldig[!gyldig] = er_gyldig_d_nummer(nummer[!gyldig])
  }
  if ("H" %in% gyldige_typar) {
    gyldig[!gyldig] = er_gyldig_h_nummer(nummer[!gyldig])
  }
  if ("FH" %in% gyldige_typar) {
    gyldig[!gyldig] = er_gyldig_fh_nummer(nummer[!gyldig])
  }
  gyldig
}

#' Sjekk syntaktisk fødselsnummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor og gjev for kvart element ut `TRUE` viss det er
#' 11 teikn langt og berre inneheld siffer, og `FALSE` elles.
#'
#' @param nummer Tekstvektor.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er syntaktisk korrekt eller ikkje.
#'
#' @examples
#' nummer = c("12345612345", "123456789", "123456789ab", "abcdefghijk")
#'
#' rapwhale:::er_syntaktisk_fnr(nummer)
er_syntaktisk_fnr = function(nummer) {
  !is.na(nummer) & str_detect(nummer, "^\\d{11}$")
}

#' Sjekk gyldig dato
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor med sekssifra element og gjev for kvart element ut
#' `TRUE` viss det er ein gyldig dato på formatet "DDMMYY", og `FALSE` elles.
#'
#' @param dato Tekstvektor der kvart element har 6 siffer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `dato` alt etter om det høvesvis er ein gyldig dato eller ikkje.
#'
#' @examples
#' datoar = c(
#'   "010101",
#'   "290204", # Skotårsdag
#'   "290225", # «Skotårsdag» som aldri er gyldig ...
#'   "320101"
#' )
#'
#' rapwhale:::er_gyldig_fnr_dato(datoar)
er_gyldig_fnr_dato = function(dato) {
  dato_tekst = str_c(
    str_sub(dato, 1, 4),
    rep("20", length(dato)),
    str_sub(dato, 5, 6)
  ) # Legg til hundreårsinfo

  # Sjekk om det kan vera ein gyldig dato
  datoar = as.Date(dato_tekst, format = "%d%m%Y")
  !is.na(datoar)
}

#' Kontroller sjekksum for fødselsnummer og liknande
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor der kvart element har 11 siffer,
#' og gjev for kvart element ut `TRUE` viss dei to siste siffera,
#' kontrollsiffera, er korrekt i høve dei føregåande siffera,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor der kvart element har 11 siffer.
#'
#' @details
#' Kvart av kontrollsiffera skal vera ein sjekksum rekna ut frå dei
#' føregåande siffera. Viss dei ni fyrste siffera av eit fødselsnummer,
#' eller anna liknande nummer, er d1 d2 m1 m2 å1 å2 i1 i2 i3,
#' skal kontrollsiffera, k1 og k2, oppfylla fylgjande:
#' - `k1 = 11 - ((3 * d1 + 7 * d2 + 6 * m1 + 1 * m2 + 8 * å1 + 9 * å2 + 4 * i1 + 5 * i2 + 2 * i3) mod 11)`
#' - `k2 = 11 - ((5 * d1 + 4 * d2 + 3 * m1 + 2 * m2 + 7 * å1 + 6 * å2 + 5 * i1 + 4 * i2 + 3 * i3 + 2 * k1) mod 11)`.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis gjev korrekt sjekksum eller ikkje.
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "15076500575"
#' ) # Feil i kontrollsiffer
#'
#' rapwhale:::er_fnr_sjekksum_korrekt(nummer)
er_fnr_sjekksum_korrekt = function(nummer) {
  # Del fødselsnummera opp i siffer
  siffer = str_split_fixed(nummer, "", n = 11) %>%
    t()

  # Sidan as.numeric() mistar matrisedimensjonane,
  # lat oss ta vare på dei manuelt
  di = dim(siffer)
  siffer = as.numeric(siffer)
  dim(siffer) = di

  # Koeffisientar for utrekning
  koef1 = c(3, 7, 6, 1, 8, 9, 4, 5, 2)
  koef2 = c(5, 4, 3, 2, 7, 6, 5, 4, 3, 2)

  # Rekn ut kva kontrollsiffera *skulle* vera
  # Første kontrollsiffer
  k1 = 11 - (colSums(koef1 * siffer[1:9, , drop = FALSE]) %% 11)
  k1 = ifelse(k1 == 11, 0, k1)

  # Andre kontrollsiffer
  k2 = 11 - (colSums(koef2 * rbind(siffer[1:9, , drop = FALSE], k1)) %% 11)
  k2 = ifelse(k2 == 11, 0, k2)

  # Er dei utrekna kontrollsiffera lik dei oppgjevne?
  (k1 == siffer[10, ]) & (k2 == siffer[11, ])
}

#' Sjekk gyldig F-nummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig fødselsnummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#'
#' @details
#' I eit gyldig fødselsnummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", og dei siste fem siffera eit personnummmer.
#' Personnummeret inneheld tre individsiffer, og to kontrollsiffer rekna ut
#' frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig F-nummer eller ikkje.
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "70019950032", # Gyldig D-nummer
#'   "01410199935", # Gyldig H-nummer
#'   "88888888831", # Gyldig FH-nummer
#'   "98019800546"
#' ) # Ugyldig nummer generelt
#'
#' rapwhale:::er_gyldig_f_nummer(nummer)
er_gyldig_f_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  datoar = str_sub(nummer[gyldig], 1, 6)

  gyldig[gyldig] = er_gyldig_fnr_dato(datoar) &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Sjekk gyldig D-nummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig D-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#'
#' @details
#' I eit gyldig D-nummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", men der det er lagt til 4 til det fyrste sifferet.
#' Dei siste fem siffera er eit personnummmer som inneheld tre individsiffer,
#' og to kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig D-nummer eller ikkje.
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "70019950032", # Gyldig D-nummer
#'   "01410199935", # Gyldig H-nummer
#'   "88888888831", # Gyldig FH-nummer
#'   "98019800546"
#' ) # Ugyldig nummer generelt
#'
#' rapwhale:::er_gyldig_d_nummer(nummer)
er_gyldig_d_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  # Sjekkar at fyrste siffer er 4, 5, 6 eller 7
  fyrste_siffer = as.numeric(str_sub(nummer[gyldig], 1, 1))
  gyldig[gyldig] = fyrste_siffer %in% 4:7

  # Reknar ut faktiske datoar for D-nummer
  datoar = str_c(
    as.numeric(str_sub(nummer[gyldig], 1, 1)) - 4,
    str_sub(nummer[gyldig], 2, 6)
  )

  gyldig[gyldig] = er_gyldig_fnr_dato(datoar) &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Sjekk gyldig H-nummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig H-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#'
#' @details
#' I eit gyldig H-nummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", men der det er lagt til 4 til det tredje sifferet.
#' Dei siste fem siffera er eit personnummmer som inneheld tre individsiffer,
#' og to kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig H-nummer eller ikkje.
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "70019950032", # Gyldig D-nummer
#'   "01410199935", # Gyldig H-nummer
#'   "88888888831", # Gyldig FH-nummer
#'   "98019800546"
#' ) # Ugyldig nummer generelt
#'
#' rapwhale:::er_gyldig_h_nummer(nummer)
er_gyldig_h_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  # Sjekkar at tredje siffer er 4 eller 5
  tredje_siffer = as.numeric(str_sub(nummer[gyldig], 3, 3))
  gyldig[gyldig] = tredje_siffer %in% 4:5

  # Reknar ut faktiske datoar for H-nummer
  datoar = str_c(
    str_sub(nummer[gyldig], 1, 2),
    as.numeric(str_sub(nummer[gyldig], 3, 3)) - 4,
    str_sub(nummer[gyldig], 4, 6)
  )

  gyldig[gyldig] = er_gyldig_fnr_dato(datoar) &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Sjekk gyldig FH-nummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn ein tekstvektor,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig FH-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#'
#' @details
#' Eit gyldig FH-nummer er eit ellevesifra nummer
#' der fyrste siffer er 8 eller 9,
#' dei neste åtte siffera er tilfeldige,
#' og dei to siste siffera er kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig FH-nummer eller ikkje.
#'
#' @examples
#' nummer = c(
#'   "15076500565", # Gyldig F-nummer
#'   "70019950032", # Gyldig D-nummer
#'   "01410199935", # Gyldig H-nummer
#'   "88888888831", # Gyldig FH-nummer
#'   "98019800546"
#' ) # Ugyldig nummer generelt
#'
#' rapwhale:::er_gyldig_fh_nummer(nummer)
er_gyldig_fh_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  # Fyrste siffer, som skal vera 8 eller 9 i FH-nummer
  fh_siffer = as.numeric(str_sub(nummer[gyldig], 1, 1))

  gyldig[gyldig] = fh_siffer %in% 8:9 &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Finn type ID-nummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Tek inn vektor med `nummer` som skal sjekkast,
#' og gjev ut ein tekstvektor som elementvis seier kva type ID-nummer det er,
#' og `NA` for element som ikkje er ein av dei støtta typane ID-nummer
#' (fødselsnummer, D-nummer, H-nummer og FH-nummer). Sjå [fnr_er_gyldig()]
#' for meir info om dei ulike typane.
#'
#' @param nummer Tekstvektor med nummer som skal sjekkast.
#'
#' @return Tekstvektor som elemetvis seier kva type ID-nummer elementa
#' i `nummer` er.
#' @export
#'
#' @examples
finn_type_idnummer = function(nummer) {
  rep(NA, length(nummer)) # Plasshaldar for ekte implementasjon
}

#' Foreslå lignende fødselsnummer
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Hvis det finnes ugyldige fødselsnummer kan denne funksjonen brukes for å
#' foreslå lignende fødselsnummer som *er* gyldige. Dette gjøres ved å bytte
#' ut ett vilkårlig siffer, eller bytter om på to nabosiffer.
#'
#' @details
#' OBS! *Ikke* bruk resultatene av funksjonen direkte. Sjekk de oppgitte
#' fødselsnummer i DIPS eller andre kilder for å kontrollere at de viser til
#' rett person.
#'
#' @param x tekststreng med ugyldige fødselsnummer som det skal
#' foreslås lignende fødselsnummer for.
#'
#' @return
#' Returnerer en tekstvektor med et gyldig fødselsnummer hvis det finnes et
#' nærliggende fødselsnummer som er gyldig. Hvis det ikke finnes et gyldig
#' fødselsnummer returneres en tom tekststreng.
#'
#' @examples
#' ugyldig_fodselsnummer = "11284968756"
#' fnr_foresla(ugyldig_fodselsnummer)
fnr_foresla = function(x) {
  stopifnot(length(x) == 1 && is.character(x) && nchar(x) == 11)

  # Bytt ut einskildsiffer med eit (vilkårleg) anna
  moglege_fnr_1 = str_c(
    str_sub(x, 1, 0:10),
    rep(0:9, each = 11),
    str_sub(x, 2:12, 11)
  )

  # Byt om på to etterfølgjande siffer
  x_mat = str_split_fixed(x, "", 11) %>%
    matrix(ncol = 11, nrow = 10, byrow = TRUE)
  for (nr in 1:10) {
    x_mat[nr, nr:(nr + 1)] = x_mat[nr, (nr + 1):nr]
  }
  moglege_fnr_2 = apply(x_mat, 1, str_c, collapse = "")

  # Returner dei kandidatnummera som er gyldige
  moglege_fnr = unique(c(moglege_fnr_1, moglege_fnr_2))
  moglege_fnr[fnr_er_gyldig(moglege_fnr)]
}
