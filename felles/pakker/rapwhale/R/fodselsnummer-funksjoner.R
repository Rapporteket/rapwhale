#' @importFrom stringr str_detect str_c str_sub str_split_fixed
NULL
#' Test om fødselsnummer er gyldig
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Tester om fødselsnummer (inkludert D-nummer og H-nummer) er gyldige.
#'
#' @details
#' Basert på skildringene fra:
#' * https://no.wikipedia.org/wiki/F%C3%B8dselsnummer
#' * https://lovas.info/2013/12/01/identitetsnummer-i-norge/
#'
#' @param x En tekstvektor med fødselsnummer som skal sjekkes.
#'
#' @return
#' Returnerer en navngitt logisk variabel som indikerer om fødselsnummer
#' er gyldige eller ikke.
#' @export
#'
#' @examples
#' d_fnr = c("01234567899", "15354897865", "15151515151", "11037627154")
#' fnr_er_gyldig(d_fnr)
fnr_er_gyldig = function(x) {
  # Sjekk først at det er snakk om tekststreng,
  # ikkje tal (som ville mista førstesifferet om dette er 0)
  if (!is.character(x)) {
    stop("Fødselsnummer må vera ein tekstvektor")
  }

  # Talet på fødselsnummer me sjekkar
  n = length(x)

  # Rekn først alle fødselsnummera som gyldige
  # (og så markerer me dei som ugyldige etter
  # kvart som dei ikkje passerer testar)
  ok = !logical(n)
  names(ok) = x # Hugselapp for å sjå kva nummer me testar ...


  ##### TEST 1: Har me data i det heile tatt? #####

  # Dei som ikkje har nokon verdiar er iallfall ugyldige
  ok[is.na(x)] = FALSE


  ##### TEST 2: Har me (berre) 11 fornuftige siffer? #####
  # Sjekk at nummeret har 11 siffer og at siffera er nokolunde OK
  #
  # Fødselsnummer er bygde opp som
  #
  #   DDMMÅÅIIIKK (dag, månad, år, individnummer, kontrollsiffer)
  #
  # DD er 01-31 (ekte fødselsnummer) eller 41-71 (D-nummer)
  # MM er 01-12 (ekte fødselsnummer) eller 41-52 (H-nummer)
  # ÅÅ er 00-99
  # III = 000-999
  # KK = 00-99

  # Regulært uttrykk som lukar ut dei grovaste feila
  re_siffer = "^[0-7][0-9][0145][0-9][0-9]{7}$"
  ok[ok] = str_detect(x[ok], regex(re_siffer))


  ##### TEST 3: Ser dei først seks siffera ut som datoar? #####
  # Me vil sjekka om datodelen (dei første 6 siffera)
  # er ein gyldig dato, men får problem med at me
  # ikkje veit kva hundreår det er snakk om.
  # Er berre problem for 29. februar, og skotår er
  # dei same for alle alle hundreåra (eks. var både
  # 1804, 1904 og 2004 skotår), med unntak av år --00,
  # som var skotår i 2000 men ikkje i 1900 eller 1800
  # (eller andre hundreår som ikkje er delbare på 400).
  #
  # Latar derfor som alle år var på 2000-talet, slik
  # at me ikkje feilaktig påstår 29. februar ikkje
  # eksisterte.
  #
  # I teorien kunne me brukt individnummera
  # til å gjetta hundreår, men reglane her endrar seg
  # etter kvart som me går tom for fødselsnummer ... :(
  #
  # Viss første siffer er 4, 5, 6 eller 7, kan det vera
  # eit D-nummer. Då trekker me 4 frå sifferet for å laga fødselsdato.
  # Viss tredje siffer er 4 eller 5, kan det vera
  # eit H-nummer. Då trekker me 4 frå sifferet for å laga fødselsdato.
  dato_tekst = str_c(str_sub(x[ok], 1, 4), "20", str_sub(x[ok], 5, 6)) # Legg til hundreårsinfo
  fs_dag = as.numeric(str_sub(dato_tekst, 1, 1)) # Første siffer i dagverdien
  dnummer = fs_dag %in% 4:7 # Er det snakk om D-nummer?
  fs_mnd = as.numeric(str_sub(dato_tekst, 3, 3)) # Første siffer i månadsverdien
  hnummer = fs_mnd %in% 4:5 # Er det snakk om H-nummer?
  # Rekn ut den faktiske fødselsdatoen dersom
  # det er snakk om D-nummer eller H-nummer
  # (ved å trekka 4 frå høvesvis første dag-
  # eller månadssiffer)
  dato_dmod = str_c(fs_dag - 4, str_sub(dato_tekst, 2, 8))
  dato_hmod = str_c(str_sub(dato_tekst, 1, 2), fs_mnd - 4, str_sub(dato_tekst, 4, 8))
  # Eit nummer kan berre anten vera fødselsnummer,
  # D-nummer eller H-nummer, ikkje fleire samtidig
  # Bruk den utrekna datoen basert på kva type nummer det er.
  dato_tekst = ifelse(dnummer, dato_dmod, ifelse(hnummer, dato_hmod, dato_tekst))

  # Sjekk om det kan vera ein gyldig dato
  dato = as.Date(dato_tekst, format = "%d%m%Y")
  ok[ok][is.na(dato)] = FALSE


  ##### TEST 4: Er kontrollsiffera korrekte? #####
  # Til slutt den viktige, store og kraftige testen
  # basert på korleis fødselsnummer er designa:
  # Me sjekkar kontrollsiffera, dei to siste siffera.
  if (any(ok)) {
    # Del fødselsnummera opp i siffer
    x2 = x[ok]
    siffer = str_split_fixed(x2, "", n = 11) %>%
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
    ok[ok] = (k1 == siffer[10, ]) & (k2 == siffer[11, ])
  }

  # Alle testane er utførte. Returner info om fødselsnummeret
  # er gyldig.
  ok
}

#' Sjekk gyldig fødselsnummmer og liknande
#'
#' @param nummer Tekstvektor med fødselsnummer.
#' @param gyldige_typar Tekstvektor med typar fødselsnummer som skal reknast
#' som gyldige. Standard verdi er `c("FNR", "H", "D", "FH")`.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis gyldige fødselsnummer av typane i
#' `gyldige_typar` eller ikkje.
#' @export
#'
#' @examples
fnr_er_gyldig_v2 = function(nummer, gyldige_typar = c("FNR", "H", "D", "FH")) {
  stopifnot(all(gyldige_typar %in% c("FNR", "H", "D", "FH")))
  stopifnot(!is.character(nummer))

  gyldig = er_syntaktisk_fnr(nummer)

  if ("FNR" %in% gyldige_typar) {
    gyldig[gyldig] = er_gyldig_f_nummer(nummer[gyldig])
  }
  if ("H" %in% gyldige_typar) {
    gyldig[gyldig] = er_gyldig_h_nummer(nummer[gyldig])
  }
  if ("D" %in% gyldige_typar) {
    gyldig[gyldig] = er_gyldig_d_nummer(nummer[gyldig])
  }
  if ("FH" %in% gyldige_typar) {
    gyldig[gyldig] = er_gyldig_fh_nummer(nummer[gyldig])
  }
  gyldig
}

#' Sjekk syntaktisk fødselsnummer
#'
#' @param nummmer Tekstvektor.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er syntaktisk korrekt eller ikkje.
er_syntaktisk_fnr = function(nummmer) {

}

#' Sjekk gyldig dato
#'
#' @param dato Tekstvektor der kvart element har seks siffer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `dato` alt etter om det høvesvis er ein gyldig dato eller ikkje.
er_gyldig_fnr_dato = function(dato) {

}

#' Sjekk gyldig F-nummer
#'
#' @param nummer Tekstvektor med F-nummer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig F-nummer eller ikkje.
er_gyldig_f_nummer = function(nummer) {

}

#' Sjekk gyldig H-nummer
#'
#' @param nummer Tekstvektor med H-nummer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig H-nummer eller ikkje.
er_gyldig_h_nummer = function(nummer) {

}

#' Sjekk gyldig D-nummer
#'
#' @param nummer Tekstvektor med D-nummer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig D-nummer eller ikkje.
er_gyldig_d_nummer = function(nummer) {

}

#' Sjekk gyldig FH-nummer
#'
#' @param nummer Tekstvektor med FH-nummer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig FH-nummer eller ikkje.
er_gyldig_fh_nummer = function(nummer) {

}

#' Kontroller sjekksum for fødselsnummer og liknande
#'
#' @param nummer Tekstvektor med fødselsnummer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis gjev korrekt sjekksum eller ikkje.
er_fnr_sjekksum_korrekt = function(nummer) {

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
