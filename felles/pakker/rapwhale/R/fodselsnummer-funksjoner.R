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

#' Sjekk om fødselsnummmer eller liknande er gyldige
#'
#' @description
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
fnr_er_gyldig_v2 = function(nummer,
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
#' Tek inn ein tekstvektor og gjev for kvart element ut `TRUE` viss det er
#' 11 teikn langt og berre inneheld siffer, og `FALSE` elles.
#'
#' @param nummer Tekstvektor.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er syntaktisk korrekt eller ikkje.
er_syntaktisk_fnr = function(nummer) {
  str_detect(nummer, "^\\d{11}$")
}

#' Sjekk gyldig dato
#'
#' @description
#' Tek inn ein tekstvektor med sekssifra element og gjev for kvart element ut
#' `TRUE` viss det er ein gyldig dato på formatet "DDMMYY", og `FALSE` elles.
#'
#' @param dato Tekstvektor der kvart element har 6 siffer.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `dato` alt etter om det høvesvis er ein gyldig dato eller ikkje.
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
#' Tek inn ein tekstvektorder der kvart element har 11 siffer,
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
#' `k1 = 11 - ((3 * d1 + 7 * d2 + 6 * m1 + 1 * m2 + 8 * å1 + 9 * å2 + 4 * i1 + 5 * i2 + 2 * i3) mod 11)`
#' `k2 = 11 - ((5 * d1 + 4 * d2 + 3 * m1 + 2 * m2 + 7 * å1 + 6 * å2 + 5 * i1 + 4 * i2 + 3 * i3 + 2 * k1) mod 11)`.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis gjev korrekt sjekksum eller ikkje.
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
#' Tek inn ein tekstvektorder der kvart element har 11 siffer,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig fødselsnummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor der kvart element har 11 siffer.
#'
#' @details
#' I eit gyldig fødselsnummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", og dei siste fem siffera eit personnummmer.
#' Personnummeret inneheld tre individsiffer, og to kontrollsiffer rekna ut
#' frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig F-nummer eller ikkje.
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
#' Tek inn ein tekstvektorder der kvart element har 11 siffer,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig D-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor der kvart element har 11 siffer.
#'
#' @details
#' I eit gyldig D-nummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", men der det er lagt til 4 til det fyrste sifferet.
#' Dei siste fem siffera er eit personnummmer som inneheld tre individsiffer,
#' og to kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig D-nummer eller ikkje.
er_gyldig_d_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  # Reknar ut faktiske datoar for D-nummer
  d_siffer = as.numeric(str_sub(nummer[gyldig], 1, 1))
  datoar = str_c(d_siffer - 4, str_sub(nummer[gyldig], 2, 6))

  gyldig[gyldig] = er_gyldig_fnr_dato(datoar) &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Sjekk gyldig H-nummer
#'
#' @description
#' Tek inn ein tekstvektorder der kvart element har 11 siffer,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig H-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor der kvart element har 11 siffer.
#'
#' @details
#' I eit gyldig H-nummer er dei fyrste seks siffera ein fødselsdato på
#' formatet "DDMMYY", men der det er lagt til 4 til det tredje sifferet.
#' Dei siste fem siffera er eit personnummmer som inneheld tre individsiffer,
#' og to kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig H-nummer eller ikkje.
er_gyldig_h_nummer = function(nummer) {
  gyldig = er_syntaktisk_fnr(nummer)

  # Reknar ut faktiske datoar for H-nummer
  h_siffer = as.numeric(str_sub(nummer[gyldig], 3, 3))
  datoar = str_c(
    str_sub(nummer[gyldig], 1, 2),
    h_siffer - 4,
    str_sub(nummer[gyldig], 4, 6)
  )

  gyldig[gyldig] = er_gyldig_fnr_dato(datoar) &
    er_fnr_sjekksum_korrekt(nummer[gyldig])

  gyldig
}

#' Sjekk gyldig FH-nummer
#'
#' @description
#' Tek inn ein tekstvektorder der kvart element har 11 siffer,
#' og gjev for kvart element ut `TRUE` viss det er eit gyldig FH-nummer,
#' og `FALSE` elles.
#'
#' @param nummer Tekstvektor der kvart element har 11 siffer.
#'
#' @details
#' Eit gyldig FH-nummer er eit ellevesifra nummer
#' der fyrste siffer er 8 eller 9,
#' dei neste åtte siffera er tilfeldige,
#' og dei to siste siffera er kontrollsiffer rekna ut frå dei føregåande siffera.
#'
#' @return Logisk vektor som gjev ut `TRUE` eller `FALSE` for kvart element
#' i `nummer` alt etter om det høvesvis er eit gyldig FH-nummer eller ikkje.
er_gyldig_fh_nummer = function(nummer) {
  logical(length(nummer)) # Plasshaldar for ekte implementasjon
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
