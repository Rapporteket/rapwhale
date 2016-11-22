
# Graffunksjoner ----------------------------------------------------------

#' Funksjonal for breaks-argument i ggplot2
#'
#' Funksjon som lager funksjon som tar inn to tall
#' og lager aritmetisk tallfølge med valgfri
#' intervallbredde slik at alle tall i følgen er
#' multiplum av intervallbredden og de to tallene
#' er innenfor range() av følgen (puh!).
#' Eks. er breaks_bredde(5)(c(9,16)) lik c(5,10,15,20).
#' Nyttig til bruk i breaks- og minor-breaks-argument
#' i ggplot2, eks. breaks = breaks_bredde(10)
#' Viss min eller maks er definert, bruk dette
#' i stedet for verdiene fra lims
#' @param bredde Intervallbredden til aksen. Standardverdi er 5.
#' @param min Laveste mulige verdi å vise på aksen. Standardverdi er NULL.
#' @param maks Høyeste mulige ver+di å vise på aksen. Standardverdi er NULL.
#' @export

breaks_bredde = function(bredde = 5, min = NULL, maks = NULL) {
  function(lims) {
    lims = c(max(min, lims[1]), min(maks, lims[2]))
    seq(round_any(lims[1], bredde, floor),
      round_any(lims[2], bredde, ceiling),
      by = bredde
    )
  }
}


#' Funksjon som flytter opp labels inni grafer hvis de kolliderer
#'
#'  Lagar ny y-koordinat, der tekstane forhåpentlegvis ikkje overlappar
#'  @param y y-koordinat til (midten av) tekstane
#'  @param tekst Teksten i tekstane (berre brukt til å telja kor mange linjer det er)
#'  @param hoyde Høgda kvar linje tekst tar opp (i grafkoordinatar). Viss teksten framleis
#'  overlappar, auk hoyde-argumentet.
#'  @export

flytt_opp = function(y, tekst, hoyde = .015) {
  tekst_ny = tekst[order(y)]
  y = y[order(y)]
  linjer = tekst_ny %>%
    str_split("\n") %>%
    sapply(length)
  nedre = y - linjer * hoyde / 2
  ovre = y + linjer * hoyde / 2
  for (i in 2:length(y)) {
    avs = nedre[i] - ovre[i - 1]
    if (avs < 0) {
      y[i] = y[i] - avs
      ovre[i] = ovre[i] - avs # Nedre treng me ikkje endra, sidan me ikkje brukar han
    }
  }
  y[match(tekst, tekst_ny)]
}

#' Lag linjegraf med 95 % konfidensintervall
#'
#' Krev følgjande aes-verdiar: x, y, ymin, ymax (dei to siste berre viss konfint = TRUE)
#' @param refline y-koordinaten til vassrett referanselinje
#' @param refline_df Dataramme med éi rad for kvart panel refline skal gjelda for. Valgfritt.
#' @param xlab Tekst på x-aksen (standardverdi: "År")
#' @param ylab Tekst på y-aksen (standardverdi: NULL (tom))
#' @param angle Viss TRUE, vis verdiane på x-aksen på skrå (for å få plass til fleire)
#' @param konfint Legg til konfidensintervall på kvar punkt
#' @export

graf_linje = function(refline = NULL, refline_df = NULL, xlab = "År", ylab = NULL,
                      angle = TRUE, konfint = TRUE) {
  grafdel = list()
  # Legg ev. til referanselinje(r)
  if (!is.null(refline)) {
    if (is.null(refline_df)) {
      grafdel = append(grafdel, list(geom_hline(yintercept = refline, col = colPrim[6], size = 2)))
    } else {
      grafdel = append(grafdel, list(geom_hline(
        data = refline_df,
        mapping = aes_string(yintercept = refline),
        col = colPrim[6], size = 2
      )))
    }
  }
  # Legg ev. til konfidensintervall (bak alt anna)
  if (konfint) {
    grafdel = append(grafdel, geom_linerange(size = .5, colour = colPrim[5]))
  }
  # Legg til resten
  grafdel = append(
    grafdel,
    list(
      geom_line(colour = colPrim[3], size = 1), # Linjer over tid
      geom_point(size = 2, colour = colPrim[2]), # Punkt
      xlab(xlab),
      ylab(ylab),
      tema,
      fjern_x
    )
  )
  if (angle) {
    grafdel = append(grafdel, list(theme(
      axis.text.x =
        element_text(angle = 45, vjust = 0.5)
    )))
  }
  grafdel
}


# Fargefunksjonar ---------------------------------------------------------

#' Lag mørkare/lysare fargar
#'
#' Gjer ein vektor med fargar mørkare.
#' Brukar CIELAB-fargerommet til utrekningar
#' (i staden for RGB-fargerommet), for
#' betre resultat (meir tilpassa korleis
#' synet vårt fungerer).
#' @param fargar Fargane man ynskjer å gjera mørkare/lysare.
#' Kan vere éin farge eller ein vektor med fargar. Brukar CIELAB-fargerommet.
#' @param grad Seier kor mykje mørkare fargen skal gjerast
#' (so bruk negative verdiar for å gjera han lysare).
#' @examples
#' colPrim = c(
#'   "#000059", "#084594", "#2171b5", "#4292c6",
#'   "#6baed6", "#c6dbef"
#' )
#' farge_morkare(fargar = colPrim, grad = 5)
#' @export

farge_morkare = function(fargar, grad = 5) {
  farge_lab = as(hex2RGB(fargar), "LAB")
  farge_lab@coords[, 1] = pmax(farge_lab@coords[, 1] - grad, 0)
  farge_rgb = as(farge_lab, "RGB")
  farge_rgb@coords[] = pmax(farge_rgb@coords, 0)
  farge_rgb@coords[] = pmin(farge_rgb@coords, 1)
  hex(farge_rgb)
}



# Innlesingsfunskjoner ----------------------------------------------------
#'
#' Les inn CSV-fil (norsk Excel-format) og fjern BOM-teikn om det finst.
#'
#' Fjern eventuelle hermeteikn (feil i read_csv*() gjer at ev. hermeteikn
#' i *første* kolonnenamn ikkje vert fjerna dersom fila har BOM)
#' les_csv er basert på read_csv, og skal dermed brukes for datasett med komma-separatorer.
#' les_csv2 har tatt utgangspunkt i read_csv2 og skal brukes i situasjoner med semikolondelte .csv filer.
#' Alle argumenter fra read_csv og read_csv2 kan brukes i les_csv og les_csv2.
#'
#' @param x Dataramme for innlesing
#' @param na Et argument for kva slags celler som regnes for å være manglande verdiar.
#' @export

# (fixme: ikkje lenger nødvendig i neste versjon
# av readr, > 1.0.0, men nødvendig 2016-08-08)

les_csv = function(x, ..., lesefunksjon = read_csv) {
  df = lesefunksjon(x, ...)
  namn1 = charToRaw(names(df)[1]) # Gjer første kolonnenamn om til råverdiar (byte-verdiar)
  har_bom = identical(namn1[1:3], as.raw(c(0xef, 0xbb, 0xbf)))

  # Fjern dei tre første bytane (BOM-teiknet) viss fila har BOM-teikn
  nytt_namn1 = ifelse(har_bom,
    rawToChar(namn1[-(1:3)]),
    rawToBits(namn1)
  )

  # Fjern eventuelle hermeteikn (feil i read_csv*() gjer at ev. hermeteikn
  # i *første* kolonnenamn ikkje vert fjerna dersom fila har BOM)
  nytt_namn1 = nytt_namn1 %>%
    str_replace_all('"', "")
  names(df)[1] = nytt_namn1
  df
}

#' Les inn CSV-fil (norsk Excel-format) og fjern BOM-teikn om det finst.
#'
#' Fjern eventuelle hermeteikn (feil i read_csv*() gjer at ev. hermeteikn
#' i *første* kolonnenamn ikkje vert fjerna dersom fila har BOM)
#' les_csv er basert på read_csv, og skal dermed brukes for datasett med komma-separatorer.
#' les_csv2 har tatt utgangspunkt i read_csv2 og skal brukes i situasjoner med semikolondelte .csv filer.
#' Alle argumenter fra read_csv og read_csv2 kan brukes i les_csv og les_csv2.
#'
#' @param x Dataramme for innlesing
#' @param na Et argument for kva slags celler som regnes for å være manglande verdiar.
#' @export

les_csv2 = function(x, ...) {
  les_csv(x, ..., lesefunksjon = read_csv2)
}


#' Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar
#'
#' @examples
#' c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet") %>%
#'   normaliser_varnamn()
#' # som gjev
#' c("hopp_og_sprett_test", "sykdoms_aktivitet_pasient_global_sykdomsaktivitet")
#' @export

normaliser_varnamn = function(x) {
  teikn = x %>%
    str_split("") # Splitt i enkeltteikn

  # Putt inn _ før alle store bokstavar (utanom første teikn i strengen)
  teikn = teikn %>%
    map(~ str_replace_all(., "([[:upper:]])", "_\\1"))

  teikn %>%
    map_chr(~ paste0(., collapse = "")) %>% # Slå saman til lange strengar igjen
    str_replace_all("[\\._ ]+", "_") %>% # Erstatt etterfølgjande punktum, mellomrom og/eller _ med éin _,
    str_replace_all("^_", "") %>% # Fjern ev. _ på starten av strengane
    tolower() # Gjer om til små bokstavar
}



# LaTeX-/rapportskrivingsfunksjoner ---------------------------------------

#' Funksjon for å legge inn tall i en rapport som bruker LaTeX
#'
#' Tar inn eit tal x og viser det som \num{x}, som (om nødvendig)
#' legg inn fine mellomrom som tusenskiljeteikn og endrar
#' desimalpunktum til desimalkomma.
#'
#' @param desimalar Talet på desimalar etter komma (rund av og vis så mange desimalar)
#' @export

num = function(x, desimalar) {
  # Argument til \num-kommandoen
  argtekst = ""

  # Spesialtilpass kommandoen etter talet på desimalar
  if (!missing(desimalar)) {
    # \num kan runda av for oss, men rundar av i R
    # for å sikra avrundinga vert identisk som ved
    # andre plassar der me ikkje brukar \num.
    x = round(x, desimalar)

    # Viss me vil ha desimalar, vis *alle* desimalane
    # (eks. vert både 3.1 og 3.123 vist som 3,1),
    # også for heiltal (eks. vert 3 vist som 3.0).
    if (desimalar > 0) {
      argtekst = paste0(
        argtekst,
        "round-precision=", desimalar,
        ",round-integer-to-decimal=true"
      )
    }
  }
  paste0("\\num[", argtekst, "]{", format(x, scientific = FALSE), "}")
}


#' Prosent med norsk stavemåte i aksenotasjoner
#'
#' Tar inn eit desimaltal og viser det som prosent,
#' med mellomrom før prosentteiknet (slik det skal vera
#' på norsk), eks. 0.5 <U+2192> "50 %".
#'
#' @param x Ein vektor med desimaltal
#' @export

prosent = function(x) {
  stringr::str_replace(scales::percent(x), "%$", " %")
}



# Andre funksjoner --------------------------------------------------------

#' Variant av table()-funksjonen som tar med NA-verdiar om dei finst
#'
#' Lag tabell som også viser NA-verdiar om dei finst
#' Brukar same argument som table()
#' @export

tab = function(...) {
  table(..., useNA = "ifany")
}


#' Avrundig med valfri presisjon og funksjon
#'
#' tatt frå plyr-pakken
#'
#' @param f round, floor eller ceiling.
#' @export

round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}


#' Konfidensintervall for binomisk fordeling
#'
#' Brukar Wilson-intervallet, som anbefalt i
#' «Binomial confidence intervals and contingency tests:
#' mathematical fundamentals and the evaluation of alternative methods»,
#' av Sean Wallis, University College London
#'
#' @export

ki_bin = function(x, n) {
  ki = binom.wilson(x, n)
  tibble(
    low = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    high = pmin(1, ki$upper)
  )
}

#' Konfidenstinervall basert på gjennomsnittet til en  kontinuerlig variabel
#'
#' Gir ut low, mean og high basert på verdiene til en kontinuerlig variabel
#' Hvis det er for få eller for lite varierende
#' observasjoner til å regne ut konfidensintervall,
#' returner NA for konfidensintervallene
#'
#' @param x numerisk vektor
#' @export

ki_univar = function(x) {
  # Hvis det er for få eller for lite varierende
  # observasjoner til å regne ut konfidensintervall,
  # returner NA for konfidensintervallene
  if ((length(x) < 2) | (sd(x, na.rm = TRUE) == 0)) {
    tibble(
      low = NA,
      mean = mean(x, na.rm = TRUE),
      high = NA
    )
  } else {
    mod = t.test(x)
    tibble(
      low = mod$conf.int[1],
      mean = mod$estimate,
      high = mod$conf.int[2]
    )
  }
}

#' LaTex tabell
#'
#' For å lage pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
#' med mulighet for å gjøre den stor nok til hele siden (wide = TRUE).
#' valgfrie argumenter inkluderer colheads=c() og caption = paste0("").
#'
#' @param dataframe dataramme
#' @param label Navnet på tabellen. Brukes til å referere til tabellen i
#' LaTex teksten.
#' @param wide Om man ønsker at tabellen skal være bred nok
#' til å dekke hele siden (TRUE) eller ikke (FALSE).
#' Standardverdi er FALSE.
#' @param colheads En vektor med tekst som skal være kolonnenavnene til variabelene i tabellen.
#' @param caption En tekstreng med hva som skal stå i beskrivelsen til tabellen.
#' @export

create_ltable = function(dataframe, label, wide = FALSE, ...) {
  table = capture.output(latex(dataframe,
    file = "", center = "centering",
    label = label, rowname = NULL,
    where = "htbp", booktabs = TRUE, numeric.dollar = FALSE, ...
  ))
  if (wide) {
    table %<>% str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
  }
  cat(table, sep = "\n")
}
