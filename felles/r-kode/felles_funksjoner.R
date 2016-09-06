# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.


# Graffunksjoner ----------------------------------------------------------

### Funksjonal for breaks-argument i ggplot2

# Funksjon som lager funksjon som tar inn to tall
# og lager aritmetisk tallfølge med valgfri
# intervallbredde slik at alle tall i følgen er
# multiplum av intervallbredden og de to tallene
# er innenfor range() av følgen (puh!).
#
# Eks. er breaks_bredde(5)(c(9,16)) lik c(5,10,15,20).
# Nyttig til bruk i breaks- og minor-breaks-argument
# i ggplot2, eks. breaks = breaks_bredde(10)
# Viss min eller maks er definert, bruk dette
# i stedet for verdiene fra lims
breaks_bredde = function(bredde = 5, min = NULL, maks = NULL) {
  function(lims) {
    lims = c(max(min, lims[1]), min(maks, lims[2]))
    seq(round_any(lims[1], bredde, floor),
      round_any(lims[2], bredde, ceiling),
      by = bredde
    )
  }
}


### Funksjon som flytter opp labels inni grafer hvis de kolliderer

# Innargument:
#   y:     y-koordinat til (midten av) tekstane
#   tekst: teksten i tekstane (berre brukt til å telja kor mange linjer det er)
#   hoyde: høgda kvar linje tekst tar opp (i grafkoordinatar)
# Ut: Ny y-koordinat, der tekstane forhåpentlegvis ikkje overlappar (elles: auk hoyde-argumentet)
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



# Fargefunksjonar ---------------------------------------------------------

### Lag mørkare/lysare fargar

# Gjer ein vektor med fargar mørkare.
# Brukar CIELAB-fargerommet til utrekningar
# (i staden for RGB-fargerommet), for
# betre resultat (meir tilpassa korleis
# synet vårt fungerer).
#
# «grad» seier kor mykje mørkare fargen
# skal gjerast (so bruk negative verdiar for
# å gjera han lysare).
library(colorspace)
farge_morkare = function(fargar, grad = 5) {
  farge_lab = as(hex2RGB(fargar), "LAB")
  farge_lab@coords[, 1] = pmax(farge_lab@coords[, 1] - grad, 0)
  farge_rgb = as(farge_lab, "RGB")
  farge_rgb@coords[] = pmax(farge_rgb@coords, 0)
  farge_rgb@coords[] = pmin(farge_rgb@coords, 1)
  hex(farge_rgb)
}



# Innlesingsfunskjoner ----------------------------------------------------

### Les inn CSV-fil (norsk Excel-format) og fjern BOM-teikn om det finst

# (fixme: ikkje lenger nødvendig i neste versjon
# av readr, > 1.0.0, men nødvendig 2016-08-08)
library(readr)
library(magrittr)
library(stringr)
les_csv2 = function(x, ...) {
  df = read_csv2(x, ...)
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


### Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar

# Eksempel på bruk:
#   c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet") %>% normaliser_varnamn
#   som gjev
#   c("hopp_og_sprett_test", "sykdoms_aktivitet_pasient_global_sykdomsaktivitet")
library(purrr)
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

### Funksjon for å legge inn tall i en rapport som bruker LaTeX

# Tar inn eit tal x og viser det som \num{x}, som (om nødvendig)
# legg inn fine mellomrom som tusenskiljeteikn og endrar
# desimalpunktum til desimalkomma.

# Innargument:
#   desimalar: talet på desimalar etter komma (rund av og vis så mange desimalar)
num = function(x, desimalar) {
  # Argument til \num-kommandoen
  argtekst = ""

  # Spesialtilpass kommandoen etter talet på desimalar
  if (!missing(desimalar)) {
    # \num kan runda av for oss, men rundar av i R
    # for å sikra avrundinga vert identisk som ved
    # andre plassar der me ikkje brukar \num.
    x = round(x, desimalar)

    # Viss me vil ha desimalar, vis alle desimalane
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


### Prosent med norsk stavemåte i aksenotasjoner

# Tar inn eit desimaltal og viser det som prosent,
# med mellomrom før prosentteiknet (slik det skal vera
# på norsk), eks. 0.5 <U+2192> "50 %".
prosent = function(x) {
  stringr::str_replace(scales::percent(x), "%$", " %")
}



# Andre funksjoner --------------------------------------------------------

### Variant av table()-funksjonen som tar med NA-verdiar om dei finst

# Lag tabell som også viser NA-verdiar om dei finst
tab = function(...) {
  table(..., useNA = "ifany")
}


### Avrundig med valfri presisjon og funksjon

# tatt frå plyr-pakken (nyttige verdiar
# av f er round, floor og ceiling)
round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}


#### Konfidensintervall for binomisk fordeling

# Brukar Wilson-intervallet, som anbefalt i
# «Binomial confidence intervals and contingency tests:
# mathematical fundamentals and the evaluation of alternative methods», av Sean Wallis, University College London
library(binom)
ki_bin = function(x, n) {
  ki = binom.wilson(x, n)
  tibble(
    low = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    high = pmin(1, ki$upper)
  )
}
