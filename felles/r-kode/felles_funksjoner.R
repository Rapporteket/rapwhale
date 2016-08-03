# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.


### Funksjon for å legge inn tall i en rapport som bruker latex

# Skriv talet x som \num{x}, for finformatering av tal
# med LaTeX (berre nødvendig for x > 999 og x < 0)
num = function(x) {
  paste0("\\num{", format(x, scientific = FALSE), "}")
}



### Funksjon som legger inn "NA" i tabeller hvis det er noen "NA".

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



### For akser...:

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




### Konfidensintervall for binomisk fordeling

# Brukar Wilson-intervallet, som anbefalt i ... (fixme) # Vel, den er anbefalt i
# "Binomial confidence intervals and contingency tests: '
# mathematical fundamentals and the evaluation of alternative methods", av Sean Wallis, University College London

ki_bin = function(x, n) {
  ki = binom.wilson(x, n)
  tibble(
    low = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    high = pmin(1, ki$upper)
  )
}

### Prosent med norsk stavemåte i aksenotasjoner.

# Tar inn eit desimaltal og viser det som prosent,
# med mellomrom før prosentteiknet (slik det skal vera
# på norsk), eks. 0.5 <U+2192> "50 %".
prosent = function(x) {
  stringr::str_replace(scales::percent(x), "%$", " %")
}

### Les inn CSV-fil (norsk Excel-format), og fjern BOM-teikn om det finst

# (fixme: ikkje lenger nødvendig i neste versjon
# av readr, men nødvendig 2016-08-01)
les_csv2 = function(x, ...) {
  df = read_csv2(x, ...)
  forste_kol = charToRaw(names(df)[1])
  har_bom = identical(forste_kol[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
  names(df)[1] = ifelse(har_bom,
    rawToChar(forste_kol[-(1:3)]),
    rawToBits(forste_kol)
  )
  df
}


### Funksjon som flytter opp labels inni grafer hvis de kolliderer.

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



### Normaliser variabelnamn til å ha _ som skiljeteikn og berre små bokstavar

# Eksempel:
# c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet") %>% normaliser_varnamn
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
