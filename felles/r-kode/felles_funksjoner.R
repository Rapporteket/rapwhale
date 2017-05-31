# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.

# grafobjekter ----------------------------------------------------------
# de offisielle fargene (som eg ikkje er så glad i)
# du mener, som INGEN liker.
library(ggplot2)

colPrim = c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt = c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr = "#FF7260" # Kontrastfarge

# ggplot2-tema for figurar
tema = theme_light(base_size = 13)
tema$panel.grid.minor$colour = "white"
tema$strip.background$fill = "#f3f1ee"
tema$strip.background$colour = "#e4e0da"
tema$strip.text.x$colour = "black"
tema$panel.spacing = unit("13", "pt")
tema$panel.border$colour = tema$strip.background$colour
tema$panel.grid.major$colour = tema$strip.background$colour
tema$panel.grid.minor$colour = tema$strip.background$fill
tema$axis.title.y$angle = 0
tema$axis.title.y$margin = margin(r = 5)
tema$axis.title.x$margin = margin(t = 5)

# Fjern vannrette eller loddrette rutenett
fjern_x = theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)
fjern_y = theme(
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)

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

### Lag linjegraf med 95 % konfidensintervall

# Krev følgjande aes-verdiar: x, y, ymin, ymax (dei to siste berre viss konfint = TRUE)
# Argument:
#   refline:    y-koordinaten til vassrett referanselinje
#   refline_df: ev. dataramme med éi rad for kvart panel refline skal gjelda for
#   xlab:       tekst på x-aksen (standardverdi: "År")
#   ylab:       tekst på y-aksen (standardverdi: NULL (tom))
#   angle:      viss sann, vis verdiane på x-aksen på skrå (for å få plass til fleire)
#   konfint:    Legg til konfidensintervall på kvar pnutk
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



# Variabelnamnfunksjonar ----------------------------------------------------


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


### Prosent med norsk stavemåte i aksenotasjoner

# Tar inn eit desimaltal og viser det som prosent,
# med mellomrom før prosentteiknet (slik det skal vera
# på norsk), eks. 0.5 --> "50 %".
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

### Konfidenstinervall basert på gjennomsnittet til en  kontinuerlig variabel
# med mulighet for bootstrap lagt inn i funksjonen
library(dplyr)

ki_univar = function(x, bootstrap = FALSE, antall, ...) {
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

    # Hvis man ønsker boostrap kjøres denne koden,
    # antall ganger beskrives med argumentet "antall"
    if (bootstrap == TRUE) {
      utvalg = sample(x, antall, replace = TRUE)
      mod = t.test(utvalg)

      tibble(
        low = mod$conf.int[1],
        mean = mod$estimate,
        high = mod$conf.int[2]
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
}

# For å lage pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
# med mulighet for å gjøre den stor nok til hele siden (wide = TRUE).
# optional arguments inkluderer colheads=c() og caption = paste0("").
library(Hmisc)
library(stringr)
library(magrittr)
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
