# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# (og noen, uunnværlige) i registersammenheng og i andre sammenheng.


# LaTeX-ting --------------------------------------------------------------

# Kopier klassefila me brukar til ein plass LaTeX finn ho,
# slik at me slepp å ha ein kopi overalt.
# Sjå https://tex.stackexchange.com/a/1138 for meir informasjon.
texmappe_rot = system2("kpsewhich", "-var-value=TEXMFHOME", stdout = TRUE)
texmappe = paste0(texmappe_rot, "/tex/latex/kvalreg/")
dir.create(texmappe, showWarnings = FALSE, recursive = TRUE)
invisible(file.copy(
  from = "h:/kvalreg/felles/latex-klassar/kvalreg-rapport.cls",
  to = texmappe, overwrite = TRUE, copy.date = TRUE
))





# Fargar og grafinnstillingar/-objekt -------------------------------------

# Dei offisielle fargene (som eg ikkje er så glad i)
# du mener, som INGEN liker.
colPrim = c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef") # Primærfarge (mørk til lys)
colNoyt = c("#4D4D4D", "#737373", "#A6A6A6", "#DADADA") # Nøytralfarge
colKontr = "#FF7260" # Kontrastfarge

# ggplot2-tema for figurar
library(ggplot2)
if (!exists("skriftstorleik")) { # Skriftstorleik bør vera definert i kvar årsrapportfil
  skriftstorleik = 13
}
tema = theme_light(base_size = skriftstorleik)
tema$panel.grid.minor$colour = "white"
tema$strip.background$fill = "#f3f1ee"
tema$strip.background$colour = "#e4e0da"
tema$strip.text.x = element_text(colour = "black")
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

# Fjern strekmarkeringar for viste tal/kategoriar
# (tilsvarer «major breaks» på aksen).
# Dette er nyttig for søylediagram med kategoriske
# verdiar, der strekmarkeringane er unødvendige/stygge.
fjern_x_ticks = theme(axis.ticks.x = element_blank())
fjern_y_ticks = theme(axis.ticks.y = element_blank())

# Søyler skal i starta heilt inn til aksen, men ha litt luft
# over seg, altså asymmetriske expand-verdiar. Her er ein
# variabel som definerer dette, og som ein kan mata til
# expand-argumentet til skaladefinisjonar.
expand_soyle = expand_scale(mult = c(0.0, .05), add = 0)

# Fjern luft til venstre for y-akseteksten og legg
# til ekstra luft til høgre for han, fjern luft under
# x-akseteksten og legg til ekstra luft over han,
# fjern nesten all luft rundt figurane (eventuell
# nødvendig luft legg me til via LaTeX).
#
# (Merk: Me set den ytre margen til 3 punkt
# opp og nede i staden for 0 punkt for å sikra at at
# kantane på alle bokstavane alltid vert med.)
tema$axis.title.y$margin = margin(r = tema$text$size / 2)
tema$axis.title.x$margin = margin(t = tema$text$size / 2)
tema$plot.margin = margin(3, 3, 3, 3)


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

# Funksjon for å lage shewhart charts
# Denne følger samme input som qic i qicharts-pakken
# Den skal utivkles til ggplot format etterhvert.
# y = responsvariabel (y-aksen)
# antall = nevneren i en prosent-chart
# x = forklaringsvariabel/tisdvariabel/gruppe (x-aksen)
# figtype = figurtypen, basert på typer i kallet til qic. som oftest xbar eller p (prosentfigur)
# d = data, gjerne på long format
# tittel = Hvis du vil mot all formening ha en tittel til "Figuren".
# gruppe = grupper som deles opp i et panel.

lag_shewhart = function(y, x, antall = NULL, figtype, skriftstorleik = 0.8,
                        data, tittel = NULL, gruppe = NULL, ...) {

  # argumenter som gjelder ALLE shewhart diagram
  tcc_args = list(n = data[[y]], x = data[[x]], chart = figtype, cex = skriftstorleik, main = tittel, ...)

  # argumentet "antall" brukes bare av noen figurtyper. deriblandt figurtype = "p", prosent, som trenger en nevner
  if (!is.null(antall)) {
    tcc_args = append(tcc_args, list(d = data[[antall]]))
  }
  if (!is.null(gruppe)) { # hvis man ønsker å dele opp panelet i grupper
    tcc_args = append(tcc_args, list(g1 = data[[gruppe]]))
  }
  do.call(tcc, tcc_args) +
    theme(
      axis.title.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.line.x = element_line(color = "black", size = 0.5), # noe fjerner aksen fjerner akse
      axis.line.y = element_line(color = "black", size = 0.5)
    ) # legger dem til igjen her
}

### Lag linjegraf med 95 % konfidensintervall

# Krev følgjande aes-verdiar: x, y, ymin, ymax (dei to siste berre viss konfint = TRUE)
# Argument:
#   refline:    y-koordinaten til vassrett referanselinje
#   refline_df: ev. dataramme med éi rad for kvart panel refline skal gjelda for
#   xlab:       tekst på x-aksen (standardverdi: "År")
#   ylab:       tekst på y-aksen (standardverdi: NULL (tom))
#   angle:      viss sann, vis verdiane på x-aksen på skrå (for å få plass til fleire)
#   konfint:    Legg til konfidensintervall på kvar punkt
graf_linje = function(refline = NULL, refline_df = NULL, xlab = "\uc5r", ylab = NULL,
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

# funksjon for å lage p-chart shewhart-diagram med eller uten facets
# funksjonen krever et datasett (d) som inneholder teller (y) og nevner (n), med variabel for x.aksen (x) som
# ofte er en datovariabel, men hvis det ikke er det settes tidsvinsing til FALSE.
# panel_gruppe er hvilken variabel man ønkser å dele opp et panel på (gjelder kun tidsvisning),
# periode hvilket tidsrom (f.eks "month" eller "2 months", gjelder kun tidsvising) og tittel for tittelen til plottet.
# krever pakkene tidyverse og qicharts2
lag_shewhart_pro = function(d, x, y, n, panel_gruppe, tidsvisning = TRUE, periode, tittel) {
  d$qic_x = d[[x]]
  d$qic_y = d[[y]]
  d$qic_n = d[[n]]
  d$qic_facet = d[[panel_gruppe]]

  if (tidsvisning) {
    plot = suppressMessages(qic(
      x = qic_x,
      y = qic_y, # telleren i indikatoren
      n = qic_n, # nevneren til indikatoren
      data = d,
      facets = ~qic_facet,
      chart = "p", # plottypen
      x.period = periode,
      show.labels = FALSE,
      xlab = NULL,
      ylab = "Andel",
      title = tittel
    ) +
      tema +
      fjern_x +
      fjern_y +
      theme(legend.position = "none") +
      scale_x_discrete(expand = c(0, 0.1)) +
      scale_y_continuous(labels = akse_prosent))
  } else {

    # setter i rekkefølge fra størst til minst i nevneren
    nevner = d %>%
      count(qic_x)
    d = d %>%
      left_join(nevner, by = "qic_x") %>%
      arrange(desc(nnn)) %>%
      mutate(qic_x = fct_inorder(qic_x))
    #
    plot = suppressMessages(qic(
      x = qic_x,
      y = qic_y, # telleren i indikatoren
      n = qic_n, # nevneren til indikatoren
      data = d,
      chart = "p", # plottypen
      show.labels = FALSE,
      xlab = NULL,
      ylab = "Andel",
      title = tittel
    ) +
      coord_flip() +
      tema +
      fjern_x +
      fjern_y +
      theme(legend.position = "none") +
      scale_x_discrete(expand = c(0, 0.1)) +
      scale_y_continuous(labels = akse_prosent))
  }

  plot
}



# funksjon for å lage p-chart shewhart-diagram med eller uten facets
# funksjonen krever et datasett (d) som inneholder teller (y) og nevner (n), med variabel for x.aksen (x) som
# ofte er en datovariabel, men hvis det ikke er det settes tidsvinsing til FALSE.
# panel_gruppe er hvilken variabel man ønkser å dele opp et panel på (gjelder kun tidsvisning),
# periode hvilket tidsrom (f.eks "month" eller "2 months", gjelder kun tidsvising) og tittel for tittelen til plottet.
lag_shewhart_xbar = function(d, x, y, yakse_tekst, panel_gruppe, tidsvisning = TRUE, periode, tittel) {
  d$qic_x = d[[x]]
  d$qic_y = d[[y]]
  d$qic_facet = d[[panel_gruppe]]

  if (tidsvisning) {
    plot = suppressMessages(qic(
      x = qic_x,
      y = qic_y, # telleren i indikatoren
      data = d,
      facets = ~qic_facet,
      chart = "xbar", # plottypen
      x.period = periode,
      show.labels = FALSE,
      xlab = NULL,
      ylab = yakse_tekst,
      title = tittel
    ) +
      tema +
      fjern_x +
      fjern_y +
      theme(legend.position = "none") +
      geom_point())
  } else {
    # setter i rekkefølge fra størst til minst i nevneren
    nevner = d %>%
      count(qic_x)
    d = d %>%
      left_join(nevner, by = "qic_x") %>%
      arrange(desc(n)) %>%
      mutate(qic_x = fct_inorder(qic_x))
    plot = suppressMessages(qic(
      x = qic_x,
      y = qic_y, # telleren i indikatoren
      data = d,
      chart = "xbar", # plottypen
      show.labels = FALSE,
      xlab = NULL,
      ylab = yakse_tekst,
      title = tittel
    ) +
      coord_flip() +
      tema +
      fjern_x +
      fjern_y +
      theme(legend.position = "none"))
  }

  plot
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

# funksjon for å lage søylediagram
# d = datasett
# x = variabel for x-aksen - som vanligvis er en kategorisk variabel
# y = variabel for y-aksen - en kontinuerlig variabel. Kan være en prosent.
# farge = fargen på søylene, default er kjedelig SKDE-blå. Kan endres i andre sammenheng
# facet = boolsk, bestemmer om man skal lage panel på en variabel, TRUE, eller ikke FALSE (default)
# facet_gruppe = hvilken variabel som skal brukes for å dele på panel, brukes kun hvis facet = TRUE
# prosent = om y-aksen er en prosent (TRUE) eller ikke (FALSE). Er defaultet som prosent, siden disse er så vanlige.
lag_soyle = function(d, x, y, farge = ColPrim[3], facet = FALSE, facet_gruppe = NULL, prosent = TRUE, ...) {
  x_var = syms(x)[[1]]
  y_var = syms(y)[[1]]

  plott = ggplot(d, aes(x = !!x_var, y = !!y_var)) +
    geom_barh(stat = "identity", fill = farge, width = 2 / 3) +
    xlab(NULL) +
    ylab(NULL) +
    tema +
    fjern_y +
    scale_x_continuous(expand = expand_soyle) +
    fjern_y_ticks
  if (facet) {
    facet_gruppe = syms(facet_gruppe)[[1]]
    plott = plott + facet_wrap(vars(!!facet_gruppe))
    plott
  }
  if (prosent) {
    plott = plott +
      scale_x_continuous(labels = akse_prosent, limits = c(NA, 1), breaks = breaks_bredde(0.1), expand = expand_soyle)
    plott
  }
  plott
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
#      tabell: talet vert brukt i ein tabell og skal derfor ha tabelltekst
#
# Argumentet «tabell» burde vore unødvendig, men siunitx *insisterer*
# på å endra skrifta til \textrm, sjølv om eg har slått på alle moglege
# detect-argument (og prøvd mykje anna, og søkt på nettet etter løysingar
# (bruk søkeorda «siunitx» og «fontspec»)). Alle andre løysingar eg har
# funne gjer at anten vert ikkje rett skrift brukt i brødteksten eller så vert
# ikkje rett tekst brukt i tabellforklaringa eller så vert ikkje rett tekst
# brukt i sjølve tabellen. (Merk at me brukar ulik skrift i tabell-/
# figurforklaringa, sjølv om dei begge er Calibri. Ein ser lettast forskjellen
# ved å studera 1-tala.)
#
# Som ei nødløysing har me ordna det slik at me kan manuelt velja at
# tabellskrifta skal brukast når me kallar num()-funksjonen.
num = function(x, desimalar, tabell = FALSE) {
  # Argument til \num-kommandoen
  arg = NULL
  if (tabell) {
    arg = "text-rm=\\tablefont"
  }

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
      arg = arg %>%
        append(c(
          paste0("round-precision=", desimalar),
          "round-integer-to-decimal=true"
        ))
    }
  }
  # Legg til argumentliste
  argtekst = paste0("[", paste0(arg, collapse = ", "), "]")

  # Returner LaTeX-kode for talformatering. Me legg *heile* kommandoen
  # mellom {} for å hindra problem ved bruk for eksempel inni shortcap-delen
  # av \caption[shortcap]{longcap} (eventuelle ]-teikn vert elles tolka
  # til å avslutta shortcap-argumentet, jf. https://tex.stackexchange.com/a/78416)
  paste0("{\\num", argtekst, "{", format(x, scientific = FALSE), "}}")
}


### Prosent med norsk stavemåte i aksenotasjoner

# fixme: Bør rydda opp i prosentfunksjonane slik at dei alle
#        tar same argument og elles er meir gjennomtenkte
#        (krev gjerne endringar i filene som brukar dei).
#
# Tar inn eit desimaltal og viser det som prosent,
# med mellomrom før prosentteiknet (slik det skal vera
# på norsk), eks. 0.5 --> "50 %", og med komma
# som desimalteikn. Som standard vert tala viste
# med éin desimal, men dette kan ein endra ved
# å spesifisera «accuracy», for eksempel «accuracy = 0.1»
# for éin desimal eller «accuracy = .05» for å runda av til
# næraste halve promille. Bruk «accuracy = NULL» for
# automatisk/«smart» val av desimalar (vanlegvis ikkje tilrådd).
akse_prosent = function(x, accuracy = 1, decimal.mark = ",", ...) {
  scales::percent(x,
    suffix = " %",
    accuracy = accuracy, decimal.mark = decimal.mark, ...
  )
}
# Liknande funksjon for formatering av prosentverdiar som LaTeX-tekst.
prosent = function(x, desimalar = 0, tabell = FALSE) {
  prosent_tekst = x %>%
    map_chr(~ num(100 * .x, desimalar, tabell = tabell) %>%
      str_c("\\prosent"))
  ifelse(is.na(x), "\\textendash{}", prosent_tekst)
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
library(simpleboot)

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
    if (bootstrap) {
      snitt_ki = function(x) {
        n = sum(!is.na(x))
        snitt = mean(x)
        if ((length(x) > 1) & (sd(x) > 0)) {
          b = one.boot(x, mean, R = 9999)
          ki = boot.ci(b, type = "perc")$percent[4:5]
        } else {
          ki = c(NA, NA)
        }
        tibble(
          mean = snitt,
          low = ki[1],
          high = ki[2],
          n = n
        )
      }
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
create_ltable = function(dataframe, label, caption, wide = FALSE, ...) {

  # Viss dataramma ikkje har nokon radar, bryt latex()-funksjonen
  # heilt saman dersom numeric.dollar er FALSE (og det er FALSE
  # me *vil* ha, for å få rett formatering av tal).
  #
  # fixme: Feilen er meldt inn til forfattaren av Hmisc-pakken
  #        i januar 2018, og er lovd retta. Fjern derfor følgjande
  #        if()-test når dette er retta og Hmisc-pakken er oppdatert.
  #
  #        Kan bruka følgjande kodesnutt for å sjekka om feilen
  #        er retta:
  #          latex(head(iris, 0), file="", numeric.dollar=FALSE)
  #        (skal gje tabell, ikkje feilmelding)
  if (nrow(dataframe) == 0) {
    tabell = paste0(
      "\\begin{table}[htbp]\n",
      "\\caption{", caption, "\\label{", label, "}}\n",
      "{\\color{errorcolor}(Tabellen har 0 rader. Her må noko vera gale!)}\n",
      "\\end{table}"
    )
  } else {
    tabell = capture.output(latex(dataframe,
      file = "", center = "centering",
      label = label, caption = caption, rowname = NULL,
      where = "htbp", booktabs = TRUE, numeric.dollar = FALSE, ...
    ))
    if (wide) {
      tabell %<>% str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
    }
    tabell = paste0(tabell, sep = "\n")
  }

  # Returner tabellen (eller feilmelding)
  tabell
}
