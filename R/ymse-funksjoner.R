# Dette skriptet inneholder en rekke funksjoner som er potensielt nyttige
# i registersammenheng og i andre sammenheng.
# Disse funksjonene tilhører ingen annen kategori,
# og er ikke flere enn én i samme kategori, derav navnet ymse.

#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all str_split
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr
NULL
#' Normaliser variabelnamn
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Funksjon for å konvertere variabelnavn til standardformat.
#'
#' @details
#' Standardformatering er "snake_case",
#' hvor "_" blir brukt som skilletegn,
#' og kun små bokstaver benyttes.
#' Om variabelnavnene som blir tatt inn produserer flere like variabelnavn
#' skrevet i "snake_case" vil man motta en advarsel.
#'
#' @note
#' R tillater variabelnavn uten bokstaver,
#' for eksempel "._.".
#' Det gir ikke mening å skrive et slikt variabelnavn i "snake_case".
#' Derfor tillater
# `normaliser_varnamn()`
#' kun variabelnavn som inneholder minst en bokstav.
#'
#' @param x Tekststreng med variabelnavn som skal konverteres.
#'
#' @return
#' Returnerer en tekststreng av samme lengde som `x`,
#' hvor alle elementer er skrevet i "snake_case".
#'
#' @export
#'
#' @examples
#' normaliser_varnamn(
#'   c("hopp og.SprettTest", "SykdomsAktivitet.PasientGlobalSykdomsaktivitet")
#' )
normaliser_varnamn = function(x) {
  stopifnot(is.character(x))

  # Variabelnavn trenger ikke inneholde bokstaver, for eksempel er "." et
  # gyldig variabelnavn. Vi godtar kun variabelnavn som inneholder minst en
  # bokstav, ellers vil navnet som blir spyttet ut være tomt, "".
  inneholder_bokstav = str_detect(x, "[a-zA-Z]")
  if (!all(inneholder_bokstav)) {
    navn_uten_bokstav = x[!inneholder_bokstav]
    indeks_uten_bokstav = which(!inneholder_bokstav)
    navn_uten_bokstav = paste0(
      "\n  (", indeks_uten_bokstav, ") ", navn_uten_bokstav, collapse = ""
    )
    feilmelding = paste0(
      "Alle variabelnavn må inneholde minst en bokstav.",
      "\nFølgende variabelnavn inneholder ingen bokstaver ",
      "(indeks i inndata i parentes):",
      navn_uten_bokstav,
      collapse = ""
    )
    stop(feilmelding)
  }

  teikn = x %>%
    str_split("") # Splitt i enkeltteikn

  # Putt inn _ før alle store bokstavar (utanom første teikn i strengen)
  teikn = teikn %>%
    purrr::map(~ str_replace_all(., "([[:upper:]])", "_\\1"))

  varnavn = teikn %>%
    map_chr(~ paste0(., collapse = "")) %>% # Slå saman til lange strengar igjen
    str_replace_all("[\\._ ]+", "_") %>% # Erstatt etterfølgjande punktum, mellomrom og/eller _ med éin _,
    str_replace_all("^_|_$", "") %>% # Fjern ev. _ på starten og slutten av strengane
    tolower() # Gjer om til små bokstavar

  # Undersøk om to variabelnavn er like
  unike_varnavn = unique(varnavn)
  if (length(unike_varnavn) != length(x)) {
    advarsel = "Utdata inneholder flere like variabelnavn (indeks i parentes): "
    ikke_unike_navn = varnavn[-match(unike_varnavn, varnavn)]
    for (navn in ikke_unike_navn) {
      indeks_ikke_unike_navn = which(varnavn == navn)
      indeks = paste0(indeks_ikke_unike_navn, collapse = ",")
      advarsel = paste0(advarsel, "\n  (", indeks, ") ", navn)
    }
    warning(advarsel)
  }

  varnavn
}

### Variant av table()-funksjonen som tar med NA-verdiar om dei finst

# Lag tabell som også viser NA-verdiar om dei finst

#' Tabell inkludert NA-verdier
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Variant av [base::table()] som tar med NA-verdiar om dei finst.
#' @param ... Tilsvarende argumenter som brukes i [base::table()].
#' @export
#'
#' @examples
#' tab(c(rpois(95, 5), rep(NA, 5)))
tab = function(...) {
  table(..., useNA = "ifany")
}


#' Konfidensintervall for binomisk fordeling
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Brukar Wilson-intervallet, som anbefalt i «Binomial confidence intervals
#' and contingency tests: mathematical fundamentals and the evaluation of
#' alternative methods», av Sean Wallis, University College London.
#'
#' @param x Antall suksesser i forsøket.
#' @param n Antall uavhengige forsøk.
#' @param alfa Én minus nivået til konfidensintervallet.
#'   Standardverdi er 0.05, som tilsvarer et 95 %-konfidensintervall.
#'
#' @return
#' Returnerer en tibble med nedre og øvre grense for et
#' Wilson-konfidensintervall på angitt nivå.
#'
#' @export
#'
#' @examples
#' n_forsok = 1000
#' n_suksess = sample.int(n_forsok, 1)
#' regn_ki_bin(n_suksess, n_forsok)
regn_ki_bin = function(x, n, alfa = 0.05) {
  ki = binom::binom.wilson(x, n, 1 - alfa)
  tibble(
    lower = pmax(0, ki$lower), # Fiks for at grensene av og til kan gå *bitte litt* utanfor [0,1]
    upper = pmin(1, ki$upper)
  )
}



#' Konfidenstinervall basert på gjennomsnittet til en kontinuerlig variabel
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Finner konfidenstinervall basert på gjennomsnittet til en
#' kontinuerlig variabel med mulighet for bootstrap lagt inn i funksjonen.
#'
#' @param x Numerisk vektor man ønsker å lage et konfidensintervall for.
#' @param bootstrap Logisk verdi for om man ønsker å bruke bootstrap-metoden
#' eller ei.
#' Standardverdi er `FALSE`,
#' altså å ikke bruke bootstrap.
#' @param alfa Numerisk verdi som angir konfidensnivået.
#' Standardverdi er 0.05,
#' som tilsvarer et 95 %-konfidensintervall.
#' @param R
#' Antall bootstrap-replikasjoner.
#' Kun i bruk når `bootstrap` er `TRUE`.
#' Standardverdi = 9999.
#'
#' @details
#' Inndatasettet kan enten være en vektor,
#' eller en kolonne i en tibble/dataramme.
#' Om man anvender en tibble/dataramme,
#' kan man for eksempel bruke funksjonen inni [dplyr::summarise()].
#' Hvis inndataene er gruppert,
#' blir resultatet regnet ut på gruppenivå,
#' med én rad per gruppe.
#'
#' Funksjonen ignorerer `NA`-verdier i `x`.
#'
#' @return
#' Returnerer en tibble med nedre og øvre grense for konfidensintervall,
#' samt gjennomsnittsverdien av `x`.
#'
#' @export
#'
#' @examples
#' regn_ki_univar(runif(100))
#'
#' # Konfidensintervall for drivstofforbruk, gruppert på type girkasse
#' library(dplyr)
#'
#' mtcars %>%
#'   group_by(am) %>%
#'   summarise(regn_ki_univar(mpg))
regn_ki_univar = function(x, bootstrap = FALSE, alfa = 0.05, R = 9999) {
  # Fjern eventuelle NA-verdier
  x = x[!is.na(x)]

  # Hvis det er for få eller for lite varierende
  # observasjoner til å regne ut konfidensintervall,
  # returner NA for konfidensintervallene
  if ((length(x) < 2) | (sd(x) == 0)) {
    tibble::tibble(
      low = NA_real_,
      mean = mean(x),
      high = NA_real_
    )
  } else {
    if (bootstrap) { # Hvis man ønsker boostrap kjøres denne koden,
      ki_bootstrap = function(x, alfa) {
        snitt_stat = function(x, idx) {
          data = x[idx]
          mean(data)
        }
        bootstrap = boot::boot(x, snitt_stat, R = R)
        konfint = boot::boot.ci(
          boot.out = bootstrap,
          conf = 1 - alfa,
          type = "perc"
        )
        ki = konfint$percent
        tibble::tibble(
          low = ki[4],
          mean = mean(x),
          high = ki[5]
        )
      }
      ki_bootstrap(x, alfa)
    } else {
      mod = t.test(x, conf.level = 1 - alfa)
      tibble::tibble(
        low = mod$conf.int[1],
        mean = unname(mod$estimate),
        high = mod$conf.int[2]
      )
    }
  }
}


#' Lag LaTeX-tabell
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Lager pene LaTeX-tabeller i et standardisert format for alle årsrapporter,
#' med mulighet for å gjøre den stor nok til å dekke hele siden (`wide = TRUE`).
#'
#' @param dataframe Dataramme som inneholder data som skal vises i tabell.
#' @param label Label for tabellen som brukes for å lage referanser i teksten.
#' @param caption Forklaringstekst for tabellen.
#' @param wide Valgmulighet for om tabellen skal være breiere enn
#' standard tekstbredde.
#' Må være `TRUE` eller `FALSE`,
#' standard er `FALSE`.
#' @param ... Ytterligere argumenter som kan gis til [Hmisc::latex()].
#'
#' @details
#' Ytterligere argumenter som kan gis til [Hmisc::latex()] inkluderer
#' blant annet `colhead = c()`.
#'
#' @return
#' Tekst som, i en LaTeX-kompilator, genererer en tabell.
#'
#' @export
#'
#' @examples
#' # Pakke for bruk av tibble-objekt og rør-operatoren
#' library(dplyr)
#'
#' cars_top_mpg = mtcars %>%
#'   arrange(desc(mpg)) %>%
#'   head()
#' cars_top_mpg_tab = lag_tab_latex(cars_top_mpg,
#'   label = "mpg_table",
#'   caption = "Cars with best mileage"
#' )
#' cat(cars_top_mpg_tab)
lag_tab_latex = function(dataframe, label, caption, wide = FALSE, ...) {

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
    tabell = capture.output(Hmisc::latex(dataframe,
      file = "", center = "centering",
      label = label, caption = caption, rowname = NULL,
      where = "htbp", booktabs = TRUE, numeric.dollar = FALSE, ...
    ))
    if (wide) {
      tabell = tabell %>%
        stringr::str_replace("^\\\\(begin|end)\\{table\\}", "\\\\\\1\\{widetable\\}") # Superrobust ... ;)
    }
    tabell = paste0(tabell, sep = "\n")
  }

  # Returner tabellen (eller feilmelding)
  tabell
}

#' Lag statistisk samandragsfunksjon med trunkerte grenser for konfidensintervall
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Lagar ein statistisk samandragsfunksjon tilsvarande
#' [ggplot2::mean_cl_normal()] der grensene for konfidensintervallet
#' vert trunkerte.
#'
#' @param ymin Nedre grense for konfidensintervall. Standard verdi `NA`.
#' @param ymax Øvre grense for konfidensintervall. Standard verdi `NA`.
#'
#' @details
#' Tek inn ei nedre grense `ymin` og ei øvre grense `ymax` og gjev ut ein
#' funksjon tilsvarande [ggplot2::mean_cl_normal()] der grensene for
#' konfidensintervallet er trunkerte til desse verdiane.
#'
#' Dersom `ymin` og/eller `ymax` er `NA`, vert ikkje tilhøyrande grenser
#' trunkerte.
#'
#' Denne funksjonen kan nyttast til å laga trunkerte konfidensintervall
#' viss inndata har kjende nedre og/eller øvre verdigrenser.
#'
#' Funksjonen er også laga for å brukast i [ggplot2::stat_summary()].
#'
#' @return
#' Ein funksjon tilsvarande [ggplot2::mean_cl_normal()], men der grensene for
#' konfidensintervallet vert trunkerte
#' @export
#'
#' @examples
#' # Pakke for å laga figurar
#' library(ggplot2)
#'
#' # Vektorar med eksempel på svar frå undersøkjing
#'
#' # På ein skala frå 1-5, kor godt nøgd er du med x?
#' x = c(5, 5, 4, 5)
#' # På ein skala frå 1-5, kor godt nøgd er du med y?
#' y = c(1, 2, 1, 1)
#'
#' mean_cl_normal(x)
#' mean_cl_normal_truncated(ymin = 1, ymax = 5)(x)
#'
#' d = tibble::tibble(spm = rep(c("x", "y"), each = 4), verdi = c(x, y))
#' p = ggplot(d, aes(spm, verdi)) +
#'   geom_point(position = position_dodge2(width = 0.05))
#'
#' p + stat_summary(fun.data = mean_cl_normal, size = 1, color = "red")
#' p + stat_summary(fun.data = mean_cl_normal_truncated(ymin = 1, ymax = 5), size = 1, color = "red")
mean_cl_normal_truncated = function(ymin = NA, ymax = NA) {
  function(x, ...) {
    res = ggplot2::mean_cl_normal(x, ...)
    res$ymin = pmax(ymin, res$ymin, na.rm = TRUE)
    res$ymax = pmin(ymax, res$ymax, na.rm = TRUE)
    res
  }
}
