#' Tema for bruk i figurer
#'
#' @description 
#' Inneholder temainnstillinger som skal brukes i figurer 
#' i årsrapporter.
#' 
#' @return Objektet `tema` som inneholder temainnstillinger.
#' @export
tema_kvalreg = function() {

# ggplot2-tema for figurar
if(!exists("skriftstorleik")) # Skriftstorleik bør vera definert i kvar årsrapportfil
   skriftstorleik = 9
tema = theme_light(base_size=skriftstorleik)
tema$panel.grid.minor$colour="white"
tema$strip.background$fill="#f3f1ee"
tema$strip.background$colour="#e4e0da"
tema$strip.text.x = element_text(colour="black")
tema$panel.spacing=unit("13" ,"pt")
tema$panel.border$colour=tema$strip.background$colour
tema$panel.grid.major$colour=tema$strip.background$colour
tema$panel.grid.minor$colour=tema$strip.background$fill
tema$axis.title.y$angle=0
tema$axis.title.y$margin=margin(r=5)
tema$axis.title.x$margin=margin(t=5)

# Fjern luft til venstre for y-akseteksten og legg
# til ekstra luft til høgre for han, fjern luft under
# x-akseteksten og legg til ekstra luft over han,
# fjern nesten all luft rundt figurane (eventuell
# nødvendig luft legg me til via LaTeX).
#
# (Merk: Me set den ytre margen til 3 punkt
# opp og nede i staden for 0 punkt for å sikra at at
# kantane på alle bokstavane alltid vert med.)
tema$axis.title.y$margin=margin(r=tema$text$size/2)
tema$axis.title.x$margin=margin(t=tema$text$size/2)
tema$plot.margin=margin(3, 3, 3, 3)

tema
}
#' Temainstillinger for figurer 
#' 
#' @description 
#' Disse funksjonene brukes for å gjøre mindre endringer for panellinjer og 
#' strekmarkeringer på aksene i figurer. 
#' De krever ingen argumenter og legges til direkte i et ggplot-kall. 
#' 
#' @details 
#' Funksjoner som er inkludert her: 
#' - `fjern_x()`: Fjerner panellinjer på x-aksen.   
#' - `fjern_y()`: Fjerner panellinjer på y-aksen.
#' - `fjern_x_ticks()`: Fjerner strekmarkeringer på x-aksen.  
#' - `fjern_y_ticks()`: Fjerner strekmarkeringer på y-aksen.  
#' 
#' Fjerning av panellinjer kan være nyttig for å redusere støy i figurer 
#' hvor det ikke er like stort behov for følgelinjer på begge aksene.  
#' Fjerning av strekmarkeringer kan være nyttig for søylediagram med 
#' kategoriske verdier, der strekmarkeringen er unødvendig.
#' Se eksempel under før hvordan figurene blir seende ut med de ulike 
#' funksjonene. 
#' 
#' @return 
#' Funksjonene returnerer et theme-objekt som kan brukes direkte i et ggplot-kall. 
#' 
#' @examples   
#' p = ggplot(iris, aes(x = Petal.Width, y = Petal.Length)) +
#' geom_point()
#' 
#' Eksempel på hva de ulike funksjonene gjør 
#' p
#' p + fjern_x()
#' p + fjern_y()
#' p + fjern_x_ticks()
#' p + fjern_y_ticks()
#' 
#' # Flere av funksjonene kan brukes samtidig
#' p + fjern_x() + 
#' fjern_y_ticks()
#' 
#' @export
fjern_x = function() {
fjern_x = theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank())
}

#' @rdname fjern_x
#' @export
fjern_y = function() {
fjern_y = theme(panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank())
}

#' @rdname fjern_x
#' @export
fjern_x_ticks = function() {
fjern_x_ticks = theme(axis.ticks.x = element_blank())
}

#' @rdname fjern_x
#' @export
fjern_y_ticks = function() {
fjern_y_ticks = theme(axis.ticks.y = element_blank())
}

#' Juster søylediagram
#' 
#' @description 
#' Disse funksjonene brukes for å justere plassering av søyler i søylediagram. 
#' Søylene starter helt inntil aksen og får litt luft i enden av søylen. 
#'  
#' @details 
#' - `expand_soyle()`: Brukes for å flytte start av søyle helt 
#' inn til aksen og legger til litt luft i enden av søylen. 
#' - `expand_soyle_str_fig()`: Brukes i de tilfeller der det ikke 
#' er tilstrekkelig plass til tekst-label i plot. Gir litt større 
#' plass i enden av søylene. 
#' 
#' @return Returnerer en tall-vektor som brukes som expand-argument i 
#' scale_* funksjoner.
#' 
#' @examples  
#' 
#' library(tidyverse)
#' p = ggplot(mtcars, aes(x = cyl, y = mpg)) +
#' geom_col()
#' 
#' # Default plot har ikke start av søyle helt inntil aksen
#' p
#' p + scale_y_continuous(expand = expand_soyle())
#' 
#' # Legge til mer rom i enden av søylene for tekst-labels
#' p = mtcars %>% 
#' ggplot(aes(x = cyl)) +
#' geom_bar() +
#' geom_text(stat = 'count', aes(x = cyl, label =..count..), vjust=-1)
#' 
#' p
#' p + scale_y_continuous(expand = expand_soyle_str_fig)
#' @export
expand_soyle = function() {
  expand_soyle = expansion(mult = c(0.0, .05), add = 0)
}

# I noen tilfeller er det ikke tilstrekkelig plass for tekst-label i plot.
# Da kan vi bruke expand-argumentet under.
#' @rdname expand_soyle
#' @export
expand_soyle_str_fig_ny = function() {
  expand_soyle_str_fig = expansion(mult = c(0.0, .09), add = 0)
}

# expand_soyle_str_fig = expansion(mult = c(0.0, .09), add = 0)
