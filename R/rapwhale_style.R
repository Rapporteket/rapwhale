#' Rapwhale-kodestil
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Formater kode etter Rapwhale-kodestilen.
#'
#' @param scope
#' Omfanget av manipulasjon. Kan variera frå `"none"` (minst inngripande) til
#' `"tokens"` (mest inngripande). Sjå 'Details'.
#' Dette argumentet er ein tekststreng eller ein vektor av klassen `AsIs`.
#' @param strict
#' Ein logisk verdi som indikerer om eit sett med strenge eller ikkje så strenge
#' transformasjonsfunksjonar skal returnerast.
#' Samanlikn funksjonane returnert med eller utan `strict = TRUE`.
#' Til dømes vil det med `strict = TRUE` tvingast eitt mellomrom etter ",",
#' og eitt linjeskift etter avsluttande krøllparentes.
#' Med `strict = FALSE` vil det leggjast til mellomrom og linjeskift viss det
#' ikkje finst, men ikkje endrast noko viss det finst fleire. Sjå Examples'.
#' @param indent_by
#' Kor mange mellomrom med innrykk skal setjast inn etter operatorar som `(`.
#' @param start_comments_with_one_space
#' Skal kommentarar starta med berre eitt mellomrom
#' (sjå [styler::start_comments_with_space()]).
#' @param reindention
#' Ei liste over parametrar for regex re-innrykk, mest føremålstenleg laga med
#' [styler::specify_reindention()].
#' @param math_token_spacing
#' Ei liste over parametrar som definerar mellomrom i kring matematiske teikn,
#' føremålstenleg laga med [styler::specify_math_token_spacing()].
#'
#' @details
#' Rapwhale-kodestilen er lik Tidyverse-stilen [styler::tidyverse_style()],
#' med ei lita endring:
#' Den brukar `=`-operatoren i staden for `<-`-operatoren.
#'
#' Fylgjande nivå for `scope` er tilgjengelege:
#' - "none": Utfører ingen transformasjon.
#' - "spaces": Manipulerar mellomrom mellom teikn på same linje.
#' - "indention": Manipulerar innrykk, dvs. tal på mellomrom i starten av
#' kvar linje.
#' - "line_breaks": Manipulerar linjeskift.
#' - "tokens": Manipulerar tokens (symbol og teikn).
#'
#' `scope` kan spesifiserast på to måtar:
#' - Som ein tekststreng: I dette tilfellet er alle mindre inngripande
#' `scope`-nivå underforstått,
#' t.d. "line_breaks" inkluderar "indention" og "spaces".
#' Dette er kort og det ein oftast treng.
#' - Som ein vektor av klassen `AsIs`:
#' Kvart nivå må listas opp eksplisitt ved å leggja eitt eller fleire nivå
#' inn i I().
#'
#' Sjå 'Examples' for detaljar.
#'
#' @return
#' Stilguide, ei liste med funksjonar og annan info,
#' til bruk ved formatering av kode med `styler`-pakken.
#'
#' @export
#'
#' @examples
#' library(styler)
#'
#' style_text("call( 1)", style = rapwhale_style, scope = "spaces")
#' style_text("call( 1)", transformers = rapwhale_style(strict = TRUE))
#' style_text(c("ab = 3", "a  =3"),
#'   style = rapwhale_style,
#'   strict = FALSE
#' ) # Tek vare på justering av "="
#' style_text(c("ab = 3", "a  =3"),
#'   style = rapwhale_style,
#'   strict = TRUE
#' ) # Fjernar justering av "="
#'
#' # Endrar linjeskift og symbol, men ikkje mellomrom
#' style_text(c("ab <- 3", "a =3"),
#'   style = rapwhale_style,
#'   strict = TRUE, scope = I(c("line_breaks", "tokens"))
#' )
rapwhale_style = function(
    scope = "tokens",
    strict = TRUE,
    indent_by = 2,
    start_comments_with_one_space = FALSE,
    reindention = styler::tidyverse_reindention(),
    math_token_spacing = styler::tidyverse_math_token_spacing(),
    maks_blanke_linjer = 3L) {
  # Ta utgangspunkt i tidyverse-stilen
  temp_style = styler::tidyverse_style(
    scope = scope, strict = strict, indent_by = indent_by,
    start_comments_with_one_space = start_comments_with_one_space,
    reindention = reindention, math_token_spacing = math_token_spacing
  )

  temp_style$style_guide_name = "rapwhale::rapwhale_style()"
  temp_style$style_guide_version = packageVersion("rapwhale") |>
    as.character()

  # Bruk = i staden for <-
  if ("tokens" %in% scope) {
    temp_style$token$force_assignment_op = NULL
    temp_style$token$force_equals_op = function(pd) {
      to_replace = pd$token == "LEFT_ASSIGN" & !(pd$text %in% c("<<-", ":="))
      pd$token[to_replace] = "EQ_ASSIGN"
      pd$text[to_replace] = "="
      pd
    }
    temp_style$transformers_drop$token$force_assignment_op = NULL
    temp_style$transformers_drop$token$force_equals_op = "LEFT_ASSIGN"
  }

  # if, else osv. skal alltid ha krøllparentesar
  temp_style$token = temp_style$token |>
    append(c(
      wrap_if_else_while_for_fun_in_curly_rapwhale =
        wrap_if_else_while_for_fun_in_curly_rapwhale
    ), after = 0)

  # Alltid linjeskift etter røyr, også med berre eitt røyr
  temp_style$line_break$add_line_break_after_pipe =
    add_line_break_after_pipe_rapwhale

  # Tillat maks tre blanke linjer på rad
  temp_style$line_break$set_line_breaks_between_top_level_exprs = function(pd, allowed_blank_lines = maks_blanke_linjer) {
    styler:::set_line_breaks_between_top_level_exprs(pd,
      allowed_blank_lines = allowed_blank_lines
    )
  }

  temp_style
}
