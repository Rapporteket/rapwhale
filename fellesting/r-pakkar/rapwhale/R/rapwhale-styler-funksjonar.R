#' Wrap a statement in curly braces
#'
#' @description
#' Wrap a statement in curly braces.
#'
#' @param pd
#' A parse table.
#' @param indent_by
#' The amount of spaces used to indent an expression in curly braces.
#' Used for unindention.
#' @param space_after
#' How many spaces should be inserted after the closing brace.
#' @param key_token
#' The token that comes right before the token that contains the expression
#' to be wrapped (ignoring comments).
#' For if and while loops, this is the closing "')'",
#' for a for-loop it's "forcond".
#'
#' @details
#' Lik [styler::wrap_multiline_curly],
#' men der alle uttrykk får krøllparentesar (ikkje berre multi-line).
wrap_curly_rapwhale = function(pd,
                               indent_by,
                               space_after = 1,
                               key_token) {
  to_be_wrapped_expr_with_child = styler:::next_non_comment(
    pd,
    which(pd$token == key_token)[1]
  )
  next_terminal = styler:::next_terminal(
    pd[to_be_wrapped_expr_with_child, ]
  )$text
  requires_braces = if_for_while_part_requires_braces_rapwhale(pd, key_token) &&
    !any(pd$stylerignore)
  if (requires_braces | next_terminal == "return") {
    closing_brace_ind = which(pd$token == key_token)[1]
    pd$spaces[closing_brace_ind] = 1L
    all_to_be_wrapped_ind = rlang::seq2(
      closing_brace_ind + 1L,
      to_be_wrapped_expr_with_child
    )
    pd = styler:::wrap_subexpr_in_curly(
      pd,
      all_to_be_wrapped_ind,
      indent_by,
      space_after
    )
    if (nrow(pd) > 5) {
      pd$lag_newlines[6] = 0L
    }
  }
  pd
}

#' Check if if, for or while loop expression require braces
#'
#' @description
#' This is the case if they are not yet wrapped into curly braces.
#'
#' @param pd
#' A parse table.
#' @param key_token
#' The token that comes right before the token that contains the expression
#' to be wrapped (ignoring comments).
#' For if and while loops, this is the closing "')'",
#' for a for-loop it's "forcond".
#'
#' @details
#' Lik [styler::if_for_while_part_requires_braces],
#' men der alle uttrykk krev krøllparentesar (ikkje berre multi-line).
if_for_while_part_requires_braces_rapwhale = function(pd, key_token) {
  pos_first_key_token = which(pd$token == key_token)[1]
  child = pd$child[[styler:::next_non_comment(pd, pos_first_key_token)]]
  !styler:::is_curly_expr(child)
}
