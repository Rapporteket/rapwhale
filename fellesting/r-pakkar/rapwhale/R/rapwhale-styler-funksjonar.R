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
