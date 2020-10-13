# Generelle funksjoner for å teste logikker

# Sann hvis og bare hvis 'a' sann impliserer 'b' sann
# (og håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
# aldri NA)
impl = function(a, b) {
  (is.na(a) | !a) | tidyr::replace_na(b, FALSE) # eg. (!a | b), men håndterer NA
}

# Hvis og bare hvis (som håndterer NA)
dobbelimpl = function(a, b) {
  impl(a, b) & impl(b, a)
}
