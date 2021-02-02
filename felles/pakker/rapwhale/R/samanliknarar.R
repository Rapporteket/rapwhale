samanlikn_identisk = function(varnamn, verdi1, verdi2) {
  # Utdata skal seia (elementvis) om verdi1 er lik verdi2 og skal handtera NA-verdiar
  like = (verdi1 == verdi2) | (is.na(verdi1) & is.na(verdi2))
  like[is.na(like)] = FALSE
  return(like)
}
