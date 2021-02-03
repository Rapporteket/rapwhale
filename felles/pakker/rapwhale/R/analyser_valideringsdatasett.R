analyser_valideringsdatasett = function(d_vld, samanliknar = samanlikn_identisk) {
  # Skal stoppa med feilmelding dersom inndata ikkje er gyldig
  if (!er_valideringsdatasett_gyldig(d_vld)) {
    stop("Datasettet er ikkje på rett format")
  }
}
