entall_flertall = function(x, entall, flertall, nulltekst = "0") {
  if (!is.character(entall) ||
    !is.character(flertall) ||
    !is.character(nulltekst) ||
    length(entall) != 1 ||
    length(flertall) != 1 ||
    length(nulltekst) != 1) {
    stop(
      "Argumentene 'entall', 'flertall' og 'nulltekst' må alle være ",
      "av klasse character og av lengde 1"
    )
  }
  if (length(x) == 0) {
    stop("Ingen inndata")
  }
  if (!is.numeric(x)) {
    stop(paste("Inndata må være tall (heltall/flyttall), men er:", class(x)))
  }
  if (NA %in% x) {
    stop("Inndata inneholder minst en NA-verdi")
  }
  if (!all(x >= 0)) {
    stop("Inndata inneholder minst en negativ verdi")
  }

  tall_tekst = ifelse(x == 0, nulltekst, x)
  enhet = ifelse(x == 1, entall, flertall)
  paste(tall_tekst, enhet)
}
