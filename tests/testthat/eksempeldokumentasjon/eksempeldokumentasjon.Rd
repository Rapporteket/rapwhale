\name{utviklingsnivaa}
\alias{utviklingsnivaa}
\title{Lag oversikt over utviklingsnivå}
\usage{
utviklingsnivaa(mappe = "man")
}
\arguments{
\item{mappe}{Adresse til mappe med hjelpefiler. Standard er "man".}
}
\value{
Tibble med kolonnane \code{funksjon} og \code{utviklingsnivaa} som gjev ein
oversikt over utviklingsnivåa til alle funksjonane som har
hjelpefil i \code{mappe}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Sjekkar alle .Rd-filer i ei mappe, og lagar ein oversikt over utviklingsnivåa.
}
\details{
Funksjonen tek inn adressa til ei mappe, og ser etter utviklingsnivå
(\code{lifecycle}-badge) i alle .Rd-filene i mappa. Den gjev så ut ein oversikt
over alle funksjonsnamn og utviklingsnivå i en tibble.
}
\examples{
utviklingsnivaa()

utviklingsnivaa("H:\\\\kvalreg\\\\fellesting\\\\r-pakkar\\\\rapwhale\\\\man")
}
