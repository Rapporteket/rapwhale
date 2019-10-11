
#' @importFrom dplyr group_by summarise case_when
#' @importFrom rlang sym !!
#' @importFrom tibble as_tibble
{ # lage et test-sett:
  # random string funksjon.
  # rnd_ID <- function(n = 5000) {
  #   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  #   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  # }

  { # lag_d = function(n = 1000, seed = 123, char_kol = TRUE, num_kol = TRUE, log_kol = TRUE, gruppe_var = TRUE){
    #   set.seed(seed)
    #   d = tibble(PasientID = rnd_ID(n), ki_krit_nevner = rbinom(n, size = 1, prob = 0.7), ki_krit_teller = c(rep(0, n)))
    #
    #   if(char_kol){
    #     kol_char = rnd_ID(n)
    #     d = cbind(d, kol_char)
    #   }
    #   if(num_kol){
    #     kol_num = rnorm(n, 100, 10)
    #     d = cbind(d, kol_num)}
    #   if(log_kol){
    #     kol_log = as.logical(rbinom(n, 1, 0.7))
    #     d = cbind(d, kol_log)}
    #
    #   if(gruppe_var){
    #     gruppe = factor(round(runif(n, 1, 4), 0), levels = c(1:5))
    #     d = cbind(d, gruppe)
    #   }
    #
    #   # Legger inn at nevner kan være lik 0, teller kan være missing HVIS nevner er lik 0.
    #   n_nevner = sum(d$ki_krit_nevner == 0)
    #   n_nevn_1 = sum(d$ki_krit_nevner == 1)
    #   find_na = rbinom(n_nevner, 1, prob = 0.15)
    #   find_na[find_na == 1] <- NA_integer_
    #
    #   find_1 = rbinom(n_nevn_1, 1, prob = 0.8)
    #
    #   d$ki_krit_teller[d$ki_krit_nevner == 0] <- find_na
    #   d$ki_krit_teller[d$ki_krit_nevner == 1] <- find_1
    #
    #   d = as_tibble(d)
    #   attr(d, "ki_navn") <- c("Andel_slips")
    #   attr(d, "ki_andel") <- c("Andelen som bruker slips når de ser på tv")
    #   attr(d, "ki_maal") <- c("Vi målte dette ved hjelp av VAR-teknologi")
    #   d
    # }
    #
    #
    # d_ki_ind = d_uten_teller
  }
}

#' Regn ut Kvalitetsindikator - Andel:
#'
#' Funksjon for å regne ut kvalitetsindikatorer for andeler.
#' Tar inn et datasett på 101-format, og returnerer et estimert resultat
#'
#'
#' @param d_ki_ind Inndata på 101-format
#' @param alpha Verdi for å bestemme bredde på konfidensintervall, default er 0.05
#' @export
aggreger_ki_prop = function(d_ki_ind, alpha = 0.05, gruppering = NULL) {

  # "Inndata må ha både 'ki_krit_teller' og 'ki_krit_nevner'"
  if (!(exists("ki_krit_nevner", d_ki_ind) & exists("ki_krit_teller", d_ki_ind))) {
    stop("Inndata må ha både 'ki_krit_teller' og 'ki_krit_nevner'")
  }

  if (is.numeric(c(d_ki_ind$ki_krit_nevner, d_ki_ind$ki_krit_teller))) {
    "Kriterievariablene må være tall"
  }

  # Sjekk at kriterie-variabler er kun gyldige verdier (0,1,NA):
  if (any(!d_ki_ind$ki_krit_teller %in% c(0, 1, NA)) |
    any(!d_ki_ind$ki_krit_nevner %in% c(0, 1))) {
    stop("kriterie variablene inneholder ugyldige verdier")
  }

  # Sjekke at det ikke finnes observasjoner hvor teller_krit er oppfylt når nevner_krit ikke er oppfylt.
  stopifnot(all(d_ki_ind$ki_krit_teller <= d_ki_ind$ki_krit_nevner |
    is.na(d_ki_ind$ki_krit_teller)))

  # regn ki:
  d_agg_prop =
    d_ki_ind %>%
    {
      if (!is.null(gruppering)) {
        group_by(., !!sym(gruppering), .drop = FALSE) # .drop = FALSE bestemmer om tomme grupper som finnes i faktor-level skal inkluderes i utdata.
      } else {
        .
      }
    } %>%
    summarise(
      est = case_when(
        sum(ki_krit_nevner) == 0 ~ 0,
        TRUE ~ sum(ki_krit_teller, na.rm = TRUE) / sum(ki_krit_nevner)
      ),
      konfint_nedre = est - qnorm(1 - (alpha / 2)) * sqrt(est * (1 - est) / nrow(d_ki_ind)),
      konfint_ovre = est + qnorm(1 - (alpha / 2)) * sqrt(est * (1 - est) / nrow(d_ki_ind)),
      ki_teller = sum(ki_krit_teller, na.rm = TRUE),
      ki_nevner = sum(ki_krit_nevner)
    )

  as_tibble(d_agg_prop)
}


# library(testthat)
# test_adr = "felles\\pakker\\rapwhale\\tests\\testthat\\regn_ki_andel_tester.R"
# test_file(test_adr, reporter="minimal") # *Veldig* kort og konsist samandrag
# test_file(test_adr, reporter="check")   # 13-linjes samandrag
# test_file(test_adr, reporter="summary") # Alt, med fint samandrag i starten
# test_file(test_adr)
