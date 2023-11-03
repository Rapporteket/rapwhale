# Testar for regn_konfint_univar()

test_that("Gir NA-verdier i ki om for få elementer eller for lav varians", {
  x_ingen_element = numeric()
  x_fa_element = 1
  x_lav_varians = rep(1, 4)

  fasit_ingen_element = tibble(
    low = NA_real_,
    mean = NaN,
    high = NA_real_
  )
  fasit_fa_element = tibble(low = NA_real_, mean = 1, high = NA_real_)

  expect_identical(regn_konfint_univar(x_ingen_element), fasit_ingen_element)
  expect_identical(regn_konfint_univar(x_fa_element), fasit_fa_element)
  expect_identical(regn_konfint_univar(x_lav_varians), fasit_fa_element)
})

test_that("regn_konfint_univar() gjev forventa resultat", {
  sepal_length_resultat = regn_konfint_univar(iris$Sepal.Length)
  sepal_length_forventa = tibble(
    low = 5.70973248150668855060985151794739067554473876953125,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.97693418515997887396906662615947425365447998046875
  )
  expect_identical(sepal_length_resultat, sepal_length_forventa)
})

test_that("regn_konfint_univar() gjev forventa resultat ved val av konfidensnivå", {
  sepal_length_konf_resultat = regn_konfint_univar(iris$Sepal.Length,
    konf_niva = 0.9
  )
  sepal_length_konf_forventa = tibble(
    low = 5.731426832856339359523190069012343883514404296875,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.955239833810328065055728075094521045684814453125
  )
  expect_identical(sepal_length_konf_resultat, sepal_length_konf_forventa)
})

test_that("regn_konfint_univar() funkar med grupperte inndata", {
  sepal_length_gruppert_resultat = iris %>%
    group_by(Species) %>%
    summarise(regn_konfint_univar(Sepal.Length))
  sepal_length_gruppert_forventa = tibble(
    Species = as.factor(c("setosa", "versicolor", "virginica")),
    low = c(
      4.90582353929926373581338339135982096195220947265625,
      5.7893057831068261975815403275191783905029296875,
      6.40728501911750303321468891226686537265777587890625
    ),
    mean = c(
      5.006000000000000227373675443232059478759765625,
      5.93599999999999994315658113919198513031005859375,
      6.58800000000000007815970093361102044582366943359375
    ),
    high = c(
      5.10617646070073671893396749510429799556732177734375,
      6.0826942168931736887316219508647918701171875,
      6.76871498088249712310471295495517551898956298828125
    )
  )
  expect_identical(
    object = sepal_length_gruppert_resultat,
    expected = sepal_length_gruppert_forventa
  )
})

alle_na = rep(NA_real_, 5)
ein_pluss_na = c(1, rep(NA_real_, 5))
fleire_pluss_na = c(1, 0.9, 0.8, 1.05, 1.1, 0.95, 1, rep(NA_real_, 5))

test_that("regn_konfint_univar() ignorerer NA-verdiar i inndata", {
  expect_identical(
    object = regn_konfint_univar(alle_na),
    expected = regn_konfint_univar(numeric())
  )
  expect_identical(
    object = regn_konfint_univar(ein_pluss_na),
    expected = regn_konfint_univar(ein_pluss_na[!is.na(ein_pluss_na)])
  )
  expect_identical(
    object = regn_konfint_univar(fleire_pluss_na),
    expected = regn_konfint_univar(fleire_pluss_na[!is.na(fleire_pluss_na)])
  )
})

test_that("regn_konfint_univar() med bootstrapping gjev forventa resultat", {
  set.seed(12345)
  sepal_length_boot_resultat = regn_konfint_univar(iris$Sepal.Length,
    bootstrap = TRUE
  )
  sepal_length_boot_forventa = tibble(
    low = 5.71266666666666633744853243115358054637908935546875,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.97866666666666635165938714635558426380157470703125
  )
  expect_identical(sepal_length_boot_resultat, sepal_length_boot_forventa)
})

test_that("regn_konfint_univar() med bootstrapping gjev forventa resultat ved val av konfidensnivå", {
  set.seed(55555)
  sepal_length_boot_konf_resultat = regn_konfint_univar(iris$Sepal.Length,
    bootstrap = TRUE,
    konf_niva = 0.9
  )
  sepal_length_boot_konf_forventa = tibble(
    low = 5.73333333333333339254522798000834882259368896484375,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.9546666666666663303431050735525786876678466796875
  )
  expect_identical(
    object = sepal_length_boot_konf_resultat,
    expected = sepal_length_boot_konf_forventa
  )
})

test_that("regn_konfint_univar() med bootstrapping gjev forventa resultat ved val av tal replikasjonar", {
  set.seed(247385)
  sepal_length_boot_repl_resultat = regn_konfint_univar(iris$Sepal.Length,
    bootstrap = TRUE,
    R = 500
  )
  sepal_length_boot_repl_forventa = tibble(
    low = 5.7243555705374244979566356050781905651092529296875,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.9787553907549995102499451604671776294708251953125
  )
  expect_identical(
    object = sepal_length_boot_repl_resultat,
    expected = sepal_length_boot_repl_forventa
  )
})

test_that("regn_konfint_univar() med bootstrapping gjev ikkje feilmelding med NA-verdiar i inndata", {
  expect_no_error(regn_konfint_univar(alle_na, bootstrap = TRUE))
  expect_no_error(regn_konfint_univar(ein_pluss_na, bootstrap = TRUE))
  expect_no_error(regn_konfint_univar(fleire_pluss_na, bootstrap = TRUE))

  # Seed som i utgangspunktet gav feilmelding med bootstrap
  set.seed(11111)
  to_pluss_na = c(1, 2, rep(NA_real_, 5))
  expect_no_error(regn_konfint_univar(to_pluss_na, bootstrap = TRUE))
})
