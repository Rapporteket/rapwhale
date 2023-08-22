# Testar for regn_ki_univar()

test_that("regn_ki_univar() gjev forventa resultat", {
  sepal_length_resultat = regn_ki_univar(iris$Sepal.Length)
  sepal_length_forventa = tibble::tibble(
    low = 5.70973248150668855060985151794739067554473876953125,
    mean = 5.843333333333333712289459072053432464599609375,
    high = 5.97693418515997887396906662615947425365447998046875
  )
  expect_identical(sepal_length_resultat, sepal_length_forventa)
})

test_that("regn_ki_univar() funkar med grupperte inndata", {
  sepal_length_gruppert_resultat = iris %>%
    group_by(Species) %>%
    summarise(regn_ki_univar(Sepal.Length))
  sepal_length_gruppert_forventa = tibble::tibble(
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

test_that("regn_ki_univar() ignorerer NA-verdiar i inndata", {
  expect_identical(
    object = regn_ki_univar(alle_na),
    expected = regn_ki_univar(numeric())
  )
  expect_identical(
    object = regn_ki_univar(ein_pluss_na),
    expected = regn_ki_univar(ein_pluss_na[!is.na(ein_pluss_na)])
  )
  expect_identical(
    object = regn_ki_univar(fleire_pluss_na),
    expected = regn_ki_univar(fleire_pluss_na[!is.na(fleire_pluss_na)])
  )
})

test_that("regn_ki_univar() med bootstrapping gjev ikkje feilmelding med NA-verdiar i inndata", {
  expect_error(regn_ki_univar(alle_na, bootstrap = TRUE), NA)
  expect_error(regn_ki_univar(ein_pluss_na, bootstrap = TRUE), NA)
  expect_error(regn_ki_univar(fleire_pluss_na, bootstrap = TRUE), NA)

  # Seed som i utgangspunktet gav feilmelding med bootstrap
  set.seed(11111)
  to_pluss_na = c(1, 2, rep(NA_real_, 5))
  expect_error(regn_ki_univar(to_pluss_na, bootstrap = TRUE), NA)
})
