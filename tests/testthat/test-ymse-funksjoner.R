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
