test_that("utviklingsnivaa() har utviklingsnivå experimental", {
  nivaa = utviklingsnivaa() %>%
    filter(funksjon == "utviklingsnivaa()") %>%
    pluck(2)
  expect_identical(nivaa, "experimental")
})
