test_that("utviklingsnivaa() har utviklingsniv√• experimental", {
  nivaa = utviklingsnivaa() %>%
    filter(funksjon == "utviklingsnivaa()") %>%
    pluck(2)
  expect_identical(nivaa, "experimental")
})
