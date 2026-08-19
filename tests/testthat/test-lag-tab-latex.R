# Testing av lag_tab_latex() ---------------------------------------------

d_eks = head(mtcars[, 1:3], 3)
d_tom = d_eks[0, ]

test_that("lag_tab_latex() lagar ein vanleg tabell for ikkje-tomme data", {
  tab = lag_tab_latex(d_eks, label = "eks", caption = "Døme")
  expect_type(tab, "character")
  expect_true(any(str_detect(tab, fixed("\\begin{table}"))))
  expect_true(any(str_detect(tab, fixed("\\end{table}"))))
  expect_true(any(str_detect(tab, fixed("\\label{eks}"))))
  expect_true(any(str_detect(tab, fixed("Døme"))))
})

# Ein tom tabell i ein årsrapport tyder som regel at noko er gale med
# dataa, så me vil ha ei synleg åtvaring i staden for ein tom tabell.
# Testen sikrar òg at me ikkje kjem i skade for å senda ei dataramme
# utan rader til Hmisc::latex(), som ikkje handterer slike (sjå
# kommentaren i lag_tab_latex()).
test_that("lag_tab_latex() gjev åtvaring i staden for tabell ved 0 rader", {
  tab = lag_tab_latex(d_tom, label = "tom", caption = "Tom tabell")
  expect_type(tab, "character")
  expect_true(any(str_detect(tab, fixed("0 rader"))))
  expect_true(any(str_detect(tab, fixed("\\color{errorcolor}"))))
  expect_true(any(str_detect(tab, fixed("\\label{tom}"))))
  # Skal ikkje innehalda sjølve tabellen
  expect_false(any(str_detect(tab, fixed("\\begin{tabular}"))))
})

test_that("lag_tab_latex() brukar widetable-miljøet når wide = TRUE", {
  tab = lag_tab_latex(d_eks, label = "eks", caption = "Døme", wide = TRUE)
  expect_true(any(str_detect(tab, fixed("\\begin{widetable}"))))
  expect_true(any(str_detect(tab, fixed("\\end{widetable}"))))
  expect_false(any(str_detect(tab, fixed("\\begin{table}"))))
})

# wide = TRUE vart tidlegare ignorert for tomme tabellar
test_that("lag_tab_latex() brukar widetable-miljøet òg ved 0 rader", {
  tab = lag_tab_latex(d_tom, label = "tom", caption = "Tom", wide = TRUE)
  expect_true(any(str_detect(tab, fixed("\\begin{widetable}"))))
  expect_true(any(str_detect(tab, fixed("\\end{widetable}"))))
  expect_false(any(str_detect(tab, fixed("\\begin{table}"))))
})

# Tidlegare returnerte 0-rad-greina éin enkelt tekststreng, medan
# den vanlege greina returnerte ein vektor med éi linje per element
test_that("lag_tab_latex() returnerer same format for tomme og
          ikkje-tomme tabellar", {
  tab_full = lag_tab_latex(d_eks, label = "eks", caption = "Døme")
  tab_tom = lag_tab_latex(d_tom, label = "tom", caption = "Tom")
  expect_type(tab_tom, typeof(tab_full))
  expect_gt(length(tab_tom), 1)
  # Alle linjene skal avsluttast med linjeskift
  expect_true(all(endsWith(tab_full, "\n")))
  expect_true(all(endsWith(tab_tom, "\n")))
})
