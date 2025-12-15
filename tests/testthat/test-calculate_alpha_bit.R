library(testthat)
library(VLSIDynPower)

test_that("calculate_alpha_bit compte correctement la distance de Hamming (transitions)", {

  # --- Cas 1 : Aucune transition ---
  A1 <- c(0, 0, 1, 1, 0)
  B1 <- c(0, 0, 1, 1, 0)
  expect_equal(calculate_alpha_bit(A1, B1), 0) # 0 transitions

  # --- Cas 2 : Transitions partielles (Exemple documenté) ---
  # A: 1 0 1 1
  # B: 1 1 0 1
  # Diff:0 1 1 0 (Somme = 2)
  A2 <- c(1, 0, 1, 1)
  B2 <- c(1, 1, 0, 1)
  expect_equal(calculate_alpha_bit(A2, B2), 2)

  # --- Cas 3 : Transition complète (tous les bits basculent) ---
  # A: 1 0 1 0
  # B: 0 1 0 1
  # Diff:1 1 1 1 (Somme = 4)
  A3 <- c(1, 0, 1, 0)
  B3 <- c(0, 1, 0, 1)
  expect_equal(calculate_alpha_bit(A3, B3), 4)

  # --- Cas 4 : Vecteurs vides ou de taille 1 (cas limites) ---
  expect_equal(calculate_alpha_bit(c(), c()), 0)
  expect_equal(calculate_alpha_bit(c(1), c(0)), 1)
  expect_equal(calculate_alpha_bit(c(0), c(0)), 0)

  # --- Cas 5 : Transition entre des types numériques/entiers ---
  A5 <- as.numeric(c(1, 0, 0))
  B5 <- as.integer(c(1, 1, 0))
  expect_equal(calculate_alpha_bit(A5, B5), 1) # Seulement le deuxième bit a basculé

})

test_that("calculate_alpha_bit gère l'erreur de non-conformabilité (longueurs différentes)", {

  # --- Cas d'erreur : Vecteurs de longueurs différentes ---
  A_long <- c(1, 0, 1, 1)
  B_short <- c(1, 1, 0)

  # La fonction doit s'arrêter et retourner un message d'erreur spécifique
  expect_error(calculate_alpha_bit(A_long, B_short), "non-conformable")
  expect_error(calculate_alpha_bit(c(1), c(1, 0)), "non-conformable")

})

test_that("calculate_alpha_bit gère l'erreur de non-binarité de données ", {

  # --- Cas d'erreur : Vecteurs de données non-binaires ---
  A_long <- c(1, 0, 2, 1)
  B_short <- c(5, 1, 0, 0)

  # La fonction doit s'arrêter et retourner un message d'erreur spécifique
  expect_error(calculate_alpha_bit(A_long, B_short), "Binary values obliged !")

})
