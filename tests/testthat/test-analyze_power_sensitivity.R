library(testthat)
library(VLSIDynPower)

# --- Paramètres Fixes pour le Test ---
ALPHA_FIXED <- 0.2
C_FIXED <- 100e-12 # 100 pF

# --- Plages de Test Simples ---
VDD_RANGE_TEST <- c(1.0, 1.2) # 2 VDD values
F_RANGE_TEST <- c(1e9, 2e9, 3e9) # 3 Frequency values

# --- Calcul Manuel pour Vérification (Oracle) ---
# Formule: P_dyn = K * VDD^2 * f, où K = 0.5 * alpha * C
K_oracle <- 0.5 * ALPHA_FIXED * C_FIXED # K = 0.5 * 0.2 * 100e-12 = 10e-12

# Points spécifiques à vérifier
# P(VDD=1.0, f=1e9) = K * 1.0^2 * 1e9 = 10e-12 * 1e9 = 0.01 W
P_1_1 <- K_oracle * 1.0^2 * 1e9

# P(VDD=1.2, f=3e9) = K * 1.2^2 * 3e9 = 10e-12 * 1.44 * 3e9 = 43.2e-3 W = 0.0432 W
P_2_3 <- K_oracle * 1.2^2 * 3e9

# Structure attendue de la matrice (2 lignes x 3 colonnes)
# R1C1 (VDD=1.0, f=1e9) : 0.01 W
# R2C3 (VDD=1.2, f=3e9) : 0.0432 W

test_that("analyze_power_sensitivity calcule correctement la matrice de puissance", {

  # Exécution de la fonction
  power_matrix <- analyze_power_sensitivity(ALPHA_FIXED, C_FIXED, VDD_RANGE_TEST, F_RANGE_TEST)

  # --- Vérification de la structure et des dimensions ---
  expect_true(is.matrix(power_matrix))
  expect_equal(nrow(power_matrix), 2) # Nombre de VDDs
  expect_equal(ncol(power_matrix), 3) # Nombre de Fréquences

  # --- Vérification des calculs spécifiques (Oracle) ---

  # Vérification de l'élément (1, 1)
  expect_equal(power_matrix[1, 1], P_1_1)

  # Vérification de l'élément (2, 3)
  expect_equal(power_matrix[2, 3], P_2_3)

  # Vérification de l'impact quadratique de VDD (VDD 1.2 est 1.44 fois plus puissant que VDD 1.0)
  # Pour f=1e9 : P(1.2V) / P(1.0V)
  ratio_vd_sq <- (1.2 / 1.0)^2
  expect_equal(power_matrix[2, 1] / power_matrix[1, 1], ratio_vd_sq)

  # Vérification de l'impact linéaire de la fréquence
  # Pour VDD=1.0 : P(2e9) / P(1e9)
  expect_equal(power_matrix[1, 2] / power_matrix[1, 1], 2)

  # --- Vérification des noms de dimensions pour la lisibilité ---
  expect_equal(rownames(power_matrix), c("VDD_1 V", "VDD_1.2 V"))
  expect_equal(colnames(power_matrix), c("f_1 GHz", "f_2 GHz", "f_3 GHz"))
})

test_that("analyze_power_sensitivity gère les entrées non-numériques", {

  # Cas 1 : VDD_range n'est pas numérique
  expect_error(
    analyze_power_sensitivity(ALPHA_FIXED, C_FIXED, c("1.0", "1.2"), F_RANGE_TEST),
    "Non-numeric values"
  )

  # Cas 2 : f_range n'est pas numérique
  expect_error(
    analyze_power_sensitivity(ALPHA_FIXED, C_FIXED, VDD_RANGE_TEST, c("1e9", "2e9")),
    "Non-numeric values"
  )
})
