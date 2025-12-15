library(testthat)
library(VLSIDynPower)

# --- Constantes de base pour le test ---
alpha_test <- 0.5
C_L_test <- 10e-12  # 10 pF
V_DD_test <- 1.5    # 1.5 V
f_test <- 2e9       # 2 GHz

# --- Calcul Manuel pour Vérification (Oracle) ---
# P_dyn = 0.5 * alpha * C_L * V_DD^2 * f
# P_dyn = 0.5 * 0.5 * 10e-12 * (1.5)^2 * 2e9 = 0.01125 W

P_oracle <- 0.5 * alpha_test * C_L_test * V_DD_test^2 * f_test

test_that("estimate_dynamic_power calcule la puissance P_dyn selon la formule CMOS", {

  # Cas 1 : Cas nominal avec des valeurs typiques
  expect_equal(estimate_dynamic_power(alpha_test, C_L_test, V_DD_test, f_test), P_oracle)

  # Cas 2 : Alpha maximum (1.0)
  P_max_alpha <- 0.5 * 1.0 * C_L_test * V_DD_test^2 * f_test
  expect_equal(estimate_dynamic_power(1.0, C_L_test, V_DD_test, f_test), P_max_alpha)

  # Cas 3 : Alpha minimum (0.0), la puissance doit être 0
  expect_equal(estimate_dynamic_power(0.0, C_L_test, V_DD_test, f_test), 0.0)

  # Cas 4 : Changement de la tension (V_DD), qui a un impact quadratique
  # Si V_DD est doublé (3.0 V), la puissance doit être multipliée par 4
  P_double_Vdd <- 0.5 * alpha_test * C_L_test * (2 * V_DD_test)^2 * f_test
  P_current <- estimate_dynamic_power(alpha_test, C_L_test, 2 * V_DD_test, f_test)
  expect_equal(P_current, P_double_Vdd)
  expect_equal(P_current, P_oracle * 4)

  # Cas 5 : Fréquence (f) à zéro, la puissance doit être zéro
  expect_equal(estimate_dynamic_power(alpha_test, C_L_test, V_DD_test, 0), 0.0)

  # Cas 6 : Capacité (C) à zéro, la puissance doit être zéro
  expect_equal(estimate_dynamic_power(alpha_test, 0, V_DD_test, f_test), 0.0)

})

test_that("estimate_dynamic_power vérifie que les paramètres sont positifs", {

  # Cas d'erreur 1 : Alpha négatif
  expect_error(
    estimate_dynamic_power(-0.1, C_L_test, V_DD_test, f_test),
    "Non-positive values"
  )

  # Cas d'erreur 2 : Capacité négative
  expect_error(
    estimate_dynamic_power(alpha_test, -1e-12, V_DD_test, f_test),
    "Non-positive values"
  )

  # Cas d'erreur 3 : Tension négative
  expect_error(
    estimate_dynamic_power(alpha_test, C_L_test, -1.0, f_test),
    "Non-positive values"
  )

  # Cas d'erreur 4 : Fréquence négative
  expect_error(
    estimate_dynamic_power(alpha_test, C_L_test, V_DD_test, -1e9),
    "Non-positive values"
  )
})
