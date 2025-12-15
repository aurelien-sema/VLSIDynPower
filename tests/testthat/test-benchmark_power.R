library(testthat)
library(VLSIDynPower)

# --- Implémentations des fonctions dépendantes pour le test (si non exportées/visibles) ---

# Fonction calculate_alpha_bit (Distance de Hamming)
calculate_alpha_bit <- function(prev_word, curr_word) {
  # Assurez-vous que cette définition correspond à celle de votre package
  sum(prev_word != curr_word)
}

# Fonction estimate_dynamic_power (Formule CMOS)
estimate_dynamic_power <- function(alpha, C, V_DD, f) {
  # Assurez-vous que cette définition correspond à celle de votre package
  0.5 * alpha * C * V_DD^2 * f
}

# --- Définition des Données de Test ---

# Données fournies dans l'exemple
D_orig_test <- list(c(0,0,0,0), c(1,1,1,1), c(0,0,0,0)) # 3 mots, 4 bits/mot
D_opt_test  <- list(c(0,0,0,0), c(0,0,0,0), c(0,0,0,0))

C_L_val <- 50e-12; V_DD_val <- 1.0; f_val <- 1e9

# --- Calculs Manuels pour Vérification (Oracles) ---

# Calculs pour D_orig_test :
# Transitions totales : (0000 -> 1111) + (1111 -> 0000) = 4 + 4 = 8
Total_T_orig_oracle <- 8
# Nombre total d'opportunités de transition : (3 mots - 1) * 4 bits = 8
Alpha_orig_oracle <- Total_T_orig_oracle / 8
P_orig_oracle <- estimate_dynamic_power(Alpha_orig_oracle, C_L_val, V_DD_val, f_val) # P = 0.5 * 1.0 * 50e-12 * 1^2 * 1e9 = 0.025 W

# Calculs pour D_opt_test :
# Transitions totales : (0000 -> 0000) + (0000 -> 0000) = 0 + 0 = 0
Total_T_opt_oracle <- 0
Alpha_opt_oracle <- 0 / 8 # = 0.0
P_opt_oracle <- estimate_dynamic_power(Alpha_opt_oracle, C_L_val, V_DD_val, f_val) # P = 0.0 W

# Calcul de la réduction :
P_reduction_oracle <- ((P_orig_oracle - P_opt_oracle) / P_orig_oracle) * 100 # = 100.0%

# --- Bloc de Test principal (testthat) ---

test_that("benchmark_power calcule correctement les métriques et la réduction de puissance", {

  # Exécution de la fonction
  results <- benchmark_power(D_orig_test, D_opt_test, C_L_val, V_DD_val, f_val)

  # Vérification des dimensions du résultat
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), 3) # Original, Optimized, Reduction
  expect_equal(ncol(results), 4)

  # --- Vérification des métriques du flux ORIGINAL (Ligne 1) ---
  expect_equal(results$Metric[1], "Original")
  expect_equal(results$Total_Transitions[1], Total_T_orig_oracle)
  expect_equal(results$Avg_Alpha[1], Alpha_orig_oracle)
  expect_equal(results$Total_Power_W[1], P_orig_oracle)

  # --- Vérification des métriques du flux OPTIMISÉ (Ligne 2) ---
  expect_equal(results$Metric[2], "Optimized")
  expect_equal(results$Total_Transitions[2], Total_T_opt_oracle)
  expect_equal(results$Avg_Alpha[2], Alpha_opt_oracle)
  expect_equal(results$Total_Power_W[2], P_opt_oracle)

  # --- Vérification du gain de RÉDUCTION (Ligne 3) ---
  expect_equal(results$Metric[3], "Reduction (%)")
  expect_true(is.na(results$Total_Transitions[3]))
  expect_true(is.na(results$Avg_Alpha[3]))
  expect_equal(results$Total_Power_W[3], P_reduction_oracle)
})

test_that("benchmark_power gère les cas limites (un seul mot, flux de longueurs différentes)", {

  # Cas A : Flux avec un seul mot (aucune transition possible)
  D_single <- list(c(1, 1, 1, 1))
  results_single <- benchmark_power(D_single, D_single, C_L_val, V_DD_val, f_val)

  # Le calcul de calculate_metrics doit renvoyer 0 transitions et 0 alpha
  expect_equal(results_single$Total_Transitions[1], 0)
  expect_equal(results_single$Avg_Alpha[1], 0)
  expect_equal(results_single$Total_Power_W[1], 0)

  # Cas B : Flux de longueur différente (devrait échouer dans calculate_alpha_bit)
  # Pour que ce test passe, il faudrait s'assurer que l'erreur 'non-conformable' de
  # calculate_alpha_bit est propagée correctement, ou que le benchmark vérifie la
  # conformité des mots avant la boucle.
  D_mixed_length <- list(c(1, 0, 1, 0), c(1, 0, 1), c(0, 0, 0, 0))

  # Comme la vérification de longueur est dans calculate_alpha_bit, on attend une erreur
  expect_error(
    benchmark_power(D_orig_test, D_mixed_length, C_L_val, V_DD_val, f_val),
    "non-conformable",
    label = "Doit échouer si les longueurs de mots changent dans le flux optimisé"
  )
})
