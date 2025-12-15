library(testthat)
library(VLSIDynPower)

# --- Fonction Dépendante pour l'Autonomie du Test ---

# Fonction calculate_alpha_bit (Distance de Hamming)
calculate_alpha_bit <- function(prev_word, curr_word) {
  # Assurez-vous que cette définition correspond à celle de votre package
  # L'erreur 'non-conformable' n'est pas testée ici pour simplifier,
  # car l'entrée est générée en interne.
  sum(prev_word != curr_word)
}

# --- Définition des Données de Test ---

# Longueur de mot constante : 8 bits
D_prev <- c(0, 0, 0, 0, 0, 0, 0, 0)
D_inv <- c(1, 1, 1, 1, 1, 1, 1, 1) # Inversion de D_prev

test_that("apply_ztc_coding prend la meilleure décision d'inversion", {

  # --- Cas 1 : Inversion très bénéfique (Maximum de transitions sauvé) ---
  D_curr_max_activity <- c(1, 1, 1, 1, 1, 1, 1, 1) # 8 transitions originales (vs D_prev)
  D_curr_max_inverted <- c(0, 0, 0, 0, 0, 0, 0, 0) # 0 transitions après inversion

  result_max <- apply_ztc_coding(D_prev, D_curr_max_activity)

  expect_equal(result_max$inversion_bit, 1)
  expect_equal(result_max$optimized_data, D_curr_max_inverted)
  expect_equal(result_max$transitions_saved, 8)

  # --- Cas 2 : Pas d'inversion (transitions originales < transitions inversées) ---
  D_curr_low_activity <- c(0, 0, 0, 1, 0, 0, 0, 0) # 1 transition originale
  D_curr_low_inverted <- c(1, 1, 1, 0, 1, 1, 1, 1) # 7 transitions après inversion

  result_low <- apply_ztc_coding(D_prev, D_curr_low_activity)

  expect_equal(result_low$inversion_bit, 0)
  expect_equal(result_low$optimized_data, D_curr_low_activity) # Le mot original est transmis
  expect_equal(result_low$transitions_saved, 0)

  # --- Cas 3 : Point d'égalité (seuil de 50%) ---
  # L'optimisation ne doit avoir lieu que si l'inversion EST STRICTEMENT INFÉRIEURE.
  # Original: 4 transitions. Inversé: 4 transitions.
  D_curr_equal_activity <- c(1, 1, 0, 0, 0, 0, 0, 0) # 4 transitions originales
  D_curr_equal_inverted <- c(0, 0, 1, 1, 1, 1, 1, 1) # 4 transitions après inversion

  result_equal <- apply_ztc_coding(D_prev, D_curr_equal_activity)

  expect_equal(result_equal$inversion_bit, 0) # Doit être 0
  expect_equal(result_equal$optimized_data, D_curr_equal_activity) # Le mot original est transmis
  expect_equal(result_equal$transitions_saved, 0)

  # --- Cas 4 : Cas où une seule transition est sauvée ---
  D_curr_save_one <- c(1, 1, 0, 0, 0, 0, 0, 1) # 3 transitions originales
  D_curr_save_one_inverted <- c(0, 0, 1, 1, 1, 1, 1, 0) # 5 transitions inversées (vs D_prev)
  # ERREUR DANS LA LOGIQUE MANUELLE CI-DESSUS :
  # D_prev est (0,0,0,0,0,0,0,0)
  # Orig: 3 trans.
  # Inv: 5 trans.
  # L'inversion NE DOIT PAS avoir lieu. Transitions sauvées = 0.

  D_curr_to_save <- c(1, 1, 1, 1, 0, 0, 0, 0) # 4 transitions originales
  D_curr_to_save_inverted <- c(0, 0, 0, 0, 1, 1, 1, 1) # 4 transitions inversées

  # Pour forcer une économie de 1 (5 vs 4)
  D_curr_5 <- c(1, 1, 1, 1, 1, 0, 0, 0) # 5 transitions originales
  D_curr_5_inverted <- c(0, 0, 0, 0, 0, 1, 1, 1) # 3 transitions inversées (5 > 3)

  result_save_one <- apply_ztc_coding(D_prev, D_curr_5)

  expect_equal(result_save_one$inversion_bit, 1)
  expect_equal(result_save_one$optimized_data, D_curr_5_inverted)
  expect_equal(result_save_one$transitions_saved, 2) # 5 - 3 = 2
})
