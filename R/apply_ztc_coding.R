#' @title Apply Zero Transition Coding (Bus-Inversion Coding)
#'
#' @description Optimizes dynamic power by minimizing the number of bit transitions (flips)
#' between two consecutive data words. It transmits the inverted word if the resulting
#' activity is lower, adding an inversion bit indicator.
#'
#' @param data_vector_prev A numeric vector (0s and 1s) representing the previously transmitted word.
#' @param data_vector_curr A numeric vector (0s and 1s) representing the current word to be transmitted.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{optimized_data}: The vector transmitted (either the original or the inverted word).
#'   \item \code{inversion_bit}: The bit indicator (1 if inversion was performed, 0 otherwise).
#'   \item \code{transitions_saved}: The number of transitions saved by the optimization.
#' }
#'
#' @examples
#' D_prev <- c(0, 0, 0, 0, 0, 0, 0, 0)
#' D_curr_high_activity <- c(1, 1, 1, 1, 1, 1, 1, 1) # 8 transitions
#' # Inverted D_curr is 00000000 (0 transitions)
#' result <- apply_ztc_coding(D_prev, D_curr_high_activity)
#' print(result)
#'
#' @export
apply_ztc_coding <- function(data_vector_prev, data_vector_curr) {

  # Importe implicitement 'calculate_alpha_bit'

  # 1. Calcul de l'activité originale (nombre de transitions)
  transitions_original <- calculate_alpha_bit(data_vector_prev, data_vector_curr)

  # 2. Inversion du mot de données actuel
  data_vector_curr_inverted <- 1 - data_vector_curr

  # 3. Calcul de l'activité avec le mot inversé (transitions par rapport au précédent)
  transitions_inverted <- calculate_alpha_bit(data_vector_prev, data_vector_curr_inverted)

  # Initialisation des variables de retour
  optimized_data <- data_vector_curr
  inversion_bit <- 0
  transitions_saved <- 0

  # 4. Décision d'optimisation (si l'inversion réduit les transitions)
  if (transitions_inverted < transitions_original) {

    # L'inversion est bénéfique : on transmet le mot inversé
    optimized_data <- data_vector_curr_inverted

    # Le bit d'inversion est à 1 pour indiquer l'opération
    inversion_bit <- 1

    # Calcul du nombre de transitions économisées
    transitions_saved <- transitions_original - transitions_inverted
  }

  # 5. Création et retour de la liste de résultats
  return(list(
    optimized_data = optimized_data,
    inversion_bit = inversion_bit,
    transitions_saved = transitions_saved
  ))
}
