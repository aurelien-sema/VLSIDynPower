#' @title Calculate Bit-Level Switching Activity (Alpha)
#'
#' @description Computes the total sum of bit transitions (Hamming distance) between two binary vectors (data words),
#' which is directly proportional to the switching activity (\eqn{\alpha}) and dynamic power.
#'
#' @param data_vector_A A numeric or integer vector representing the previous data word (state).
#'                      Elements must be 0 or 1.
#' @param data_vector_B A numeric or integer vector representing the current data word (state).
#'                      Elements must be 0 or 1 and must have the same length as \code{data_vector_A}.
#'
#' @return An integer value representing the total of bit transitions (flips) \eqn{\alpha} (average activity).
#'
#' @examples
#' # Transition from 0b1011 to 0b1101 (2 transitions)
#' A <- c(1, 0, 1, 1)
#' B <- c(1, 1, 0, 1)
#' calculate_alpha_bit(A, B) # Should return 2
#'
#' @export
calculate_alpha_bit <- function(data_vector_A, data_vector_B) {

  # Vérification que les vecteurs ont la même longueur
  if (length(data_vector_A) != length(data_vector_B)) {
    stop("non-conformable")
  }

  if(length(data_vector_A) == 0) return(0)

  for (i in 1:length(data_vector_A)) {
    if (((data_vector_A[i] != 0) & (data_vector_A[i] != 1)) | ((data_vector_B[i] != 0) & (data_vector_B[i] != 1))) {
      stop("Binary values obliged !")
    }
  }

  # Utilisation de l'opérateur XOR pour identifier les transitions
  # A XOR B donne 1 là où les bits diffèrent (transition) et 0 là où ils sont identiques
  transitions_vector <- data_vector_A != data_vector_B

  # La fraction de transitions est la somme des éléments 'TRUE' (qui sont traités comme 1)
  # sur le nombre de bits pris en entrée
  total_transitions <- sum(transitions_vector)

  # Retourne la fraction de bits qui ont basculé
  return(total_transitions)
}
