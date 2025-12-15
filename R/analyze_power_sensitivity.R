#' @title Analyze Power Sensitivity to VDD and Frequency
#'
#' @description Performs a sensitivity analysis by calculating dynamic power over a range of
#' VDD and frequency values, by fixing switching activity and capacitance.
#'
#' @param alpha_fixed The fixed average switching activity factor (e.g., average alpha).
#' @param C_fixed The fixed total capacitance (in Farads, F).
#' @param V_DD_range A numeric vector of supply voltage values (in Volts, V) to test.
#' @param f_range A numeric vector of clock frequency values (in Hertz, Hz) to test.
#'
#' @return A matrix where rows correspond to VDD values and columns to frequency values.
#'         Each element contains the estimated dynamic power (in Watts, W).
#'
#' @examples
#' VDDs <- seq(0.8, 1.2, by = 0.1) # 0.8V to 1.2V
#' Freqs <- seq(0.5e9, 2e9, length.out = 4) # 0.5 GHz to 2 GHz
#' power_matrix <- analyze_power_sensitivity(0.15, 60e-12, VDDs, Freqs)
#' print(power_matrix)
#'
#' @export
analyze_power_sensitivity <- function(alpha_fixed, C_fixed, V_DD_range, f_range) {

  # Vérification simple que les plages sont des vecteurs
  if (!is.numeric(V_DD_range) | !is.numeric(f_range)) {
    stop("Non-numeric values")
  }

  # Utilise la fonction 'outer' pour appliquer la formule de puissance à toutes les paires (V_DD, f)
  # Le VDD est mis au carré dans la formule : P = K * VDD^2 * f
  K <- 0.5 * alpha_fixed * C_fixed

  # Crée une fonction interne pour le calcul (qui sera appliquée par 'outer')
  power_calc <- function(V, F) {
    return(K * V^2 * F)
  }

  # La fonction 'outer' crée la matrice de résultats
  power_matrix <- outer(X = V_DD_range, Y = f_range, FUN = power_calc)

  # Nomme les dimensions pour une meilleure lisibilité
  rownames(power_matrix) <- paste0("VDD_", V_DD_range, " V")
  colnames(power_matrix) <- paste0("f_", f_range/1e9, " GHz")

  return(power_matrix)
}
