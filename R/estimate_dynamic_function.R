#' @title Estimate CMOS Dynamic Power Dissipation
#'
#' @description Calculates the estimated dynamic power consumption (\eqn{P_{dyn}})
#' based on the standard CMOS formula: \eqn{P_{dyn} = 0.5 \cdot \alpha \cdot C_L \cdot V_{DD}^2 \cdot f}.
#'
#' @param alpha The average switching activity factor (0 to 1). If calculating for a word,
#'              it is the total number of transitions divided by the total number of bits.
#' @param C The total capacitance being switched (in Farads, F).
#' @param V_DD The supply voltage (in Volts, V).
#' @param f The operating clock frequency (in Hertz, Hz).
#'
#' @return A numeric value representing the estimated dynamic power (in Watts, W).
#'
#' @examples
#' # Assuming low activity (alpha=0.1), typical capacitance, voltage, and frequency
#' alpha_val <- 0.1
#' C_L_val <- 50e-12    # 50 pF
#' V_DD_val <- 1.0     # 1.0 V
#' f_val <- 1e9        # 1 GHz
#' P_dyn <- estimate_dynamic_power(alpha_val, C_L_val, V_DD_val, f_val)
#' print(P_dyn)
#'
#' @export
estimate_dynamic_power <- function(alpha, C, V_DD, f) {

  # Vérification simple des contraintes physiques (les valeurs doivent être positives)
  if (any(c(alpha, C, V_DD, f) < 0)) {
    stop("Non-positive values")
  }

  # Implémentation de la formule de puissance dynamique CMOS : 0.5 * alpha * C_L * V_DD^2 * f
  P_dyn <- 0.5 * alpha * C * V_DD^2 * f

  # Retourne la valeur de la puissance en Watts
  return(P_dyn)
}
