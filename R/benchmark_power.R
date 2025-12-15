#' @title Benchmark Power Reduction from Data Optimization
#'
#' @description Calculates and compares the estimated dynamic power for an original data stream
#' versus an optimized data stream (e.g., after ZTC or Gray coding).
#'
#' @param data_stream_original A list of binary vectors (0s and 1s) representing the original sequence of words.
#' @param data_stream_optimized A list of binary vectors (0s and 1s) representing the optimized sequence of words.
#' @param C_L_fixed The fixed total load capacitance (in Farads, F).
#' @param V_DD_fixed The fixed supply voltage (in Volts, V).
#' @param f_fixed The fixed clock frequency (in Hertz, Hz).
#'
#' @return A data frame containing the total transitions, average alpha, total power, and
#'         the percentage reduction achieved by the optimization.
#'
#' @examples
#' # Example using a simple ZTC concept (assuming ZTC output is aggregated into a stream)
#' # Note: In a real scenario, you'd aggregate the output of apply_ztc_coding.
#' D_orig <- list(c(0,0,0,0), c(1,1,1,1), c(0,0,0,0))
#' D_opt  <- list(c(0,0,0,0), c(0,0,0,0), c(0,0,0,0)) # Example of successful ZTC
#'
#' C_L_val <- 50e-12; V_DD_val <- 1.0; f_val <- 1e9
#' benchmark_results <- benchmark_power(D_orig, D_opt, C_L_val, V_DD_val, f_val)
#' print(benchmark_results)
#'
#' @export
benchmark_power <- function(data_stream_original, data_stream_optimized, C_L_fixed, V_DD_fixed, f_fixed) {

  # Implémente implicitement 'calculate_alpha_bit' et 'estimate_dynamic_power'

  # 1. Fonction interne pour calculer les métriques d'une séquence
  calculate_metrics <- function(stream) {

    # Nombre de mots dans le flux
    n_words <- length(stream)
    if (n_words <= 1) return(list(total_transitions = 0, avg_alpha = 0, total_power = 0))

    # Longueur de mot
    word_length <- length(stream[[1]])

    total_transitions <- 0

    # Boucle sur la séquence pour comparer chaque mot au précédent
    for (i in 2:n_words) {
      prev_word <- stream[[i-1]]
      curr_word <- stream[[i]]
      # Accumule le nombre de transitions pour l'ensemble du flux
      total_transitions <- total_transitions + calculate_alpha_bit(prev_word, curr_word)
    }

    # Calcule l'activité moyenne : transitions totales / (mots * bits par mot)
    avg_alpha <- total_transitions / ((n_words - 1) * word_length)

    # Calcule la puissance dynamique totale
    total_power <- estimate_dynamic_power(avg_alpha, C_L_fixed, V_DD_fixed, f_fixed)

    return(list(
      total_transitions = total_transitions,
      avg_alpha = avg_alpha,
      total_power = total_power
    ))
  }

  # 2. Calcul des métriques pour les deux flux
  metrics_orig <- calculate_metrics(data_stream_original)
  metrics_opt <- calculate_metrics(data_stream_optimized)

  # 3. Construction du tableau de résultats
  results <- data.frame(
    Metric = c("Original", "Optimized"),
    Total_Transitions = c(metrics_orig$total_transitions, metrics_opt$total_transitions),
    Avg_Alpha = c(metrics_orig$avg_alpha, metrics_opt$avg_alpha),
    Total_Power_W = c(metrics_orig$total_power, metrics_opt$total_power)
  )

  # 4. Calcul du gain d'optimisation
  power_reduction_pct <- ((metrics_orig$total_power - metrics_opt$total_power) / metrics_orig$total_power) * 100

  # 5. Ajout du résultat de réduction
  reduction_row <- data.frame(
    Metric = "Reduction (%)",
    Total_Transitions = NA,
    Avg_Alpha = NA,
    Total_Power_W = power_reduction_pct
  )

  # Retourne le tableau de résultats complet
  return(rbind(results, reduction_row))
}
