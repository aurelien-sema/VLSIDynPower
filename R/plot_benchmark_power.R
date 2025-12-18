#' @title Plot Power Benchmark Comparison and Save as PDF
#'
#' @description Calls benchmark_power to calculate the power reduction, generates
#' a static ggplot bar chart, and saves it to a PDF file.
#'
#' @param data_stream_original A list of binary vectors (0s and 1s) representing the original sequence of words.
#' @param data_stream_optimized A list of binary vectors (0s and 1s) representing the optimized sequence of words.
#' @param C_L_fixed The fixed total load capacitance (in Farads, F).
#' @param V_DD_fixed The fixed supply voltage (in Volts, V).
#' @param f_fixed The fixed clock frequency (in Hertz, Hz).
#' @param output_filename_pdf The path and filename (ending in .pdf) to save the plot.
#' Standardly, it is "benchmark_comparison.pdf"
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs theme_minimal annotate ggsave geom_text scale_y_continuous labs
#' @return The ggplot object (also saves a PDF file and an MP4 file).
#'
#' @examples
#' \dontrun{
#' D_orig <- list(c(0,1,0,0), c(1,1,0,1), c(1,1,1,0))
#' D_opt  <- list(c(1,0,0,1), c(0,0,1,1), c(1,0,0,0))
#' C_L <- 50e-12; V_DD <- 1.0; f <- 1e9
#' power_plot <- plot_benchmark_power(D_orig, D_opt, C_L, V_DD, f)
#' }
#'
#' @export
plot_benchmark_power <- function(data_stream_original, data_stream_optimized, C_L_fixed, V_DD_fixed, f_fixed, output_filename_pdf = "benchmark_comparison") {

  # 1. Preparing data
  benchmark_results <- benchmark_power(data_stream_original, data_stream_optimized, C_L_fixed, V_DD_fixed, f_fixed)
  df <- benchmark_results[!is.na(benchmark_results$Total_Power_W), ]

  P_orig <- df$Total_Power_W[df$Metric == "Original"]
  P_opt <- df$Total_Power_W[df$Metric == "Optimized"]
  power_reduction_pct <- (abs(P_orig - P_opt) / P_orig) * 100
  y_max <- max(df$Total_Power_W) * 1.1

  # --- 2. Creation et Sauvegarde du PDF Statique ---
  output_dir <- file.path("inst", "graphics")
  pdf_filename <- file.path(output_dir, paste0(output_filename_pdf, ".pdf"))

  power_plot <- ggplot2::ggplot(df, ggplot2::aes(x = Metric, y = Total_Power_W, fill = Metric)) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(Total_Power_W * 1000, 3), " mW")),
      vjust = -0.5, size = 4
    ) +
    ggplot2::labs(
      title = "Comparaison de la Puissance Dynamique Estimee",
      subtitle = paste0("Reduction de puissance obtenue : ", round(power_reduction_pct, 2), "%"),
      x = NULL,
      y = "Puissance Totale Estimee (W)",
      fill = "Sequence"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max)) +
    ggplot2::theme_minimal() +
    ggplot2::annotate(
      "text", x = 1.5, y = y_max * 0.95,
      label = paste0("Gain de Puissance : ", round(power_reduction_pct, 2), "%"),
      color = "#006400", size = 6, fontface = "bold"
    )

  ggplot2::ggsave(filename = pdf_filename, plot = power_plot, width = 7, height = 5)
  message(paste("Graphique statique sauvegarde:", pdf_filename))

  return(power_plot)
}
