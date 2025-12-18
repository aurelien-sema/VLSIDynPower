#' @title Plot and Animate Power Sensitivity Analysis
#'
#' @description Calls analyze_power_sensitivity, transforms the data, and generates two
#' static ggplot objects (P_dyn vs VDD, P_dyn vs Freq), saving each one to a separate PDF file.
#' Also generates two animated MP4 videos showing the sensitivity.
#'
#' @param alpha_fixed The fixed average switching activity factor.
#' @param C_L_fixed The fixed total load capacitance (in Farads, F).
#' @param V_DD_range A numeric vector of supply voltage values (in Volts, V) to test.
#' @param f_range A numeric vector of clock frequency values (in Hertz, Hz) to test.
#' @param base_output_filename_base The base path and filename (e.g., "sensitivity_")
#'                                 The function will append "_VDD.pdf", "_Freq.pdf",
#'                                 "_VDD.mp4", and "_Freq.mp4". Standardly, it is "power_sensitivity"
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal ggsave
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom gganimate transition_reveal ease_aes animate anim_save av_renderer
#' @return A list containing the two ggplot objects and the two gganimate objects (also saves PDF and MP4 files).
#'
#' @examples
#' \dontrun{
#' VDDs <- seq(0.8, 1.2, length.out = 10)
#' Freqs <- seq(0.5e9, 2.5e9, length.out = 10) # Augmente le nombre de points pour meilleure animation
#' sensitivity_outputs <- plot_power_sensitivity(0.15, 60e-12, VDDs, Freqs)
#' }
#' @export
plot_power_sensitivity <- function(alpha_fixed, C_L_fixed, V_DD_range, f_range, base_output_filename_base = "power_sensitivity") {

  # 1. Calcul de la matrice de puissance
  K <- 0.5 * alpha_fixed * C_L_fixed
  power_calc <- function(V, F) { return(K * V^2 * F) }
  power_matrix <- base::outer(X = V_DD_range, Y = f_range, FUN = power_calc)

  # 2. Preparation des donnees pour ggplot2 (Format long)
  sensitivity_df <- as.data.frame(power_matrix)
  colnames(sensitivity_df) <- paste0("F", 1:length(f_range))
  sensitivity_df$VDD_V <- V_DD_range

  df_long <- tidyr::pivot_longer(
    sensitivity_df,
    cols = -VDD_V,
    names_to = "Frequency_Label",
    values_to = "Power_W"
  )

  # Ajout des vraies valeurs de frequence et VDD pour la legende et les axes
  f_label_map <- data.frame(Frequency_Label = paste0("F", 1:length(f_range)),
                            Freq_GHz = f_range / 1e9)
  df_long <- dplyr::left_join(df_long, f_label_map, by = "Frequency_Label")

  # Conversion en facteurs pour la legende pour VDD (utile pour l'animation par VDD)
  df_long$VDD_Factor <- factor(df_long$VDD_V, levels = sort(unique(df_long$VDD_V)))
  df_long$Freq_Factor <- factor(df_long$Freq_GHz, levels = sort(unique(df_long$Freq_GHz)))


  # --- Graphique 1 : P_dyn vs VDD (avec 5 courbes de frequence) ---
  # Le grouping est implicitement gere par 'color = Freq_Factor'
  output_dir <- file.path("inst", "graphics")

  plot_vdd_vs_freq <- ggplot2::ggplot(df_long, ggplot2::aes(x = VDD_V, y = Power_W, color = Freq_Factor)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "P_dyn vs. VDD",
      subtitle = "Chaque courbe represente une frequence differente",
      x = "Tension d'Alimentation (V)",
      y = "Puissance Dynamique (W)",
      color = "Frequence (GHz)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")

  output_vdd_pdf <- file.path(output_dir, paste0(base_output_filename_base, "_VDD.pdf"))
  ggplot2::ggsave(output_vdd_pdf, plot = plot_vdd_vs_freq, width = 12, height = 8)
  message(paste("Graphique P_dyn vs VDD enregistre dans :", output_vdd_pdf))

  # --- Animation 1 : P_dyn vs VDD (points progressifs) ---
  # L'animation se deroule le long de l'axe X (VDD_V)
  anim_vdd_vs_freq <- plot_vdd_vs_freq +
    gganimate::transition_reveal(VDD_V) +
    gganimate::ease_aes('linear') +
    ggplot2::labs(title = 'P_dyn vs. VDD: {frame_time} VDD') # Titre dynamique

  output_vdd_mp4 <- file.path(output_dir, paste0(base_output_filename_base, "_VDD.mp4"))
  gganimate::animate(anim_vdd_vs_freq,
                     renderer = gganimate::av_renderer(), # Utilise av pour MP4
                     width = 3840, height = 2160,          # Resolution 4K
                     res = 300,                            # DPI pour la sortie
                     fps = 10,                             # Cadres par seconde
                     duration = length(V_DD_range) * 0.5)  # Duree totale de l'animation
  gganimate::anim_save(output_vdd_mp4, animation = gganimate::last_animation())
  message(paste("Video P_dyn vs VDD animee enregistree dans :", output_vdd_mp4))


  # --- Graphique 2 : P_dyn vs Frequence (avec 5 courbes de VDD) ---
  # Le grouping est implicitement gere par 'color = VDD_Factor'
  plot_freq_vs_vdd <- ggplot2::ggplot(df_long, ggplot2::aes(x = Freq_GHz, y = Power_W, color = VDD_Factor)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = "P_dyn vs. Frequence",
      subtitle = "Chaque courbe represente une tension VDD differente",
      x = "Frequence (GHz)",
      y = "Puissance Dynamique (W)",
      color = "VDD (V)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right")

  output_freq_pdf <- file.path(output_dir, paste0(base_output_filename_base, "_Freq.pdf"))
  ggplot2::ggsave(output_freq_pdf, plot = plot_freq_vs_vdd, width = 12, height = 8)
  message(paste("Graphique P_dyn vs Frequence enregistre dans :", output_freq_pdf))


  # --- Animation 2 : P_dyn vs Frequence (points progressifs) ---
  # L'animation se deroule le long de l'axe X (Freq_GHz)
  anim_freq_vs_vdd <- plot_freq_vs_vdd +
    gganimate::transition_reveal(Freq_GHz) +
    gganimate::ease_aes('linear') +
    ggplot2::labs(title = 'P_dyn vs. Frequence: {frame_time} GHz') # Titre dynamique

  output_freq_mp4 <- file.path(output_dir, paste0(base_output_filename_base, "_Freq.mp4"))
  gganimate::animate(anim_freq_vs_vdd,
                     renderer = gganimate::av_renderer(), # Utilise av pour MP4
                     width = 3840, height = 2160,          # Resolution 4K
                     res = 300,                            # DPI pour la sortie
                     fps = 10,
                     duration = length(f_range) * 0.5)
  gganimate::anim_save(output_freq_mp4, animation = gganimate::last_animation())
  message(paste("Video P_dyn vs Frequence animee enregistree dans :", output_freq_mp4))


  # 5. Retour des objets ggplot et gganimate
  return(list(
    plot_vdd_vs_freq = plot_vdd_vs_freq,
    anim_vdd_vs_freq = anim_vdd_vs_freq,
    plot_freq_vs_vdd = plot_freq_vs_vdd,
    anim_freq_vs_vdd = anim_freq_vs_vdd
  ))
}
