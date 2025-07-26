# sinus_col_generator.R

generate_col_file <- function(json_path, output_path, duration_sec = 5) {
  library(jsonlite)

  data <- fromJSON(json_path)
  freq <- as.numeric(readline("Bitte Frequenz in Hz eingeben: "))

  time_steps <- seq(0, duration_sec, by = 0.001)

  luminance_matrix <- sapply(1:nrow(data), function(i) {
    mean_lum <- data$Luminance[i]
    max_lum <- data$MaxLuminance[i]
    # Sinusmodulation
    mean_lum + (max_lum - mean_lum) * sin(2 * pi * freq * time_steps)
  })

  # Schreibe Datei: eine Zeile pro Zeit, Spalten = LEDs
  write.table(luminance_matrix, file = output_path, row.names = FALSE, col.names = FALSE)

  cat("Fertig. Datei gespeichert unter:", output_path, "\n")
}

# Beispiel-Aufruf:
# generate_col_file("SilentSubstiTutor-2025-07-26.json", "output.col")
