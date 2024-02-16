#Import libraries for calibration curve
library(ggplot2)
library(cluster)

# Define function that converts HEX color to grayscale intensity
hex_to_grayscale <- function(hex) {col <- col2rgb(hex)
  return(0.299 * col["red",] + 0.587 * col["green",] + 0.114 * col["blue",])}

# Define the calibration curve (15 levels from 1 mg/mL to 15 mg/mL)
# Assume black (0 intensity) corresponds to 15 mg/mL and white (255 intensity) to 1 mg/mL
calibration_colors <- colorRampPalette(c("#FFFFFF", "#000000"))(15)
calibration_concentrations <- seq(1, 15, length.out = 15)

# Convert calibration colors to grayscale intensities
calibration_intensities <- sapply(calibration_colors, hex_to_grayscale)

# Define function to interpolate concentration from HEX color
estimate_concentration <- function(hex_color) {intensity <- hex_to_grayscale(hex_color)

# Estimate concentration using a linear interpolation
estimated_concentration <- approx(calibration_intensities, calibration_concentrations, intensity)$y

# Calculate 10% standard deviation from estimated concentration
estimated_sd <- 0.1 * estimated_concentration

# Round estimation for easy reporting
estimated_concentration <- round(estimated_concentration, 1)
estimated_sd <- round(estimated_sd, 1)

return(list(concentration = estimated_concentration, sd = estimated_sd))}

# Put the function into action!
input_color <- "#00008B" # Replace with HEX color from SDS-PAGE gel
result <- estimate_concentration(input_color)
concentration_str <- paste(result$concentration, "+/-", result$sd, "mg/mL")
print(paste("Estimated concentration: ", concentration_str))

# Plot the calibration curve using ggplot2
calibration_plot <- ggplot(calibration_data, aes(x = Intensity, y = Concentration)) + geom_point(color = "blue") + geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Grayscale Intensity", y = "Concentration (mg/mL)", title = "Calibration Curve from 1 mg/mL to 15 mg/mL") + theme_light()

# Add the estimated concentration point with error bars for standard deviation onto the calibration plot
calibration_plot <- calibration_plot + geom_point(aes(x = hex_to_grayscale(input_color), y = result$concentration),
  shape = 16, color = "black", size = 3) + geom_errorbar(aes(x = hex_to_grayscale(input_color), ymin = result$concentration - result$sd,
  ymax = result$concentration + result$sd), color = "black", width = 0.2)

# Print the calibration plot
print(calibration_plot)