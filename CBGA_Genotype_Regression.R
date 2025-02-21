# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

# Create a dataset with controlled R²
df <- data.frame(
        Plant_ID = 1:200,
        Genotype = sample(c("AA", "AG", "GG"), 200, replace = TRUE, prob = c(0.33, 0.33, 0.34))
)

# Assign CBGA values manually for controlled R²
df <- df %>%
        mutate(CBGA_Concentration = case_when(
                Genotype == "AA" ~ 4.5 + (Plant_ID * 0.005) + rnorm(n(), mean = 0, sd = 0.1),  # **Strong correlation (R² ≈ 0.9)**
                Genotype == "AG" ~ 2.5 + (Plant_ID * 0.002) + rnorm(n(), mean = 0, sd = 0.5),  # **Moderate correlation (R² ≈ 0.5)**
                Genotype == "GG" ~ 0.5 + rnorm(n(), mean = 0, sd = 1.5)                        # **High noise (R² ≈ 0.3)**
        ))

# Function to compute R² per genotype
compute_r2 <- function(subset_data) {
        if (nrow(subset_data) > 1) {  
                model <- lm(CBGA_Concentration ~ Plant_ID, data = subset_data)  
                return(summary(model)$r.squared)  
        } else {
                return(NA)  
        }
}

# Compute R² for each genotype
r_squared_AA <- compute_r2(df %>% filter(Genotype == "AA"))
r_squared_AG <- compute_r2(df %>% filter(Genotype == "AG"))
r_squared_GG <- compute_r2(df %>% filter(Genotype == "GG"))

# Print R² values
cat("R² for AA genotype:", round(r_squared_AA, 4), "\n")  # Should be ~0.9
cat("R² for AG genotype:", round(r_squared_AG, 4), "\n")  # Should be ~0.5
cat("R² for GG genotype:", round(r_squared_GG, 4), "\n")  # Should be ~0.3 or lower

# **Plot All Genotypes Together**
ggplot(df, aes(x = Plant_ID, y = CBGA_Concentration, color = Genotype)) +
        geom_point(alpha = 0.7) +                              # Scatter points
        geom_smooth(method = "lm", se = FALSE, lwd = 1) +      # Regression lines per genotype
        labs(title = "CBGA Concentration vs Plant ID (All Genotypes)",
             subtitle = paste("R² (AA) =", round(r_squared_AA, 4),
                              "| R² (AG) =", round(r_squared_AG, 4),
                              "| R² (GG) =", round(r_squared_GG, 4)),
             x = "Plant ID",
             y = "CBGA Concentration (% w/w)") +
        theme_minimal() +
        theme(
                plot.title = element_text(size = 20),  # Increase title font size
                plot.subtitle = element_text(size = 16),  # Increase subtitle font size
                axis.title.x = element_text(size = 18),  # Increase x-axis label font size
                axis.title.y = element_text(size = 18),  # Increase y-axis label font size
                axis.text.x = element_text(size = 15),   # Increase x-axis tick label font size
                axis.text.y = element_text(size = 15)    # Increase y-axis tick label font size
        )
