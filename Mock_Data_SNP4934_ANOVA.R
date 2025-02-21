# Load necessary libraries
# 'dplyr' for data manipulation, 'ggplot2' for plotting, and 'multcompView' for Tukey HSD test letters
library(dplyr)
library(ggplot2)
library(multcompView)

# Set seed for reproducibility, ensuring that the random data generated is the same every time
set.seed(42)

# Generate mock data for the experiment
# 'Plant_ID' will be a sequence of 200 plant IDs
Plant_ID <- 1:200

# 'Genotype' will be randomly assigned as "AA", "AG", or "GG" with probabilities of 0.25, 0.50, and 0.25, respectively
Genotype <- sample(c("AA", "AG", "GG"), 200, replace = TRUE, prob = c(0.25, 0.50, 0.25))

# 'CBGA_Concentration' will be generated based on the genotype:
# - If genotype is "AA", CBGA concentration is normally distributed with mean=4.5 and sd=0.5
# - If genotype is "AG", CBGA concentration is normally distributed with mean=2.5 and sd=0.5
# - If genotype is "GG", CBGA concentration is normally distributed with mean=0.5 and sd=0.2
CBGA_Concentration <- ifelse(
        Genotype == "AA", rnorm(200, mean = 4.5, sd = 0.5),
        ifelse(Genotype == "AG", rnorm(200, mean = 2.5, sd = 0.5),
               rnorm(200, mean = 0.5, sd = 0.2))
)

# Create a dataframe from the generated data
df <- data.frame(Plant_ID, Genotype, CBGA_Concentration)

# Convert Genotype to a factor to ensure proper handling in ggplot (ensuring proper order on x-axis)
df$Genotype <- factor(df$Genotype, levels = c("AA", "AG", "GG"))

# Perform ANOVA to check for significant differences between genotypes
anova_model <- aov(CBGA_Concentration ~ Genotype, data = df)

# Extract the p-value from the ANOVA results for displaying in the subtitle
anova_p <- summary(anova_model)[[1]]["Genotype", "Pr(>F)"]

# Perform Tukey's HSD test to conduct pairwise comparisons between genotypes
tukey_results <- TukeyHSD(anova_model)

# Extract the Tukey test results and assign letters to represent statistical significance
tukey_letters <- multcompLetters4(anova_model, tukey_results)$Genotype

# Create a dataframe to hold the Tukey results and the corresponding letters
letters_df <- data.frame(Genotype = names(tukey_letters$Letters), Letter = tukey_letters$Letters)

# Create boxplot for CBGA concentration by genotype, with Tukey letters and customized axis ticks
ggplot(df, aes(x = Genotype, y = CBGA_Concentration, fill = Genotype)) +
        geom_boxplot() +  # Add boxplot to show distribution of CBGA concentrations by Genotype
        geom_text(data = letters_df, aes(x = Genotype, y = max(df$CBGA_Concentration) + 0.3, label = Letter), size = 7) +  # Add Tukey test letters above the boxes
        theme_minimal() +  # Use a clean minimal theme
        labs(title = "CBGA Concentration by Genotype",  # Add plot title
             subtitle = paste0("ANOVA p-value = ", signif(anova_p, 3)),  # Add subtitle with the ANOVA p-value
             y = "CBGA Concentration (% w/w)", x = "Genotype") +  # Label axes
        theme(plot.subtitle = element_text(size = 18),  # Set subtitle font size
              axis.title.x = element_text(size = 18),  # Set x-axis label font size
              axis.title.y = element_text(size = 18),  # Set y-axis label font size
              axis.text.x = element_text(size = 16),  # Set x-axis tick text font size
              axis.text.y = element_text(size = 16),  # Set y-axis tick text font size
              plot.title = element_text(size = 20)) +  # Make the title larger
        scale_y_continuous(breaks = seq(0, 6, by = 1.5)) +  # Customize y-axis ticks (0, 1.5, 3, 4.5, 6)
        scale_x_discrete(labels = c("AA", "AG", "GG"))  # Ensure the x-axis labels are the desired genotypes

# Save the plot as a PNG file
ggsave("CBGA_Genotype_Boxplot_With_ANOVA_Customized.png", width = 6, height = 4, dpi = 300)
