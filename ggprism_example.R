# Requirements
install.packages(c("ggplot2", "ggprism", "dplyr", "reshape2", "RColorBrewer"))

# Load libraries
library(ggplot2)
library(ggprism)
library(dplyr)
library(reshape2)
library(RColorBrewer)

################### Templates for most common plots in Prism ###################
# Define MesoLab colour Scheme
#5f0053 - Downregulated/Control/NonResponders etc.
#087c95 - Upregulated/Treated/Responders
mesolab.cols <- c("Treatment" = "#087c95", "Control" = "#5f0053")

# Create a function to draw asterisks based on the standard p-value levels
# 0.05 or lower  = *
# 0.01 or lower  = **
# 0.001 or lower = ***

get_stars <- function(p) {
      if (p < 0.001) return("***")
      else if (p < 0.01) return("**")
      else if (p <= 0.05) return("*")
      else return("ns")
}

# Example p_value for all graphs
p.value <- 0.05
p_star <- get_stars(p.value) 

# Set base size of fonts as large as possible for publication ready figures
base_size <- 30

#------------------ 1. Bar Plot ------------------------------------------------
data_bar <- data.frame(  # Example Data
  Group = c("A", "B", "C"),
  Value = c(5, 7, 4)
)

# Calculate max positiion of y axis data to position the asterisks
y_max <- max(data_bar$Value)

ggplot(data_bar, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity", color = "black") +
  theme_prism(base_size = base_size) +
  labs(title = "Bar Plot", y = "Value") + 
  annotate("segment", x = 1, xend = 3, y = y_max + 0.2, yend = y_max + 0.2) + # Creates a vertical bar
  annotate("text", x = 2, y = y_max + 0.4, label = p_star, size = 6)          # Annotates the vertical bar with * based on p value
# x argument sets the posititon of the bar to span as much elements as there are shown

################## 2. Box Plot 
data_box <- data.frame(
  Group = rep(c("Control", "Treatment"), each = 20),
  Value = c(rnorm(20, 5, 1), rnorm(20, 6, 1.2))
)

ggplot(data_box, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  theme_prism(base_size = base_size) +
  labs(title = "Box Plot") +
  scale_fill_manual(values = mesolab.cols)

################## 3. Violin Plot
ggplot(data_box, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(trim = FALSE) +
  theme_prism(base_size = base_size) +
  labs(title = "Violin Plot") +
  scale_fill_manual(values = mesolab.cols)

################## 4. Scatter Plot
data_scatter <- data.frame(
  x = rnorm(100),
  y = rnorm(100)
)

ggplot(data_scatter, aes(x = x, y = y)) +
  geom_point(color = "steelblue") +
  theme_prism(base_size = base_size) +
  labs(title = "Scatter Plot") 

################## 5. Distribution (Density) Plot
data_dist <- data.frame(
  Value = c(rnorm(100, mean = 5), rnorm(100, mean = 6)),
  Group = rep(c("Group A", "Group B"), each = 100)
)

ggplot(data_dist, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  theme_prism(base_size = base_size) +
  labs(title = "Density Plot")

################## 6. Bubble Plot
data_bubble <- data.frame(
  x = rnorm(50),
  y = rnorm(50),
  size = runif(50, 1, 10),
  group = sample(c("A", "B"), 50, replace = TRUE)
)

ggplot(data_bubble, aes(x = x, y = y, size = size, color = group)) +
  geom_point(alpha = 0.6) +
  scale_size(range = c(2, 10)) +
  theme_prism(base_size = base_size) +
  labs(title = "Bubble Plot")

################## 7. Heatmap
# Simulate matrix
mat <- matrix(rnorm(100), nrow = 10)
rownames(mat) <- paste0("Gene", 1:10)
colnames(mat) <- paste0("Sample", 1:10)
df_heatmap <- melt(mat)

ggplot(df_heatmap, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "RdYlBu", direction = -1) +
  theme_prism(base_size = base_size) +
  labs(title = "Heatmap", x = "Sample", y = "Gene")
