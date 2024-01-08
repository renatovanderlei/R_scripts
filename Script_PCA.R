# Load necessary packages
library(gridExtra)
library(FactoMineR)
library(factoextra)

#### Data ####

# Read the CSV file with functional traits
sps_data <- read.csv("traits_sp_sem_mf.csv", sep = ";", row.names = 1)
sps_data
summary(sps_data) # All good?

### PCA of Continuous Environmental Variables  ####

# Display variable names
names(sps_data)

# Perform PCA on continuous environmental variables
sps_pca <- PCA(sps_data, scale.unit = TRUE, graph = TRUE)

# Percentage of explanation for each axis
sps_pca$eig

# Scree plot to visualize the explanation of the first two axes
fviz_screeplot(sps_pca, addlabels = TRUE, ylim = c(0, 70))

# Get variables' contributions to each axis
var_env <- get_pca_var(sps_pca)
var_env$contrib

# Get individuals' contributions to each axis
ind_env <- get_pca_ind(sps_pca)
ind_env$contrib

### Biplot ###

# Create a biplot for PCA
pca_sps <- fviz_pca_biplot(sps_pca, repel = TRUE, select.var = list(cos2 = 12), col.var = "black", 
                           geom = "text", pointsize = 1, mean.point = FALSE, arrowsize = 1) + 
  theme_gray()

# Customize the biplot
pca_sps <- pca_sps + xlab("PC 1 (21.1%)") + ylab("PC 2 (19.2%)")
pca_sps <- pca_sps + theme(axis.title.x = element_text(face = "bold", size = 14),
                           axis.text.x = element_text(vjust = 0.5, size = 12))
pca_sps <- pca_sps + theme(axis.title.y = element_text(face = "bold", size = 14),
                           axis.text.y = element_text(vjust = 0.5, size = 12))

# Save the biplot as PDF and PNG
ggsave("pca_renato.pdf", height = 14, width = 14, dpi = 900, units = "cm")
ggsave("pca_renato.png", height = 14, width = 14, dpi = 900, units = "cm")
