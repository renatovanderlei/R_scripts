#### Here, we will calculate the CWM (Community Weighted Mean) of the functional traits of the species in each community.
# Then, we will perform a PCA (Principal Component Analysis) to reduce the dimensionality of the data.
# After that, we will perform a distance-based redundancy analysis (dbRDA) to test the effect of environmental variables on the functional traits of the species
# using a stepwise selection procedure.

#### We will use the package FD (Functional Diversity) to calculate the CWM.

#### First, we need to install the package FD and load it.
install.packages("FD")
library(FD)

# Now, Set your working directory
setwd("")

# Read trait data
traits <- read.table("your_traits.csv", sep = ";", row.names = 1, header = TRUE)

# Read community data
comm <- read.table("your_community_data.csv", sep = ";", dec = ',', row.names = 1, header = TRUE)

# Display structure of trait and community data
str(traits)
str(comm)

# Calculate Functional Diversity
div.func <- dbFD(x = traits, a = comm, w.abun = TRUE, stand.x = TRUE, stand.FRic = TRUE, m = 5)

# Calculate CWM for categorical dominants (e.g., dispersal syndromes).
# If you have more than one categorical dominant, you can use the function "functcomp" to calculate the CWM for each one.
# It returns a list with the CWM values for each category (e.g., CWM for species with zoochoric dispersal syndrome,
# CWM for species with anemochoric dispersal syndrome, etc.).
cwm.t1 <- functcomp(x = traits, a = as.matrix(comm), CWM.type = c("d"))

# Set column names for community data
colnames(comm) <- rownames(traits)

# Calculate overall Functional Diversity
cwm.all <- dbFD(traits, comm, w.abun = TRUE, stand.x = TRUE,
                ord = c("podani"), asym.bin = NULL,
                corr = c("sqrt"), calc.FRic = TRUE, m = "min", clust.type = "ward",
                calc.CWM = TRUE, CWM.type = c("d"), calc.FDiv = TRUE, dist.bin = 2,
                print.pco = TRUE, messages = TRUE)

# Display CWM values for specific types
cwm.t1$CWM
cwm.all$CWM

# Save CWM values to a CSV file
write.csv(cwm.t1$CWM, "CWM.csv")

#Now, let's normalize trait data and calculate Euclidean distance between them.
# First, we need to install the package vegan and load it.
install.packages("vegan")
library(vegan)

# Normalize trait data
att.norm <- decostand(traits, "normalize", MARGIN = 2, na.rm = TRUE)

# Calculate Euclidean distance between normalized traits
dist.att <- dist(att.norm, "euclidean")

#Now, we will perform a Principal Component Analysis (PCA) to reduce the dimensionality of the data.
# Generate biplot for Principal Component Analysis (PCA)
# First, we need to install the package factoextra and load it.
install.packages("factoextra")
library(factoextra)

# Perform Principal Component Analysis (PCA) and display biplot
biplot(prcomp(cwm, scale = TRUE))

# Display summary of PCA results
summary(prcomp(cwm, scale = TRUE))



# Now, we are goin to perform a distance-based redundancy analysis (dbRDA) to test the effect ####
# of environmental variables on the functional traits of the species.

#First, we need to upload the environmental data of each plot.
environmental_data<-read.csv("your_data.csv", sep=";", h=T, row.names=1)

# Define the global dbRDA model
dbrda.t <- capscale(dist.att ~ env1 + env2 + env3 + env4, dist = "euclidean", data = environmental_data)

# Check variance inflation factors (VIF)
vif.cca(dbrda.t)  # Ensure VIF is low (< 10)

# Perform ANOVA on the terms in the model
anova(dbrda.t, by = "term")

# Calculate Adjusted R-squared
RsquareAdj(dbrda.t)

########### Step Forward Selection ###########

# Perform step-forward selection
step_result <- ordistep(capscale(dist.att ~ 1, dist = "euclidean", environmental_data),
                      scope = formula(dbrda.t), direction = 'both', pstep = 1000)  # pstep = 1000 to avoid error

# Display the selected variables
selected_variables <- formula(stepwise_result$call)
print(selected_variables)

# Build a final model with the selected variables
final_model <- capscale(dist.att ~ selected_variables, dist = "euclidean", data = your_data)

# Display summary and results for the final model
summary(final_model)
anova(final_model, by = "term")
RsquareAdj(final_model)

