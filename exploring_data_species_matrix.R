#### Exploratory Data Analyses ####
# Loading necessary library
require(vegan)

# In this initial tutorial, we will use data embedded in the vegan package. 
# Please replicate these steps with your own data.

# The first step in any analysis is to understand the data. The best way to do this is by visualizing it
# through various plotting techniques.

# EXPLORING SPECIES ABUNDANCE MATRICES ####

# Step 1 - Import your species abundance matrices

# Step 2 - Explore the dimensions of each species abundance matrix

data(dune)  # Importing embedded data in the vegan package.
dim(dune)   # Returns the dimensions (number of rows x number of columns) of the species abundance matrix
nrow(dune)  # Returns the number of rows in the species abundance matrix
ncol(dune)  # Returns the number of columns in the species abundance matrix

# Step 3 - Histogram to explore the richness and dominance of species across plots

# Here, we will construct two histograms to visualize (1) the distribution of species richness and (2) the dominance
# of species (as a percentage) across plots.

# The rowSums function sums the number of species (rows) that have a non-zero value (i.e. > 0) in each plot (column).
richness = rowSums(dune > 0)  # Calculates the species richness for each plot. 

#visualize the richness of each plot
richness

# Histogram, where the x-axis indicates the number of species per plot and the y-axis indicates the frequency of plots.
hist(riqueza, 10, col = 'green', xlab = 'Species richness', cex.lab = 1.2, main = '') 

# Frequency of species (as a percentage) across plots.
freq_sp = 100 * colSums(dune > 0) / nrow(dune)

freq_sp

#Finally, let's create a histogram with the frequency of species across plots.
hist(freq_sp, 10, col = 'yellow', xlab = 'Species prevalence (%)', cex.lab = 1.2, main = '')
