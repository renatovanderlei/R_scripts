###GLM for Poisson / Negative-Binomial families

# Load the required library
library(performance)

# Fit the Poisson regression model
model_primroots <- glm(n_prim_roots ~ habitats, family = poisson(link = "log"), data = dados.parcela)
summary(model_primroots)

# Checking for overdispersion
check_overdispersion(model_primroots)  # High dispersion ratio observed

# Adjusting the model using negative binomial family
model_primroots <- glm.nb(n_prim_roots ~ habitats, link = "log", data = dados.parcela)
summary(model_primroots)
check_overdispersion(model_primroots)  # Dispersion ratio substantially reduced, but a notable outlier exists

# Visualize the plotted fitted values vs residuals
par(mfrow = c(2, 2))
plot(model_primroots)

# Assess the results using Anova
Anova(model_primroots)  # X²=10.93, df=2, p=0.004

# Differences between habitats exist, conduct pairwise comparison
nroots.tukey <- glht(model_primroots, mcp(habitats = "Tukey"))
summary(nroots.tukey)  # Recently abandoned fields have a lower number of primary roots than the other 2 habitats
rsq(model_primroots, adj = TRUE)  # Adjusted r² = 0.14

# Visualize the data using a boxplot
prim_roots <- ggplot(dados.parcela) +
  aes(x = habitats, y = n_prim_roots) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(x = "Habitats", y = "Number of primary roots") +
  theme_classic() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))
prim_roots
