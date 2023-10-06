library(tidyr)
library(ggridges)

df = read.csv('/Users/djw/Downloads/data_clean.csv')

# conver to long
dfl = gather(df, condition, measurement, bounded_rationality:autonomous_transparent, factor_key=TRUE)

theme_set(theme_minimal())

# Number of bootstrap replicates
num_replicates <- 1000

# Create an empty list to store the bootstrap samples
bootstrap_samples <- vector("list", num_replicates)

# Perform bootstrap resampling
for (r in 1:num_replicates) {
  bootstrap_samples[[r]] <- dfl[sample(nrow(dfl), replace = TRUE), ]
}

# Combine the bootstrap samples into a larger dataframe
larger_df <- do.call(rbind, bootstrap_samples)

# nudge
ggplot(
  dfl[dfl$intervention == 'nudge',], 
  aes(x = measurement, y = condition, fill=stat(x))
) +
  xlim(0, 10) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C", limits = c(0, 10)) +
  labs(title = 'Nudge') 

# Boost 1
ggplot(
  dfl[dfl$intervention == 'nudge+flowchart',], 
  aes(x = measurement, y = condition, fill=stat(x))
) +
  xlim(0, 10) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C", limits = c(0, 10)) +
  labs(title = 'Boost1') 

# Boost 2
ggplot(
  dfl[dfl$intervention == 'nudge+friction+feedback',], 
  aes(x = measurement, y = condition, fill=stat(x))
) +
  xlim(0, 10) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C", limits = c(0, 10)) +
  labs(title = 'Boost2') 
