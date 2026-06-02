library(fishprior)
library(rfishbase)
library(dplyr)

priors <- data.frame("prior" = c("Fixed", "Hoenig 1983", "Hamel-Cope 2022"),
                     "median" = c(0.2, 0.2, 0.22),
                     "log_sd" = c(NA, 0.1, 0.31))
# Hoenig 1983: median 0.2 and sd (log space) of 0.1
# Hamel-Cope 2022 uses lognormal median 0.22 and sd (log space) of 0.31

# As a first pass we'll use all species of the Merluccius genus

hakes_list <- rfishbase::species_names() |>
  dplyr::filter(Family == "Merlucciidae")

# Extract trait data in fishprior
trait_data <- fishprior::get_fishbase_traits(spec_names = hakes_list$Species)

# Sex is categorized as female, male, mixed, unsexed --
# here we use everything
trait_data_m <- trait_data |>
  dplyr::filter(trait == "M") |>
  dplyr::group_by(Species) |>
  dplyr::summarise(
    median_M = median(value),
    n_obs = n(),
    log_M = log(median_M)
  ) |>
  # Exclude Grenadiers and Pouting
  filter(!Species %in% c("Roundnose grenadier", "Pouting", "Poor cod", "Common mora"))

# calculate weighted mean (mu) and weighted SD (sigma) for the Lognormal
# use log(n_obs + 1) to smooth the weights
weights <- log(trait_data_m$n_obs + 1)

mu_prior <- weighted.mean(trait_data_m$log_M, w = weights)

# weighted SD for the prior
sigma_prior <- sqrt(weighted.mean((trait_data_m$log_M - mu_prior)^2, w = weights))
mu_prior <- exp(mu_prior)

priors <- rbind(priors,
                data.frame(prior = "Family Merlucciidae",
                           median = mu_prior,
                           log_sd = sigma_prior))


####################################################################

gadids_list <- rfishbase::species_names() |>
  dplyr::filter(Order == "Gadiformes")

# Extract trait data in fishprior
trait_data <- fishprior::get_fishbase_traits(spec_names = gadids_list$Species)

# Sex is categorized as female, male, mixed, unsexed --
# here we use female and mixed/unsexed
trait_data_m <- dplyr::filter(trait_data, trait=="M")

# Define the informed list
similar_spp <- c("Alaska pollock", "Atlantic cod", "Pacific cod", "Blue whiting",
           "Southern blue whiting", "Blue grenadier", "Saithe", "Haddock",
           "Whiting", "Blue ling", "Ling", "White hake", "Burbot")
trait_data_m <- dplyr::filter(trait_data_m, Species %in% similar_spp)

trait_data_m <- dplyr::group_by(trait_data_m, Species) |>
  dplyr::summarise(
    median_M = median(value),
    n_obs = n(),
    log_M = log(median_M)
  )

weights <- log(trait_data_m$n_obs + 1)

mu_prior <- weighted.mean(trait_data_m$log_M, w = weights)

# weighted SD for the prior
sigma_prior <- sqrt(weighted.mean((trait_data_m$log_M - mu_prior)^2, w = weights))
mu_prior <- exp(mu_prior)

priors <- rbind(priors,
                data.frame(prior = "Order Gadiformes",
                           median = mu_prior,
                           log_sd = sigma_prior))
