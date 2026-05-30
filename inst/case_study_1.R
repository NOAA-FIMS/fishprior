# florida pompano Trachinotus carolinus case study

  library(tidyverse)
  library(fishprior)
  library(tidybayes)
  library(brms)
  library(ggridges)
  
  path <- getwd()
  
  # pull pompano trait data
  species_list <- c("Trachinotus carolinus")
  
  dat <- get_fishbase_traits(spec_names = species_list)

  # plot number of studies per trait
	dat |>
  	dplyr::filter(SE > 0) |>
    dplyr::group_by(trait) |>
    dplyr::summarise(value = dplyr::n()) |>
    ggplot(aes(trait, value)) + geom_bar(stat = "identity") + 
    labs(x = "", y = "Sample size",
         title="Reported sample sizes") + 
    theme_bw()
  
	# filter by growth coefficient
	filtered_traits <- dat |>
  	dplyr::filter(trait %in% c("K"),
    SE > 0,
    Number > 0) 
 
	filtered_traits$study_id <- seq_len(nrow(filtered_traits))

 
  ######################################
  # Calculate/Estimate means ###########
  ######################################
  
  #### arithmetic mean k #### first row in table
  mean_k <- mean(filtered_traits$value)
	sd_k <- sd(filtered_traits$value)
	se_k = sd_k/(sqrt(nrow(filtered_traits)))
	
	#### Weighting based on reported sample size ####
	
	#### simple weighted mean #### second row in table
	weighted_summary <- filtered_traits |>
    dplyr::summarise(
      weighted_mean = weighted.mean(value, w = Number),
      weighted_var = sum(Number * (value - weighted_mean)^2) / sum(Number),
      weighted_se = sqrt(weighted_var / dplyr::n()),
      total_n = sum(Number),
      n_studies = dplyr::n())
	
	#### model-based weighted mean #### third row in table
	
	filtered_traits$sqrt_n <- 1 / sqrt(filtered_traits$Number)
	filtered_traits$study_id <- seq_len(nrow(filtered_traits))
	filtered_traits$log_n <- log(filtered_traits$value)

	formula <- bf(
	  log_n ~ 1,
	  sigma ~ 0 + sqrt_n)
	
	fit_WM <- brm(
	  formula,
	  data = filtered_traits,
	  family = gaussian(),
	  chains = 4,
	  iter = 5000,
	  control = list(adapt_delta = 0.999)
	)
  
	 draws_WM <- fit_WM |>
  	spread_draws(b_Intercept)
 
  #### Inverse variance weighted mean approaches ####
  
  # Model-based inverse variance weighted mean (known standard errors only) # 4th row in table
  fit_IVWM <- brm(
    value | se(SE) ~ 1,
    data = filtered_traits,
    prior = prior(normal(0, 10), class = Intercept),
    chains = 4, iter = 2000,
    control = list(adapt_delta = 0.999))

  draws_IVWM <- fit_IVWM |>
   spread_draws(b_Intercept)
 
	# Model-based inverse variance weighted mean with residual error (known standard errors with residual error) # 5th row in table
  fit_IVWMRE <- brm(
  	value | se(SE, sigma = TRUE) ~ 1, 
  	data = filtered_traits,
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(exponential(1), class = sigma)))

  
 # # Model-based hierarchical with residual error (known standard errors with residual variance and random effect) #
#	fit_HRE <- brm(
#		value | se(SE, sigma = TRUE) ~ 1 + (1 | study_id), 
#		data = filtered_traits,
 #   prior = c(prior(normal(0, 10), class = Intercept),
 #   					prior(exponential(1), class = sigma),
 #   					prior(exponential(1), class = sd)),
 # 	chains = 4, iter = 2000,
 # 	control = list(adapt_delta = 0.999))
 # 
#
#	# known standard error + random effect of study # HAD THIS ONE
#	fit_RE <- brm(
#		value | se(SE) ~ 1 + (1 | study_id), 
#		data = filtered_traits,
#	  prior = prior(normal(0, 10), class = Intercept),
#		chains = 4, iter = 2000,
 # 	control = list(adapt_delta = 0.999))
	
  ## density plot
  
  model_list <- list(
  	"Model-based\nweighted mean" = fit_WM,
  	"Model-based\ninverse variance\nweighted mean" = fit_IVWM,
  #	"Model-based\ninverse variance\nweighted mean\nwith study\nrandom effect" = fit_RE,
  	"Model-based\ninverse variance\nweighted mean\nwith residual error" = fit_IVWMRE) #,
  #	"Model-based\nhierarchical with\nresidual error" = fit_HRE)

  draws <- purrr::imap_dfr(model_list, \(mod, model_name) {
  					mod |>
  					  spread_draws(b_Intercept) |>
  					  mutate(model = model_name)
	})
  
 draws <- draws |>
  mutate(
    b_Intercept = if_else(
      model == "Model-based\nweighted mean",
      exp(b_Intercept),
      b_Intercept
    )
  )
  
	other_mods <- tibble::tibble(
  	model = c("Arithmetic\nmean", "Weighted\nmean"),
  	estimate = c(mean_k, weighted_summary$weighted_mean)) |>
  	dplyr::mutate(
    	upper = c(
      	mean_k + (1.96 * se_k),
      	weighted_summary$weighted_mean + (1.96 * weighted_summary$weighted_se)),
  		lower = c(
      	mean_k - (1.96 * se_k),
      	weighted_summary$weighted_mean - (1.96 * weighted_summary$weighted_se))
  	)

  model_order <- c(
  	"Arithmetic\nmean",
  	"Weighted\nmean",
  	"Model-based\nweighted mean",
  	"Model-based\ninverse variance\nweighted mean",
  #	"Model-based\ninverse variance\nweighted mean\nwith study\nrandom effect",
  	"Model-based\ninverse variance\nweighted mean\nwith residual error") #,
  #	"Model-based\nhierarchical with\nresidual error")

	draws <- draws |>
  	mutate(model = factor(model, levels = model_order))

	other_mods <- other_mods |>
	  mutate(model = factor(model, levels = model_order))

	posterior_sum <- draws |>
	  group_by(model) |>
	  summarise(
	    estimate = median(b_Intercept, na.rm = TRUE),
	    lower = quantile(b_Intercept, 0.025, na.rm = TRUE),
	    upper = quantile(b_Intercept, 0.975, na.rm = TRUE),
	    .groups = "drop")
	
	
	# no outline on distributions
	p <- 
	ggplot(draws, aes(x = b_Intercept, y = model, fill = after_stat(x))) +
  geom_point(
    data = other_mods,
    aes(x = estimate, y = model),
    inherit.aes = FALSE,
    size = 3) +
  geom_errorbarh(
    data = other_mods,
    aes(x = estimate, xmin = lower, xmax = upper, y = model),
    inherit.aes = FALSE,
    height = 0.15,
    linewidth = 0.8) +
  stat_density_ridges(
  	fill = "#4C9FBF",
  	alpha = 0.9,
  	color = NA,
  	jittered_points = FALSE,
  	scale = 0.8,
  	bandwidth = 0.01,
  	quantile_lines = TRUE,
  	quantiles = 2,
  	vline_color = "black") +
  scale_fill_viridis_c(name = "Intercept") +
  scale_y_discrete(limits = rev(model_order)) +
  coord_cartesian(xlim = c(0, 1)) +
  labs(
    x = "Posterior intercept",
    y = NULL) +
  ggsidekick::theme_sleek() +
  theme(legend.position = "none")
	
	
 
    ggsave(file = paste0(path, "/density_plot.png"), p, 
    			 dpi = 300, height = 5, width = 9, units = "in")
    
    #### posterior means and 95% CI ####
		means <- draws |>
		  dplyr::group_by(model) |>
		  dplyr::summarise(
		    mean_post = mean(b_Intercept),
		    se_post   = sd(b_Intercept),
		    lower     = mean_post - (1.96 * se_post),
		    upper     = mean_post + (1.96 * se_post))

	p <- 
		ggplot(draws, aes(x = b_Intercept, y = model, fill = after_stat(x))) +
  	geom_point(
  	  data = other_mods,
  	  aes(x = estimate, y = model),
  	  inherit.aes = FALSE,
  	  size = 3 ) +
  	geom_errorbarh(
  	  data = other_mods,
  	  aes(x = estimate, xmin = lower, xmax = upper, y = model),
  	  inherit.aes = FALSE,
  	  height = 0.15,
  	  linewidth = 0.8) +
  	stat_density_ridges(
  	  fill = "#4C9FBF",
  	  alpha = 0.9,
  	  color = NA,
  	  jittered_points = FALSE,
  	  scale = 0.8,
  	  bandwidth = 0.01) +
  	geom_point(
  	  data = means,
  	  aes(x = mean_post, y = model),
  	  color = "black",
  	  size = 3) +
  	geom_errorbarh(
  	  data = means,
  	  aes(x = mean_post, xmin = lower, xmax = upper, y = model),
  	  height = 0.12,
  	  linewidth = 0.7,
  	  color = "black") +
  	scale_fill_viridis_c(name = "Intercept") +
  	scale_y_discrete(limits = rev(model_order)) +
  	coord_cartesian(xlim = c(0, 1)) +
  	labs(
  	  x = "Posterior intercept",
  	  y = NULL) +
  	ggsidekick::theme_sleek() +
  	theme(legend.position = "none") +
		theme(
			axis.text = element_text(size = 14)
		)
	
	  ggsave(file = paste0(path, "/density_plot.png"), p, 
    			 dpi = 300, height = 5, width = 9, units = "in")
   
  	  
#  p <- 
#		ggplot(draws, aes(x = b_Intercept, y = model, fill = after_stat(x))) +
#  	geom_point(data = other_mods,
#    	aes(x = estimate, y = model),
#    	inherit.aes = FALSE, size = 3) +
#  	geom_errorbarh(
#  		data = other_mods,
#  		aes(x = estimate, xmin = lower, xmax = upper, y = model),
#  		inherit.aes = FALSE, height = 0.15, linewidth = 0.8) +
#		stat_density_ridges(
#		 fill = "#4C9FBF", alpha = 0.9, color = "black", linewidth = 0.3,      
#		 scale = 0.8, bandwidth = 0.01, quantile_lines = TRUE, quantiles = 2) +
#		scale_fill_viridis_c(name = "Intercept") +
#		scale_y_discrete(limits = rev(model_order)) +
#		coord_cartesian(xlim = c(-0.8, 1.3)) +
#		labs(x = "Posterior intercept",
#		 		 y = NULL) +
#  	ggsidekick::theme_sleek() +
#  	theme(legend.position = "none")
#  
#    ggsave(file = paste0(path, "/density_plot.png"), p, 
#    			 dpi = 300, height = 5, width = 9, units = "in")
 
 

#  ### old histogram plot
#  
#   
#  hist <- 
#    filtered_traits |>
#    ggplot(aes(value)) + 
#    geom_histogram() + 
#    labs(x = "Asymptotic length (cm)", y = "Count", color = "Mean type") + 
#    theme_bw() + 
#    geom_vline(aes(xintercept = weighted_summary$weighted_mean,
#                   color = "Weighted mean")) +
#    geom_vline(aes(xintercept = mean_l,
#                   color = "Arithmetic mean")) +
#    geom_vline(aes(xintercept = mean_mb,
#                   color = "Model-based mean")) +
#    geom_vline(aes(xintercept = mean_ma_noRE,
#                   color = "Meta-analytic mean no RE")) +
#    geom_vline(aes(xintercept = mean_ma_RV,
#                   color = "Meta-analytic mean RV")) +
#    geom_vline(aes(xintercept = mean_ma_RE,
#                   color = "Meta-analytic mean RE")) +
#    geom_vline(aes(xintercept = mean_ma_RERV,
#                   color = "Meta-analytic mean RERV")) +
#    scale_color_manual(values = c(
#      "Weighted mean" = "blue",
#      "Arithmetic mean" = "darkgreen",
#      "Model-based mean" = "gold",
#      "Meta-analytic mean no RE" = "purple",
#      "Meta-analytic mean RV" = "grey",
#      "Meta-analytic mean RE" = "black",
#      "Meta-analytic mean RERV" = "red")) +
#    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
#    ggsidekick::theme_sleek() +
#    theme(
#      legend.text = element_text(size = 10),
#      legend.title = element_text(size = 12),
#      legend.position = "inside",
#      legend.position.inside = c(0.95, 0.95),
#      legend.justification = c(1, 1)) 
#  
#  ggsave(file = here("CS2_hist.png"), hist, dpi = 300)
#  
# # summary <-  tibble(
# #     arithmetic_mean = mean(filtered_traits$value),
# #     weighted_mean = weighted_summary$weighted_mean,
# #     model_based =   mean_mb <- exp(mean(draws_mb$b_Intercept)),
# #     meta_analytic =  mean(draws_ma$b_Intercept), 
# #     total_n = sum(filtered_traits$Number),
# #     n_studies = nrow(filtered_traits)
# #   )
#  
#  mean_lines <- draws |>
#  group_by(model) |>
#  summarise(
#    mean_est = mean(b_Intercept, na.rm = TRUE),
#    .groups = "drop"
#  )#