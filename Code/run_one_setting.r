# %% Setup ==============================================

# Andrew 2025 05
# trying to understand the simulation better

rm(list = ls())
library(tidyverse)
library(data.table)
source("utils.r")

# simulation parameters
n_theorist = 10000
Pr_good = 0.5
h = 2
seed = NULL

# nice for scatterplot
# n_ideas = 100; mu_sig = 2; ep_sig = 1; qbad = 0.5; qgood = 0.5

# clear separation
# n_ideas = 100; mu_sig = 2; ep_sig = 1; qbad = 0.01; qgood = 0.99

# ap optimal
n_ideas = 100; mu_sig = 0.1; ep_sig = 1; qbad = 0.01; qgood = 0.99

# plot parameters
nplot <- 500  # number of points for scatterplot

# histogram plot edits
plot_edits_muhat = list(
    xlab(expression("Measured Quality " * hat(mu)[paste("i*")])),
    geom_vline(xintercept = h, color = MATRED), 
    annotate("text",
      x = h, y = Inf, label = "statistical\nhurdle",
      vjust = 1.2, hjust = 1.1, size = 3, color = MATRED
    )
)

plot_edits_mu = function(Emu) {
  list(
    xlab(expression("Actual Quality " * mu[paste("i*")])),
    geom_vline(xintercept = Emu, color = MATRED), 
    annotate("text",
      x = Emu, y = Inf, label = "mean",
      vjust = 1.2, hjust = 1.1, size = 3, color = MATRED
    )
)}

#%% Simulate  ====================

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- simulate_fund(n_ideas, n_theorist, Pr_good, mu_sig, ep_sig, qbad, qgood, h)

#%% Scatterplot  ====================


plotme <- sim$litplus %>% slice_sample(n = nplot)
xylim <- range(c(plotme$mu, plotme$muhat))

# Define method aesthetics
method_aes <- tibble(
  method = c("ap", "ph"),
  label = c("a priori", "post hoc"),
  shape = c(16, 8),
  color = c(MATBLUE, MATORANGE)
)

pscatter <- plotme %>%
  ggplot(aes(x = mu, y = muhat)) +
  geom_abline(slope = 1) +
  geom_point(aes(shape = method, color = method), size = 3) +
  theme_minimal() +
  scale_shape_manual(
    values = setNames(method_aes$shape, method_aes$method),
    labels = setNames(method_aes$label, method_aes$method)
  ) +
  scale_color_manual(
    values = setNames(method_aes$color, method_aes$method),
    labels = setNames(method_aes$label, method_aes$method)
  ) +
  labs(
    x = expression("Actual Quality " * mu[paste("i*")]),
    y = expression("Measured Quality " * hat(mu)[paste("i*")]),
    shape = "Method",
    color = "Method"
  ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 06))
  )

# save
ggsave("../Results/scatter.pdf", pscatter, width = 8, height = 4, device = cairo_pdf, scale = 1.2)

#%% litplus muhat histogram  ====================

# Create histogram data
hist_data <- create_histogram_data(sim$litplus, "muhat")

# Create plots for both methods
plots <- list(
  ap = plot_idea_hist(hist_data$data, "ap", hist_data$binlimit, hist_data$prop_range) + plot_edits_muhat,
  ph = plot_idea_hist(hist_data$data, "ph", hist_data$binlimit, hist_data$prop_range) + plot_edits_muhat
)

# Save plots
save_plots(plots, "../Results/litplus-muhat")

#%% lit mu histogram  ==================

# Create histogram data for filtered data
hist_data <- create_histogram_data(sim$litplus, "mu", filter_condition = quote(muhat > h))

Emu_ap = sim$litplussum$Emu[sim$litplussum$method == "ap" & sim$litplussum$hurdle == "h"]
Emu_ph = sim$litplussum$Emu[sim$litplussum$method == "ph" & sim$litplussum$hurdle == "h"]

# Create plots for both methods
lit_plots <- list(
  ap = plot_idea_hist(hist_data$data, "ap", hist_data$binlimit, hist_data$prop_range) + 
    plot_edits_mu(Emu_ap),
  ph = plot_idea_hist(hist_data$data, "ph", hist_data$binlimit, hist_data$prop_range) + 
    plot_edits_mu(Emu_ph)
)

# Save plots
save_plots(lit_plots, "../Results/lit-mu")
