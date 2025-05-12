# %% Setup ==============================================

# Andrew 2025 05
# heterogeneous theorists

rm(list = ls())
library(here)
source(here("Code", "utils.r"))

# simulation parameters
n_theorist = 10000
Pr_good = 0.5
h = 2
seed = 1027

# clear separation
# n_ideas = 100; mu_sig = 2; ep_sig = 1; qbad = 0.01; qgood = 0.99

# less extreme qbad and qgood
n_ideas = 20; mu_sig = 2; ep_sig = 1; qbad = 0.1; qgood = 0.9

# ap optimal
# n_ideas = 100; mu_sig = 0.1; ep_sig = 1; qbad = 0.01; qgood = 0.99


# plot parameters
nplot <- 500  # number of points for scatterplot

#%% Simulate  ====================

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- simulate_fund(n_ideas, n_theorist, Pr_good, mu_sig, ep_sig, qbad, qgood, h)

#%% Create histograms ====================
source(here("Code", "utils.r")) # for easy plot edits

# = histogram plot edits =
ANNOTATE_TEXT_SIZE = 5
ANNOTATE_COLOR = MATRED
ANNOTATE_LINE_SIZE = 1
ANNOTATE_VJUST = 3.0
plot_edits_muhat = list(
    xlab(expression("Measured Quality " * hat(mu)[paste("i*")])),
    geom_vline(xintercept = h, color = ANNOTATE_COLOR, size = ANNOTATE_LINE_SIZE), 
    annotate("text",
      x = h+0.2, y = Inf, label = "published ->",
      vjust = ANNOTATE_VJUST, hjust = 0, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    ),
    annotate("text",
      x = h-0.2, y = Inf, label = "<- unpublished",
      vjust = ANNOTATE_VJUST, hjust = 1, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    ),    
    theme(
      legend.position = c(05,80)/100,
      axis.text = element_text(size = 14),
    ),
    scale_x_continuous(breaks = seq(-20, 20, 2))
)

plot_edits_mu = function(Emu) {
  list(
    xlab(expression("Actual Quality " * mu[paste("i*")])),
    geom_vline(xintercept = Emu, color = ANNOTATE_COLOR, size = ANNOTATE_LINE_SIZE), 
    annotate("text",
      x = Emu, y = Inf, label = "mean",
      vjust = ANNOTATE_VJUST, hjust = 1.1, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    )
  )
} # end of plot_edits_mu

# = 1) muhat histogram =

# Create histogram data
hist_data <- create_histogram_data(sim$litplus, "muhat", binwidth = 0.5)

# Create plots for both methods
plots <- list()
for (method_name in c("ap", "ph")) {
  plots[[method_name]] <- hist_data$data %>%
    filter(method == method_name) %>%
    plot_histogram(xlimnum = hist_data$binlimit, ylimnum = hist_data$prop_range) +
    theme(legend.position = c(7, 9) / 10) + plot_edits_muhat
}

# Save individual plots for paper
ggsave(
  paste0(here("Results", "litplus-muhat"), "-ap.pdf"),
  plots$ap,
  width = 8, height = 4, scale = 1.0,
  device = cairo_pdf
)
ggsave(
  paste0(here("Results", "litplus-muhat"), "-ph.pdf"),
  plots$ph,
  width = 8, height = 4, scale = 1.0,
  device = cairo_pdf
)

# = 2) mu histogram =

# Create histogram data for filtered data
hist_data <- create_histogram_data(sim$litplus, "mu", filter_condition = quote(muhat > h), binwidth = 0.05)

Emu_ap = sim$litplussum$Emu[sim$litplussum$method == "ap" & sim$litplussum$hurdle == "h"]
Emu_ph = sim$litplussum$Emu[sim$litplussum$method == "ph" & sim$litplussum$hurdle == "h"]

# Create plots for both methods
lit_plots <- list()
methods <- c("ap", "ph")
Emus <- c(Emu_ap, Emu_ph)
for (i in seq_along(methods)) {
  lit_plots[[methods[i]]] <- hist_data$data %>%
    filter(method == methods[i]) %>%
    plot_histogram(xlimnum = hist_data$binlimit, ylimnum = hist_data$prop_range) +
    theme(legend.position = c(7, 9) / 10) + plot_edits_mu(Emus[i])
}

# Save individual plots for paper
ggsave(
  paste0(here("Results", "lit-mu"), "-ap.pdf"),
  lit_plots$ap,
  width = 8, height = 4, scale = 1.0,
  device = cairo_pdf
)
ggsave(
  paste0(here("Results", "lit-mu"), "-ph.pdf"),
  lit_plots$ph,
  width = 8, height = 4, scale = 1.0,
  device = cairo_pdf
)

# Save combined plot for testing
p_testing <- gridExtra::arrangeGrob(
  plots$ap + ggtitle("a priori") + theme(legend.position = "none"),
  lit_plots$ap + ggtitle("a priori") + theme(legend.position = "none"),
  plots$ph + ggtitle("post hoc") + theme(legend.position = "none"),
  lit_plots$ph + ggtitle("post hoc") + theme(legend.position = "none"),
  ncol = 2
)

ggsave(
  here("Results", "hist-testing.pdf"),
  p_testing,
  width = 8, height = 8, scale = 1.0,
  device = cairo_pdf
)
