# %% Setup ==============================================

# Andrew 2025 05
# Simulates many het models

rm(list = ls())
library(here)
source(here("Code", "utils.r"))

seed = 1027

# baseline ap optimal parameters
par0 = tibble(
  # required parameters in order
  n_ideas = 100,
  n_theorist = 10000,
  Pr_good = 0.5,
  mu_sig = 0.1,
  ep_sig = 1,
  qbad = 0.01,
  qgood = 0.99,
  # optional parameters
  h = 2
)

# #%% Test simulation  

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- do.call(simulate_fund, as.list(par0))

sim$litplussum 

# = histogram plot edits =
ANNOTATE_TEXT_SIZE = 5
ANNOTATE_COLOR = MATRED
ANNOTATE_LINE_SIZE = 1
ANNOTATE_VJUST = 3.0
h = par0$h
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
    )
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

# Save combined plot for testing
p_testing <- gridExtra::arrangeGrob(
  plots$ap + ggtitle("a priori") + theme(legend.position = "none"),
  lit_plots$ap + ggtitle("a priori") + theme(legend.position = "none"),
  plots$ph + ggtitle("post hoc") + theme(legend.position = "none"),
  lit_plots$ph + ggtitle("post hoc") + theme(legend.position = "none"),
  ncol = 2
)

ggsave(
  here("Results", "zzz-many-het-testing.pdf"),
  p_testing,
  width = 8, height = 8, scale = 1.0,
  device = cairo_pdf
)


#%% Simulate many  ====================

# make a grid of parameter values
manyset = expand_grid(
  # ep_sig = seq(1, 0.5, length.out = 2),
  # qgood = seq(0.99, 0.10, length.out = 5),
  mu_sig = seq(0.10, 2.00, length.out = 5)
) %>% 
  mutate(setid = 1:n())


# simulate for each set
manysum = foreach(setid = 1:nrow(manyset), .combine = rbind) %do% {
    print(paste0("setid: ", setid, " of ", nrow(manyset)))    
   
    # Add parameters from manyset
    temppar = par0
    update_me = manyset %>% select(-setid) %>% names()
    for (col in update_me) { 
        temppar[1, col] = manyset[setid, col]
    }
    
    # Call simulate_fund with all parameters
    sim <- do.call(simulate_fund, as.list(temppar))

    litplussum = sim$litplussum %>% mutate(setid = setid)
    
    return(litplussum)
}

manysum = manysum %>% left_join(manyset, by = "setid")

# test output
varselect = 'mu_sig'
manysum %>% filter(hurdle == 'h') %>% 
  select(setid, !!sym(varselect), method, Emu, Pr_good, N_good) %>%  
  pivot_wider(names_from = method, values_from = c(Emu, Pr_good, N_good), names_glue = "{method}_{.value}") %>% 
  mutate(ph_delta = ph_Emu - ap_Emu) %>% 
  select(setid, !!sym(varselect), ph_delta, everything()) 


#%% End ==============================
print('many_het_model.r complete')
