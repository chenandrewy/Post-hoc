#%% Setup ==============================================

# Andrew 2025 05
# Simulates many het models
rm(list = ls())

library(here)
source(here('Code', 'utils.r'))

seed = 1027

# baseline ap optimal parameters
# parameter order must match head (simulate_fund)
# for n_theorist = 100e3, and 30 sims, takes 5 minutes
par0 = tibble(
  # fixed
  n_ideas = 100, n_theorist = 100e3, Pr_good = 0.5,
  mu_sig = 0.5, ep_sig = 1,
  qbad = 0.01, qgood = 0.99,
  # optional parameters
  h = 2
)

# #%% Test simulation  

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- do.call(simulate_fund, as.list(par0) %>% c(return_fund = TRUE))


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
  here('Results', 'many-par0-check.pdf'),
  p_testing,
  width = 8, height = 8, scale = 1.0,
  device = cairo_pdf
)


#%% Simulate many  ====================

num_sim = 30

# make a grid of parameter values
manyset = expand_grid(
  # qgood = seq(0.99, 0.10, length.out = 5),
  mu_sig = seq(par0$mu_sig, sqrt(50)*par0$mu_sig, length.out = num_sim)
  # n_ideas = seq(par0$n_ideas, 1000, length.out = num_sim) %>% round()
) %>% 
  mutate(setid = 1:n())

# simulate for each set
litplussum_list <- list()
prop2_list <- list()

# Start timing
start_time <- Sys.time()

for (setid in 1:nrow(manyset)) {
    # Calculate elapsed time and estimate remaining time
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    avg_time_per_iter <- elapsed_time / (setid - 1) 
    remaining_time <- avg_time_per_iter * (nrow(manyset) - setid)
    
    print(paste0(
      "setid: ", setid, " of ", nrow(manyset), ", ", manyset %>% select(-setid) %>% names(), 
      " = ", manyset[setid, 1] %>% round(2),
      " (Est. remaining: ", round(remaining_time, 1), " mins)"
    ))
    
    # Add parameters from manyset
    temppar = par0
    update_me = manyset %>% select(-setid) %>% names()
    for (col in update_me) { 
        temppar[1, col] = manyset[setid, col]
    }

    # Call simulate_fund with all parameters
    sim <- do.call(simulate_fund, as.list(temppar))

    litplussum_list[[setid]] <- sim$litplussum %>% mutate(setid = setid)
    prop2_list[[setid]] <- sim$prop2 %>% mutate(setid = setid)
} # end for setid

#%% Combine and clean ==============================

litplussum = bind_rows(litplussum_list)
prop2 = bind_rows(prop2_list)

# add parameters to prop2 and litplussum
prop2 = prop2 %>% left_join(manyset, by = "setid") %>% 
  select(all_of(manyset %>% names()), everything())
litplussum = litplussum %>% left_join(manyset, by = "setid") %>% 
  select(all_of(manyset %>% names()), everything()) 

# add a few items to prop2
prop2 = prop2 %>% 
  left_join(
    litplussum %>% filter(hurdle == 'h', type == 'any') %>% 
      select(setid, method, n, Emu) %>% 
      pivot_wider(names_from = method, values_from = c(n, Emu)),
    by = "setid"
  )

#%% Plot 

xvar = manyset %>% select(-setid) %>% names()

plotme = prop2 %>% filter(mu_sig < 3)

# where dlearn = slearn
x0 = prop2 %>% arrange(abs(dlearn - slearn)) %>% slice(1) %>% pull(!!xvar)

learn_aes = tibble(
  method = c("slearn", "dlearn"),
  label = c("Statistical Learning", "Darwinian Learning"),
  shape = c(16, 8),
  color = c(MATBLUE, MATORANGE),
  linetype = c("solid", "dashed")
)

plotedits = list(
  xlab(expression('Standard Deviation of Actual Idea Quality ' * mu[i]))
)



p1 = plotme %>% 
  mutate(pct_delta_ph = 100*(Emu_ph - Emu_ap)/Emu_ap) %>% 
  select(!!xvar, pct_delta_ph) %>% 
  pivot_longer(cols = -!!xvar) %>% 
  ggplot(aes(x = .data[[xvar]], y = value)) +
  # add a horizontal line at 0
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  # add a vertical line at x0
  geom_vline(xintercept = x0, color = "black", linetype = "dashed") +
  # plot the line
  geom_line(color = MATPURPLE) +
  geom_point(color = MATPURPLE) +
  theme_minimal() +
  theme(legend.position = 'none') +
  ylab(expression(atop('Improvement in E(' * mu[i^'*'] * ')', 'from Post-Hoc Theorizing (%)'))) +
  plotedits

p2 = plotme %>% 
  select(!!xvar, slearn, dlearn) %>% 
  pivot_longer(cols = -!!xvar) %>% 
  mutate(name = factor(name, levels = c("slearn", "dlearn"))) %>% 
  ggplot(aes(x = .data[[xvar]], y = value, color = name, shape = name)) +
  # add a vertical line at x0
  geom_vline(xintercept = x0, color = "black", linetype = "dashed") +
  # plot the lines
  geom_line(aes(linetype = name)) +
  geom_point() +
  scale_color_manual(
    values = setNames(learn_aes$color, learn_aes$method),
    labels = setNames(learn_aes$label, learn_aes$method)
  ) +
  scale_shape_manual(
    values = setNames(learn_aes$shape, learn_aes$method),
    labels = setNames(learn_aes$label, learn_aes$method)
  ) +
  scale_linetype_manual(
    values = setNames(learn_aes$linetype, learn_aes$method),
    labels = setNames(learn_aes$label, learn_aes$method)
  ) +
  theme_minimal() +
  theme(legend.position = c(0.2, 0.9), 
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  ) +
  ylab(expression(atop('Components of', 'Proposition 2'))) +
  plotedits


# save a combined plot for checking
pboth = gridExtra::arrangeGrob(p1, p2, ncol = 1)
ggsave(here('Results', 'many-panelcheck.pdf'), pboth, 
  width = 8, height = 8, scale = 1.0, device = cairo_pdf)

# save the plots
ggsave(here('Results', 'many-dEmu.pdf'), p1, 
  width = 8, height = 4, scale = 1.0, device = cairo_pdf)

ggsave(here('Results', 'many-prop2.pdf'), p2, 
  width = 8, height = 4, scale = 1.0, device = cairo_pdf)



#%% End ==============================
print('many_het_model.r complete')
