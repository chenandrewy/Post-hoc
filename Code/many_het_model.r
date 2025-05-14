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
  # counts
  n_ideas = 100, n_theorist = 100e3, Pr_good = 0.5,
  # ideas
  mu_sig = 0.5, ep_sig = 1,
  # theorist heterogeneity
  qbad = 0.01, qgood = 0.99,
  # publication hurdle
  h = 2
)

#%% Simulate par0 ==============================
# par0 forms the baseline for the many other settings

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- do.call(simulate_fund, as.list(par0) %>% c(return_fund = TRUE))

#%% Plot par0 ==============================

# = histogram plot edits =
ANNOTATE_TEXT_SIZE = 5
ANNOTATE_COLOR = MATRED
ANNOTATE_LINE_SIZE = 1
ANNOTATE_VJUST = 3.0
h = par0$h
plot_edits_muhat = list(
    xlab(expression("Measured Quality " * hat(mu)[paste("i*")] * " (All)")),
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
      axis.text = element_text(size = 12),
    ),
    scale_x_continuous(breaks = seq(-20, 20, 2))
)

plot_edits_mu = function(Emu) {
  list(
    xlab(expression("Actual Quality " * mu[paste("i*")] * " (Published)")),
    geom_vline(xintercept = Emu, color = ANNOTATE_COLOR, size = ANNOTATE_LINE_SIZE), 
    annotate("text",
      x = Emu, y = Inf, label = "mean",
      vjust = ANNOTATE_VJUST, hjust = 1.1, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    ),
    theme(
      axis.text = element_text(size = 12),
    )
  )
} # end of plot_edits_mu

# = 1) muhat histogram =

# Create histogram data
hist_data <- create_histogram_data(sim$litplus, "muhat", binwidth = 0.25)

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
hist_data <- create_histogram_data(sim$litplus, "mu", filter_condition = quote(muhat > h), binwidth = 0.2)

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


# output a priori arranged horizontally
p_par0_ap = arrangeGrob(
  plots$ap, 
  lit_plots$ap + theme(legend.position = "none"),
  ncol = 2
)
ggsave(here('Results', 'many-par0-ap.pdf'),
  p_par0_ap,
  width = 8, height = 4, scale = 1, device = cairo_pdf
)

# output post hoc arranged horizontally
p_par0_ph = arrangeGrob(
  plots$ph,
  lit_plots$ph + theme(legend.position = "none"),
  ncol = 2
)
ggsave(here('Results', 'many-par0-ph.pdf'),
  p_par0_ph,
  width = 8, height = 4, scale = 1, device = cairo_pdf
)





#%% Simulate many  ====================

simulate_many = function(par0, xname, xval){
  # starts with parameters in par0
  # changes xname to each value in xval
  # outputs litplussum and prop2

  # simulate for each set
  litplussum_list <- list()
  prop2_list <- list()

  # Start timing
  start_time <- Sys.time()

  for (i in seq_along(xval)) {
      # Calculate elapsed time and estimate remaining time
      elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      avg_time_per_iter <- elapsed_time / (i - 1) 
      remaining_time <- avg_time_per_iter * (length(xval) - i)
      
      print(paste0(
        "iteration: ", i, " of ", length(xval), ", ", xname, 
        " = ", round(xval[i], 2),
        " (Est. remaining: ", round(remaining_time, 1), " mins)"
      ))
      
      # Add parameters from manyset
      temppar = par0
      temppar[1, xname] = xval[i]

      # Call simulate_fund with all parameters
      sim <- do.call(simulate_fund, as.list(temppar))

      litplussum_list[[i]] <- sim$litplussum %>% mutate(setid = i)
      prop2_list[[i]] <- sim$prop2 %>% mutate(setid = i)
  } # end for i

  # = Combine and clean =
  litplussum = bind_rows(litplussum_list)
  prop2 = bind_rows(prop2_list)

  # add parameters to prop2 and litplussum
  prop2 = prop2 %>% 
    mutate(!!xname := xval[setid]) %>%
    select(setid, !!xname, everything())
  litplussum = litplussum %>% 
    mutate(!!xname := xval[setid]) %>%
    select(setid, !!xname, everything()) 

  # add a few items to prop2
  prop2 = prop2 %>% 
    left_join(
      litplussum %>% filter(hurdle == 'h', type == 'any') %>% 
        select(setid, method, n, Emu) %>% 
        pivot_wider(names_from = method, values_from = c(n, Emu)),
      by = "setid"
    )

  return(list(litplussum = litplussum, prop2 = prop2))
} # end of simulate_many

#%% Simulate many mu_sig ==============================

many1set = list(
  xname = "mu_sig",
  xval = seq(par0$mu_sig, sqrt(50)*par0$mu_sig, length.out = 30)
)

many1out = simulate_many(par0, many1set$xname, many1set$xval)


#%% Simulate many qgood ==============================

many2set = list(
  xname = "qgood",
  xval = seq(par0$qbad, par0$qgood, length.out = 30)
)

many2out = simulate_many(par0, many2set$xname, many2set$xval)

#%% Plot 

plot_basic = function(plotme, xname, plotedits = list()){
  # plots the basic stuff

  # where dlearn = slearn
  x0 = plotme %>% arrange(abs(dlearn - slearn)) %>% slice(1) %>% pull(!!xname)

  learn_aes = tibble(
    method = c("slearn", "dlearn"),
    label = c("Statistical Learning", "Darwinian Learning"),
    shape = c(16, 8),
    color = c(MATBLUE, MATORANGE),
    linetype = c("solid", "dashed")
  )

  minortextsize = 12

  p_dEmu = plotme %>% 
    mutate(pct_delta_ph = 100*(Emu_ph - Emu_ap)/Emu_ap) %>% 
    select(!!xname, pct_delta_ph) %>% 
    pivot_longer(cols = -!!xname) %>% 
    ggplot(aes(x = .data[[xname]], y = value)) +
    # add a horizontal line at 0
    geom_hline(yintercept = 0, color = "black", linetype = "solid") +
    # add a vertical line at x0
    geom_vline(xintercept = x0, color = "black", linetype = "dashed") +
    # plot the line
    geom_line(color = MATPURPLE) +
    geom_point(color = MATPURPLE) +
    theme(legend.position = 'none',
      axis.text = element_text(size = minortextsize)
    ) +
    ylab(expression(atop('Improvement in E(' * mu[i^'*'] * ')', 'from Post-Hoc Theorizing (%)'))) +
    plotedits

  p_prop2 = plotme %>% 
    select(!!xname, slearn, dlearn) %>% 
    pivot_longer(cols = -!!xname) %>% 
    mutate(name = factor(name, levels = c("slearn", "dlearn"))) %>% 
    ggplot(aes(x = .data[[xname]], y = value, color = name, shape = name)) +
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
    theme(legend.position = c(25, 85)/100, 
      legend.title = element_blank(),
      legend.text = element_text(size = minortextsize),
      axis.text = element_text(size = minortextsize)
    ) +
    ylab(expression(atop('Components of', 'Proposition 2'))) +
    plotedits

  return(list(
    dEmu = p_dEmu, prop2 = p_prop2
  ))

} # end plot_basics

plotedit1 = list(
  xlab(expression(
    'Standard Deviation of Actual Idea Quality SD(' * mu[i] * ')'
  ))
)
many1plot = plot_basic(many1out$prop2, many1set$xname, plotedit1)

plotedit2 = list(
  xlab(expression('Heterogeneity of Theorists'))
)
many2plot = plot_basic(many2out$prop2, many2set$xname, plotedit2)


# save plots to disk
ggsave(here('Results', 'many-qgood.pdf'), 
  arrangeGrob(many2plot$dEmu, many2plot$prop2, ncol = 1),
  width = 6, height = 6, scale = 1.0, device = cairo_pdf)

ggsave(here('Results', 'many-mu_sig.pdf'), 
  arrangeGrob(many1plot$dEmu, many1plot$prop2, ncol = 1),
  width = 6, height = 6, scale = 1.0, device = cairo_pdf)


#%%

# try plotting all 4 panels
ggsave(here('Results', 'manyall-panelcheck.pdf'), 
  arrangeGrob(
    many1plot$dEmu, many2plot$dEmu, 
    many1plot$prop2, many2plot$prop2,
    ncol = 2
  ),
  width = 8, height = 8, scale = 1.0, device = cairo_pdf)


#%% End ==============================
print('many_het_model.r complete')
