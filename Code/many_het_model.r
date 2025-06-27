# %% Setup ==============================================

# Andrew 2025 05
# Simulates many het models
# takes about 10 minutes

rm(list = ls())

library(here)
source(here("Code", "utils.r")) 

seed <- 1027

# baseline ap optimal parameters
# parameter order must match head (simulate_fund)
# for n_theorist = 100e3, and 30 sims, takes 5 minutes
par0 <- tibble(
  # counts
  n_ideas = 100, n_theorist = 100e3, Pr_good = 0.5,
  # ideas
  mu_sig = 0.5, ep_sig = 1,
  # theorist heterogeneity
  qbad = 0.01, qgood = 0.99,
  # publication hurdle
  h = 2
)

# manysim settings
NSIM_PER_XVAR <- 20
QGOOD_N_FACTOR <- 1

# %% Simulate par0 ==============================
# par0 forms the baseline for the many other settings

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- do.call(simulate_fund, as.list(par0) %>% c(return_fund = FALSE))

# %% Simulate many  ====================

simulate_many <- function(par0, xname, xval) {
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
    temppar <- par0
    temppar[1, xname] <- xval[i]

    # Call simulate_fund with all parameters
    set.seed(seed)
    sim <- do.call(simulate_fund, as.list(temppar))

    litplussum_list[[i]] <- sim$litplussum %>% mutate(setid = i)
    prop2_list[[i]] <- sim$prop2 %>% mutate(setid = i)
  } # end for i

  # = Combine and clean =
  litplussum <- bind_rows(litplussum_list)
  prop2 <- bind_rows(prop2_list)

  # add parameters to prop2 and litplussum
  prop2 <- prop2 %>%
    mutate(!!xname := xval[setid]) %>%
    select(setid, !!xname, everything())
  litplussum <- litplussum %>%
    mutate(!!xname := xval[setid]) %>%
    select(setid, !!xname, everything())

  # add a few items to prop2
  prop2 <- prop2 %>%
    left_join(
      litplussum %>% filter(hurdle == "h", type == "any") %>%
        select(setid, method, n, Emu) %>%
        pivot_wider(names_from = method, values_from = c(n, Emu)),
      by = "setid"
    )

  return(list(litplussum = litplussum, prop2 = prop2))
} # end of simulate_many

# %% Define Prop2 Settings and Simulate ==========================

# = Simulate many mu_sig =

many1set <- list(
  xname = "mu_sig",
  xval = seq(par0$mu_sig, sqrt(60) * par0$mu_sig, length.out = NSIM_PER_XVAR)
)

many1out <- simulate_many(par0, many1set$xname, many1set$xval)

# = Simulate many qgood =

# increase n_theorist for qgood (it's noiser if it's small)
temppar <- par0
temppar$n_theorist = par0$n_theorist * QGOOD_N_FACTOR

many2set <- list(
  xname = "qgood",
  xval = seq(temppar$qbad, temppar$qgood, length.out = NSIM_PER_XVAR)
)

many2out <- simulate_many(temppar, many2set$xname, many2set$xval)

# %% Convenience Save ==============================

save.image(here("Results", "zzz-many-het.RData"))

# %% Convenience Load ==============================

load(here("Results", "zzz-many-het.RData"))

# %% Plot Darwinian Selection ==============================
# outputs many-par0-ap.pdf and many-par0-ph.pdf
# histgrams that illustrate darwinian selection

# Helper function to create histogram data
create_histogram_data <- function(data, varname, binwidth = 0.5) {
  # get bins
  binlimit <- data %>%
    pull(get(varname)) %>%
    range()
  binlimit[1] <- floor(binlimit[1] / binwidth) * binwidth
  binlimit[2] <- ceiling(binlimit[2] / binwidth) * binwidth
  edge <- seq(binlimit[1], binlimit[2], by = binwidth)
  mid <- (edge[-1] + edge[-length(edge)]) / 2

  # count
  histc <- data %>%
    group_by(method, type) %>%
    count(
      mid = cut(get(varname), edge, labels = mid),
      name = "n"
    ) %>%
    ungroup() %>%
    mutate(
      mid = as.numeric(as.character(mid)),
      prop = n / sum(n),
      pct = 100 * prop
    )
  return(list(
    data = histc,
    binlimit = binlimit,
    prop_range = histc %>% pull(prop) %>% range(),
    pct_range = histc %>% pull(pct) %>% range()
  ))
} # end create_histogram_data

# = histogram plot edits =
ANNOTATE_TEXT_SIZE <- 4
ANNOTATE_COLOR <- MATRED
ANNOTATE_COLOR2 <- 'gray30'
ANNOTATE_LINE_SIZE <- 1
ANNOTATE_VJUST <- 2.0

# = 1) muhat histograms =

# Create plots for both methods
p_all <- list()
for (method_name in c("ap", "ph")) {
  # Create histogram data
  histall <- create_histogram_data(
    sim$litplus %>% filter(method == method_name),
    "muhat",
    binwidth = 0.25
  )

  p_all[[method_name]] <- histall$data %>%
    plot_histogram(xlimnum = histall$binlimit, ylimnum = histall$pct_range) +
    xlab(expression("Measured Quality " * hat(mu)[paste("i*")] * " (All)")) +
    # publication hurdle
    geom_vline(xintercept = par0$h, color = ANNOTATE_COLOR, size = ANNOTATE_LINE_SIZE) +
    annotate("text",
      x = par0$h + 0.2, y = Inf, 
      label = expression("published ->"),
      vjust = ANNOTATE_VJUST, hjust = 0, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    ) +
    annotate("text",
      x = par0$h - 0.2, y = Inf, 
      label = expression("<- unpublished"),
      vjust = ANNOTATE_VJUST, hjust = 1, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR
    ) +
    theme(
      legend.position = c(05, 80) / 100
    ) +
    scale_x_continuous(breaks = seq(-20, 20, 2)) +
    scale_y_continuous(breaks = seq(0, 100, 2)) +
    coord_cartesian(xlim = c(-3.5, +4.5), ylim = c(0, 12))
} # end for method_name

# = 2) mu histograms =

# Create plots for both methods
p_pub <- list()
for (method_name in c("ap", "ph")) {
  # Create histogram data
  histpub <- create_histogram_data(
    sim$litplus %>% filter(method == method_name, muhat > par0$h),
    "mu",
    binwidth = 0.1
  )
  
  # find mean actual quality
  tempEmu <- sim$litplussum %>%
    filter(hurdle == "h", method == method_name, type == "any") %>%
    pull(Emu)

  p_pub[[method_name]] <- histpub$data %>%
    filter(method == method_name) %>%
    plot_histogram(xlimnum = histpub$binlimit, ylimnum = histpub$pct_range) +
    theme(legend.position = c(7, 9) / 10) +
    xlab(expression("Actual Quality " * mu[paste("i*")] * " (Published)")) +
    # mean actual quality
    geom_vline(
      xintercept = tempEmu,
      color = ANNOTATE_COLOR2, size = 0.8, linetype = "longdash"
    ) +
    annotate("text",
      x = tempEmu, y = Inf, 
      label = expression("mean published " * mu[paste("i*")]),
      vjust = ANNOTATE_VJUST, hjust = 1.1, size = ANNOTATE_TEXT_SIZE, color = ANNOTATE_COLOR2
    ) +
    scale_y_continuous(breaks = seq(0, 100, 5)) +
    scale_x_continuous(breaks = seq(-20, 20, 1)) +
    coord_cartesian(xlim = c(-1, +2.2), ylim = c(0, 18))
} # end for method_name

# = save plots =
tempwidth = 8; tempheight = 3; tempscale = 1

# output a priori arranged horizontally
p_par0_ap <- arrangeGrob(
  p_all$ap,
  p_pub$ap + theme(legend.position = "none"),
  ncol = 2
)
ggsave(here("Results", "many-par0-ap.pdf"),
  p_par0_ap,
  width = tempwidth, height = tempheight, scale = tempscale, device = cairo_pdf
)

# output post hoc arranged horizontally
p_par0_ph <- arrangeGrob(
  p_all$ph,
  p_pub$ph + theme(legend.position = "none"),
  ncol = 2
)
ggsave(here("Results", "many-par0-ph.pdf"),
  p_par0_ph,
  width = tempwidth, height = tempheight, scale = tempscale, device = cairo_pdf
)

# %% Plot Proposition 2 ==============================

## Define plotting function ====
plot_basic <- function(plotme, xname, plotedits = list()) {
  # plots the basic stuff
  # plotme is a df with cols [xname], dEmu_ph, slearn, ...
  # xname is the name of the x-axis variable

  # where dlearn = slearn
  x0 <- plotme %>%
    arrange(abs(dlearn - slearn)) %>%
    slice(1) %>%
    pull(!!xname)

  learn_aes <- tibble(
    method = c("slearn", "dlearn"),
    label = c("Statistical Learning", "Darwinian Learning"),
    shape = c(16, 8),
    color = c(MATBLUE, MATORANGE),
    linetype = c("solid", "dashed")
  )

  theme_standard = theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      legend.position = c(0.1, .9),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 1), color = NA),
      legend.box.margin = margin(0, 0, 0, 0)
  )

  minortextsize <- 12

  p_dEmu <- plotme %>%
    mutate(pct_delta_ph = 100 * (Emu_ph - Emu_ap) / Emu_ap) %>%
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
    theme_standard +
    theme(
      legend.position = "none",
    ) +
    ylab(expression(atop("Improvement from", "Post-Hoc Theorizing (%)"))) +
    plotedits

  p_prop2 <- plotme %>%
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
    theme_standard +
    theme(
      legend.position = c(05, 99) / 100,
      legend.title = element_blank()
    ) +
    ylab(expression(atop("Components of", "Proposition 2"))) +
    plotedits

  return(list(
    dEmu = p_dEmu, prop2 = p_prop2
  ))
} # end plot_basics

## Do actual plots ====

plotedit1 <- list(
  xlab(expression(
    "Standard Deviation of Actual Quality"
  ))
)
many1plot <- plot_basic(many1out$prop2, many1set$xname, plotedit1)

plotedit2 <- list(
  xlab(expression("Heterogeneity of Theories"))
)
many2plot <- plot_basic(many2out$prop2, many2set$xname, plotedit2)

# alternative version of many1plot
many1altdat = many1out$prop2 %>%
  mutate(sd_muhat = sqrt(mu_sig^2 + par0$ep_sig^2)) 

plotedit1alt <- list(
  xlab(expression("Standard Deviation of Measured Quality"))
  , coord_cartesian(xlim = c(0.95, 4.05))
)
many1altplot <- plot_basic(many1altdat, "sd_muhat", plotedit1alt)

## Save plots to disk ====

tempwidth = 6; tempheight = 6; tempscale = 0.9

ggsave(here("Results", "many-qgood.pdf"),
  arrangeGrob(many2plot$dEmu, many2plot$prop2, ncol = 1),
  width = tempwidth, height = tempheight, scale = tempscale, device = cairo_pdf
)

ggsave(here("Results", "many-mu_sig.pdf"),
  arrangeGrob(many1plot$dEmu, many1plot$prop2, ncol = 1),
  width = tempwidth, height = tempheight, scale = tempscale, device = cairo_pdf
)

ggsave(here("Results", "many-sd_muhat.pdf"),
  arrangeGrob(many1altplot$dEmu, many1altplot$prop2, ncol = 1),
  width = tempwidth, height = tempheight, scale = tempscale, device = cairo_pdf
)


# %% End ==============================
print("many_het_model.r complete")
