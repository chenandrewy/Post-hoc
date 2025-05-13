#%% Libraries ==============================================

library(tidyverse)
library(data.table)
library(ggplot2)
library(foreach)

#%% Colors ==============================================

MATBLUE <- rgb(0, 0.4470, 0.7410)
MATORANGE <- rgb(0.8500, 0.3250, 0.0980)
MATYELLOW <- rgb(0.9290, 0.6940, 0.1250)
MATPURPLE <- rgb(0.4940, 0.1840, 0.5560)
MATGREEN <- rgb(0.4660, 0.6740, 0.1880)
MATCYAN <- rgb(0.3010, 0.7450, 0.9330)
MATRED <- rgb(0.6350, 0.0780, 0.1840)

#%% Functions ==============================================

# Simulate fundamentals
simulate_fund <- function(n_ideas, n_theorist, Pr_good, mu_sig, ep_sig, qbad, qgood, h = 2, return_fund = FALSE) {
  # simulate "fundamentals"
  fund <- tibble(theorist = 1:n_theorist) %>%
    mutate(type = factor(ifelse(theorist <= n_theorist * Pr_good, "good", "bad"), 
                        levels = c("good", "bad"))) %>%
    # give each theorist a set of ideas to consider
    expand_grid(ideas = 1:n_ideas) %>%
    mutate(
      mu = rnorm(n_ideas * n_theorist, 0, mu_sig),
      ep = rnorm(n_ideas * n_theorist, 0, ep_sig),
      muhat = mu + ep
    ) %>%
    # theorists get signals depending on mu ranking and type
    arrange(theorist, mu) %>%
    group_by(theorist) %>%
    mutate(
      signal = ifelse(type == "good",
        1 * (row_number() >= ceiling(n_ideas * qgood)),
        1 * (row_number() >= ceiling(n_ideas * qbad))
      )
    ) %>%
    # testing
    # mutate(signal = mu > 0) %>% 
    # if we combine theory and data, we end up with a joint ranking
    arrange(theorist, signal, -muhat) %>%
    group_by(theorist, signal) %>%
    mutate(
      muhat_jrank = row_number()
    ) %>%
    ungroup()

  # simulate litplus (literature + ideas that fail statistical hurdle)
  litplus <- bind_rows(
    fund %>% filter(signal == 1) %>% group_by(theorist) %>%
      sample_n(1) %>% ungroup() %>%
      mutate(method = "ap") # a priori
    , fund %>% filter(signal == 1 & muhat_jrank == 1) %>%
      mutate(method = "ph") # post-hoc
  )

  # convenience function
  quicksum <- function(litdat) {
      litdat %>% summarize(
        Emuhat = mean(muhat), Emu = mean(mu),
        Pr_good = mean(type == "good"), 
        N_good = sum(type == "good"), N_bad = sum(type == "bad")
      )
  }

  # summarize
  litplussum <- litplus %>%
    group_by(method) %>%
    quicksum() %>%
    mutate(hurdle = "none") %>%
    bind_rows(
      litplus %>% filter(muhat >= h) %>% group_by(method) %>%
        quicksum() %>% mutate(hurdle = "h")
    ) %>%
    select(hurdle, everything())

  # patch in case there are no results at all
  litplussum = expand_grid(hurdle = c("none", "h"), method = c("ap", "ph")) %>% 
    left_join(litplussum, by = c("hurdle", "method")) %>% 
    replace_na(list(N_good = 0, N_bad = 0))

  # Return list with or without fund
  if (return_fund) {
    return(list(
      fund = fund,
      litplus = litplus,
      litplussum = litplussum
    ))
  } else {
    return(list(
      litplus = litplus,
      litplussum = litplussum
    ))
  }
} # end simulate_fund

# Histogram plot
plot_histogram <- function(
    histdat, ylimnum = NULL, xlimnum = NULL
    ) {  
  # for standardizing histogram plots

  plot_out <- histdat %>%
    ggplot(aes(x = mid, y = prop)) +
    geom_bar(aes(fill = type), stat = "identity", position = "identity", alpha = 0.5) +
    scale_fill_manual(
      values = c("good" = MATBLUE, "bad" = MATORANGE),
      name = "Theorist Type"
    ) +
    labs(
      y = "Proportion",
      fill = "Theorist Type"
    ) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.2, "cm"),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14),
      legend.position = c(0.1, .9),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = alpha("white", 06))
    ) +
    coord_cartesian(xlim = xlimnum, ylim = ylimnum)

  return(plot_out)
} # end plot_histogram

# Helper function to create histogram data
create_histogram_data <- function(data, varname, filter_condition = NULL, binwidth = 0.5) {
  if (!is.null(filter_condition)) {
    data <- data %>% filter(!!filter_condition)
  }
  
  # get bins  
  binlimit <- data %>% pull(get(varname)) %>% range()
  binlimit[1] = floor(binlimit[1] / binwidth) * binwidth
  binlimit[2] = ceiling(binlimit[2] / binwidth) * binwidth
  edge = seq(binlimit[1], binlimit[2], by = binwidth)
  mid <- (edge[-1] + edge[-length(edge)]) / 2
  
  plotme <- data %>%
    group_by(method, type) %>%
    count(mid = cut(get(varname), edge, labels = mid)) %>%
    mutate(
      prop = n / sum(n),
      mid = as.numeric(as.character(mid))
    )
  
  return(list(
    data = plotme,
    binlimit = binlimit,
    prop_range = plotme %>% pull(prop) %>% range()
  ))
}
