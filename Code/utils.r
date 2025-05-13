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
  quicksum = function(dat) {
    dat %>% summarize(
      Emu = mean(mu), Emuhat = mean(muhat), n = n(),
      Pr_good = mean(type == "good"), Pr_bad = mean(type == "bad"),
      .groups = "drop"
    )
  }

  # find all the summaries we might need
  litplussum = bind_rows(
    # any type, no hurdle
    litplus %>% group_by(method) %>% quicksum() %>% mutate(hurdle = 'none', type = 'any'),
    # any type, hurdle
    litplus %>% filter(muhat > h) %>% group_by(method) %>% quicksum() %>% mutate(hurdle = 'h', type = 'any'),
    # by theorist type, no hurdle
    litplus %>% group_by(method, type) %>% quicksum() %>% mutate(hurdle = 'none'),
    # by theorist type, hurdle
    litplus %>% filter(muhat > h) %>% group_by(method, type) %>% quicksum() %>% mutate(hurdle = 'h')
  ) %>% 
  select(hurdle, method, type, everything())
  
  # patch in case there are no results at all
  litplussum = expand_grid(hurdle = c("none", "h"), method = c("ap", "ph"), type = c("any", "good", "bad")) %>% 
    mutate(type = factor(type, levels = c("any", "good", "bad"))) %>% 
    left_join(litplussum, by = c("hurdle", "method", "type")) %>% 
    replace_na(list(n=0)) %>% 
    arrange(hurdle, method, type)  

  # calculate terms in prop 2
  Emu = litplussum %>% filter(hurdle == "h") %>% 
    select(method, type, Emu) %>% 
    pivot_wider(names_from = c(type, method), values_from = Emu)
  
  Pr = litplussum %>% filter(hurdle == "h", type == "any") %>% 
    select(method, good = Pr_good, bad = Pr_bad) %>% 
    pivot_wider(names_from = method, values_from = c(good, bad)) 

  prop2 = tibble(
    dEmu_ph = Emu$any_ph - Emu$any_ap,
    slearn = Pr$good_ph*(Emu$good_ph - Emu$good_ap) + Pr$bad_ph*(Emu$bad_ph - Emu$bad_ap),
    dlearn = (Pr$good_ap - Pr$good_ph)*(Emu$good_ap - Emu$bad_ap)    
  )

  # Return list with or without fund
  output = list(
    litplus = litplus,
    litplussum = litplussum,
    prop2 = prop2
  )  
  if (return_fund) output$fund = fund  
  return(output)

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

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    format(utils::object.size(x), units = "auto") })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Length/Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}