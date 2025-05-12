# %% Setup ==============================================

# Andrew 2025 05
# Simulates just the (very) simple model
# Seems like the clearest pictures come from having the very simple model
# be distinct from the heterogeneous theories model

rm(list = ls())
library(here)
source(here("Code", "utils.r"))

# simulation parameters
n_theorist = 100 # number of selected ideas for each method (ap and ph)
Pr_good = 0.5
h = -Inf # not used in this illustration
seed = 427

# nice for scatterplot
n_ideas = 20; mu_sig = 2; ep_sig = 1; qbad = 0.5
qgood = qbad


#%% Simulate  ====================

# simulate fundamentals and get litplus and litplussum
set.seed(seed)
sim <- simulate_fund(n_ideas, n_theorist, Pr_good, mu_sig, ep_sig, qbad, qgood, h)

#%% Scatterplot  ====================

# Define method aesthetics
method_aes <- tibble(
  method = c("ap", "ph"),
  label = c("a priori", "post hoc"),
  shape = c(16, 8),
  color = c(MATBLUE, MATORANGE)
)

decor = list(
    color = 'black',
    size = 1,
    textx = -0.9
)

pscatter <- sim$litplus %>%
  ggplot(aes(x = muhat, y = mu)) +
  # 45 degree line
  geom_abline(slope = 1, color = decor$color, size = decor$size) +
  # scatterplot
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
    x = expression("Measured Quality " * hat(mu)[paste("i*")]),
    y = expression("Actual Quality " * mu[paste("i*")]),
    shape = "Method",
    color = "Method"
  ) +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),    
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 06))
  ) +
  scale_x_continuous(breaks = seq(-20, 20, 2)) +
  scale_y_continuous(breaks = seq(-20, 20, 2)) +
  annotate("text", x = decor$textx, y = -2, label = "45 degree line", size = 5, color = decor$color) 

# save
ggsave(here("Results", "simple-scatter.pdf"), pscatter, width = 8, height = 4, device = cairo_pdf, scale = 1.2)

print('simple_model.r complete')