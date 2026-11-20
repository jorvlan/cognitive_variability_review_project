# R code that produces the figures in "Cognitive variability on stable ground: A narrative review on cognitive fluctuation in childhood and adolescence"
# Jordy van Langen, October 2026

# load packages
library(ggplot2)
library(ggtext)
library(patchwork)
library(dplyr)
library(scales)
library(ggrepel)

# remove later
review_output_dir = "/Users/maaikeschumer/Desktop/Jordy_PHD/review"

# Figure 1 - top panel - equal means, differing variability

# Create N participants and T timepoints
n_pp <- 2
n_datapoints <- 100

# Generate data with low variability (using pmax & 0 prevents negative values)
data_low_var <- pmax(rnorm(n_datapoints, 6.5, 0.6), 0)

# Generate data with high variability 
data_high_var <- pmax(rnorm(n_datapoints, 6.5, 3), 0)

# Create a dataframe that stores these
sim_var_data <- data.frame(data_low_var, data_high_var) 

# Make the dataframe in long-format
sim_var_data_long <- data.frame(a = unlist(sim_var_data, use.names = FALSE))

# Map running trial numbers to dataframe
sim_var_data_long$trial[1:100] <- c(1:100)
sim_var_data_long$trial[101:200] <- c(1:100)

# Map person IDs to dataframe
sim_var_data_long$person[1:100] <- "Person 1"
sim_var_data_long$person[101:200] <- "Person 2"

# Create plot
equal_means_plot <- ggplot(sim_var_data_long, aes(trial, a)) +
  geom_line(aes(group = as.factor(person), colour = as.factor(person))) +
  geom_hline(yintercept = 6.5, color = "darkgrey", linetype = "dashed", linewidth = 0.75) +
  scale_color_manual(values = c("Person 1" = "dodgerblue", "Person 2" = "firebrick")) +
  labs(
    y = "Reaction time",
    x = "Trial",
    title = "<span style = 'color: dodgerblue;'>Low Variability</span> versus <span style = 'color: firebrick;'>High Variability</span>, equal <span style = 'color: darkgrey;'>Means</span>",
    color = "Person") +
  ylim(0,15) +
  xlim(0,100) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_markdown()
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  ) +
  theme(legend.position = "none") +
  facet_wrap(~person)


# Figure 1 - bottom panel - equal variability, differing means

# Set equal SD for both persons
common_sd <- 1.5

# Set different mean values
mean_1 <- 4.0
mean_2 <- 10.0

# Generate data with low mean
data_mean_low <- pmax(rnorm(n_datapoints, mean_1, common_sd), 0)

# Generate data with high mean
data_mean_high <- pmax(rnorm(n_datapoints, mean_2, common_sd), 0)

# Create dataframe
sim_mean_data <- data.frame(data_mean_low, data_mean_high)

# Make the dataframe in long-format
sim_mean_data_long <- data.frame(a = unlist(sim_mean_data, use.names = FALSE))

# Map running trial numbers to dataframe
sim_mean_data_long$trial[1:100] <- c(1:100)
sim_mean_data_long$trial[101:200] <- c(1:100)

# Map person IDs to dataframe
sim_mean_data_long$person[1:100] <- "Person 3"
sim_mean_data_long$person[101:200] <- "Person 4"

# Create dataframe that contains the horizontal lines for the mean
hline_means <- data.frame(
  person = c("Person 3", "Person 4"),
  h = c(4, 10)
)

# Create plot
equal_var_plot <- ggplot(sim_mean_data_long, aes(trial, a)) +
  geom_line(aes(group = as.factor(person), colour = as.factor(person))) +
  geom_hline(data = hline_means, aes(yintercept = h), color = c("dodgerblue", "firebrick"),
             linetype = "dashed", linewidth = 0.75) +
  scale_color_manual(values = c("Person 3" = "darkgrey", "Person 4" = "darkgrey")) +
  labs(
    y = "Reaction time",
    x = "Trial",
    title = "<span style = 'color: dodgerblue;'>Low Mean</span> versus <span style = 'color: firebrick;'>High Mean</span>, equal <span style = 'color: darkgrey;'>Variability</span>",
    color = "Person"
  ) +
  ylim(0,15) +
  xlim(0,100) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_markdown()
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12)
  ) +
  theme(legend.position = "none") +
  facet_wrap(~person)

# Combining both plots
(equal_means_plot / equal_var_plot) +
  plot_layout(guides = "collect")

# Save plot
ggsave(
  filename = file.path(review_output_dir, paste0("fig1", ".png")),
  width = 8,
  height = 4,
  dpi = 300
)

# Figure 2 - top panel - within-task stable and variable phases due to attentional lapses

# Set T timepoints, chunk size and N chunks
n_timepoints <- 100
chunk_size <- 10
n_chunks <- n_timepoints / chunk_size

# Create 3 variables
time <- 1:n_timepoints
score <- numeric(n_timepoints)
type <- character(n_timepoints)

# Run loop that simulates data
for (i in 1:n_chunks) {
  id <- ((i -1) * chunk_size + 1):(i * chunk_size)
  if (i %% 2 == 0) {
    # Low variability
    score[id] <- pmax(rnorm(chunk_size, mean = 55, sd = 1), 0)
    type[id] <- "Stable"
  } else {
    # High variability
    score[id] <- pmax(rnorm(chunk_size, mean = 50, sd = 10), 0)
    type[id] <- "Variable"
  }
}

# Create dataframe
df_stable_var <- data.frame(
  time = time,
  score = score,
  type = factor(type, levels = c("Stable", "Variable"))
)

# Create segments with start and end of each line segment
df_stable_var_segmented <- df_stable_var %>%
  dplyr::mutate(time_next = lead(time),
                score_next = lead(score),
                type_segment = type) %>%
  dplyr::filter(!is.na(time_next))

# Create plot
short_term_transitions_plot <- ggplot(df_stable_var_segmented) +
  geom_segment(aes(x = time, y = score,
                   xend = time_next, yend = score_next,
                   color = type_segment)) +
  geom_hline(
    yintercept = mean(df_stable_var$score),
    linetype = "dashed",
    color = "grey",
    linewidth = 0.8) +
  scale_color_manual(values = c("Stable" = "dodgerblue", "Variable" = "firebrick"),
                     guide = "none") +
  labs(
    title = "Attentional control: <span style='color: firebrick;'>Variable</span> and <span style='color: dodgerblue;'>Stable</span> phases",
    x = "Trial",
    y = "Reaction time",
    color = "type"
  ) + 
  theme_minimal() +
  theme(
    plot.title = ggtext::element_markdown(),
    legend.position = "right"
  )

# Figure 2 - middle panel - within-task stable and variable phases + trend

# Shape parameters
start_mean <- 80
plateau <- 50
decay_rate <- 0.025

# Create a smooth curve
mean_curve <- plateau + (start_mean - plateau) * exp(-decay_rate * (time - 1))

# Create function that indicates the decline in variability over time
var_decrease <- function(chunk_id, base_sd = 10, min_sd = 1) {
  sd <- base_sd - (base_sd - min_sd) * (chunk_id - 1) / (n_chunks - 1)
  return(sd)
}

#loop that simulates data
for (i in 1:n_chunks) {
  id <- ((i - 1) * chunk_size + 1):(i * chunk_size)
  
  if (i %% 2 == 0) {
    # Low variability 
    score[id] <- pmax(mean_curve[id] + rnorm(chunk_size, mean = 0, sd = 1), 0)
    type[id] <- "Stable"
  } else {
    # High variability 
    sd_current <- var_decrease(i)
    score[id] <- pmax(mean_curve[id] + rnorm(chunk_size, mean = 0, sd = sd_current), 0)
    type[id] <- "Variable"
  }
}

# Create dataframe
df_stable_var <- data.frame(
  time = time,
  score = score,
  type = factor(type, levels = c("Stable", "Variable"))
)

df_stable_var$type[df_stable_var$time > 0.6 * n_timepoints] <- "Stable"

# Create segments start and end of each line segment
df_stable_var_segmented <- df_stable_var %>%
  dplyr::mutate(time_next = lead(time),
                score_next = lead(score),
                type_segment = type) %>%
  dplyr::filter(!is.na(time_next))

# Create plot
short_term_transitions_plot_b <- ggplot(df_stable_var_segmented) +
  geom_segment(aes(x = time, y = score,
                   xend = time_next, yend = score_next,
                   color = type_segment)) +
  scale_color_manual(values = c("Stable" = "dodgerblue", "Variable" = "firebrick"),
                     guide = "none") +
  labs(
    title = "Within-task learning: <span style='color: firebrick;'>Variable</span> and <span style='color: dodgerblue;'>Stable</span> phases alternate before <span style='color: dodgerblue;'>stabilizing</span> ",
    x = "Trial",
    y = "Reaction time",
    color = "type"
  ) +
  theme_minimal() +
  theme(
    plot.title = ggtext::element_markdown(),
    legend.position = "right") +
  geom_smooth(
    data = df_stable_var,
    aes(x = time, y = score),
    method = "loess",
    se = FALSE,
    color = "grey",
    linewidth = 1)

# Figure 2 - bottom panel - developmental change in cognitive variability 

# Shape parameters
start_mean <- 40
plateau <- 10
decay_rate <- 0.025

time <- 1:n_timepoints
step <- paste0("Step_", ceiling(time / chunk_size))
chunk_id <- ceiling(time / chunk_size)

# Create smooth curve
mean_curve <- plateau + (start_mean - plateau) * exp(-decay_rate * (time - 1))

# Decreasing variability over time
sd_curve <- seq(from = 5, to = 1, length.out = n_timepoints)

# Additional variability at chunk-level
noise_sd_mean <- seq(from = 4, to = 0.5, length.out = n_chunks)
chunk_offset <- rnorm(n_chunks, mean = 0, sd = noise_sd_mean)

# Simulate data
score <- pmax(mean_curve + chunk_offset[chunk_id] +
                rnorm(n_timepoints, mean = 0, sd = sd_curve), 0)

# Create dataframe
df_development_var_24 <- data.frame(time = time, score = score, step = step)

# Create dataframe for the curve
df_curve <- data.frame(
  time = time,
  mean_score = mean_curve,
  ymin = pmax(mean_curve - sd_curve, 0),
  ymax = mean_curve + sd_curve,
  step_segment = step
)

# Duplicate the first point of each chunk into the previous chunk so that Ribbons join with no gaps
df_boundary <- df_curve %>%
  filter(time %% chunk_size == 1, time > 1) %>%
  mutate(step_segment = paste0("Step_", ceiling(time / chunk_size) - 1))

# Bind rows
df_ribbon_mean_sd <- bind_rows(df_curve, df_boundary) %>%
  arrange(step_segment, time)

# Create segments start and end of each line segment
df_development_var_24_segmented <- df_development_var_24 %>%
  mutate(
    time_next = lead(time),
    score_next = lead(score),
    step_segment = step) %>%
  filter(!is.na(time_next))

# Create color palette
step_cols <- setNames(
  c("#B22222", "#D9534F", "#F18773", "#F5B2A0", "#F7DCD4",
    "#BFE5E5", "#8ED3EB", "#64BFF2", "#3FA9F5", "#1E90FF"),
  paste0("Step_", 1:10)
)

# Create plot
long_term_transitions_plot <- ggplot(df_development_var_24_segmented) +
  geom_segment(
    aes(x = time, y = score, xend = time_next, yend = score_next, color = step_segment)
  ) +
  scale_color_manual(values = step_cols, guide = "none") +
  #smooth L-shaped mean trajectory
  geom_smooth(
    data = df_curve,
    aes(x = time, y = mean_score),
    inherit.aes = FALSE,
    linewidth = 1, color = "grey"
  ) +
  scale_x_continuous(
    breaks = c(0, 25, 50, 75, 100),
    labels = c(0, 6, 12, 18, 24)
  ) +
  labs(
    title = "Developmental trajectory of cognitive variability",
    x = "Age",
    y = "Cognitive variability"
  ) +
  theme_minimal() +
  theme(plot.title = ggtext::element_markdown())

#final Figure 2: combine 3 plots into 1 plot
short_term_transitions_plot / short_term_transitions_plot_b + long_term_transitions_plot

#save plot
ggsave(
  filename = file.path(review_output_dir, paste0("fig2", ".png")),
  width = 8,
  height = 6,
  dpi = 300
)

# Figure 3 - overlapping waves

# Create the x-axis
x <- seq(0,100, length.out = 500)

# Function for asymmetric peak
asym_peak <- function(x, mu, h, sd_left, sd_right, floor_right = 0) {
  ifelse(
    x < mu,
    h * exp(-(x-mu)^2 / (2 * sd_left^2)),
    floor_right + (h - floor_right) * exp(-(x - mu)^2 / (2 * sd_right^2))
  )
}

# Create S-shape uprise for the last strategy
logistic <- function(x, mid, h, steepness) {
  h / (1 + exp(-(x - mid) / steepness))
}

# Create dataframe that contains the density curve for each strategy
df_waves <- data.frame(
  x = x,
  `Strategy 1` = asym_peak(x, mu = 8, h = 0.90, sd_left = 12, sd_right = 25),
  `Strategy 2` = asym_peak(x, mu = 24, h = 0.85, sd_left = 12, sd_right = 30),
  `Strategy 3` = asym_peak(x, mu = 32, h = 0.30, sd_left = 8, sd_right = 7),
  `Strategy 4` = asym_peak(x, mu = 42, h = 0.55, sd_left = 10, sd_right = 12),
  `Strategy 5` = asym_peak(x, mu = 52, h = 1.00, sd_left = 28, sd_right = 12, floor_right = 0.28),
  `Strategy 6` = asym_peak(x, mu = 64, h = 0.45, sd_left = 9, sd_right = 10),
  `Strategy 7` = asym_peak(x, mu = 76, h = 0.65, sd_left = 10, sd_right = 14, floor_right = 0.15),
  `Strategy 8` = asym_peak(x, mu = 80, h = 0.55, sd_left = 11, sd_right = 15, floor_right = 0.15),
  `Strategy 9` = asym_peak(x, mu = 84, h = 0.75, sd_left = 12, sd_right = 16, floor_right = 0.15),
  `Strategy 10` = logistic(x, mid = 88, h = 0.95, steepness = 7),
  check.names = FALSE) %>%
  pivot_longer(-x, names_to = "strategy", values_to = "use")


# Position the labels around the curve's peak
df_labels <- df_waves %>%
  filter(strategy != "Strategy 10") %>%
  group_by(strategy) %>%
  slice_max(use, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  bind_rows(
    df_waves %>%
      filter(strategy == "Strategy 10") %>%
      slice_min(abs(x - 97), n = 1, with_ties = FALSE))

# Assign colors to each strategy
strategy_cols <- c(
  "Strategy 1"  = "#B22222",
  "Strategy 2"  = "#D9534F",
  "Strategy 3"  = "#F18773",
  "Strategy 4"  = "#E8A090",
  "Strategy 5"  = "#8ED3EB",
  "Strategy 6"  = "#64BFF2",
  "Strategy 7"  = "#3FA9F5",
  "Strategy 8"  = "#1E90FF",
  "Strategy 9"  = "#1A6FD6",
  "Strategy 10" = "#0B4F9C"
)

# Create plot

overlapping_waves_plot <- ggplot(df_waves, aes(x = x, y = use, color = strategy)) +
  geom_line(linewidth = 1) +
  geom_label_repel(
    data = df_labels,
    aes(label = strategy),
    color = "darkgrey",
    fill = "white",
    size = 4,
    label.size = 0.2,
    nudge_x = ifelse(df_labels$strategy == "Strategy 10", 9, 0),
    nudge_y = 0.09,
    min.segment.length = 0,
    segment.color = "lightgrey",
    box.padding = 0.4,
    seed = 2094,
    show.legend = FALSE) +
  scale_color_manual(values = strategy_cols, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0,0.035))) +
  scale_y_continuous(limits = c(0, 1.3), expand = expansion(mult = c(0,0))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "",
    x = "Time (age & learning)",
    y = "Density") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(
      linewidth = 0.95,
      arrow = arrow(length = unit(0.25, "cm"), type = "closed")
    ),
    axis.title = element_text(size = 16))

# Save plot

#save plot
ggsave(
  filename = file.path(review_output_dir, paste0("fig3", ".png")),
  width = 8,
  height = 4,
  dpi = 300
)