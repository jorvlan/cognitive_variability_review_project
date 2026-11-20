# make_figure2.R
library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
library(psych); library(cmdstanr); library(reshape2)

# ── inputs saved from the .qmd (run once after rendering) ─────────────────────
#saveRDS(df.LSME,        "df_LSME.rds")
#saveRDS(df.LSME_wider,  "df_LSME_wider.rds")
#saveRDS(real_simdat,    "real_simdat.rds")
# fit2.rds already exists
df.LSME       <- readRDS("df_LSME.rds")
df.LSME_wider <- readRDS("df_LSME_wider.rds")
real_simdat   <- readRDS("real_simdat.rds")
fit2          <- readRDS("fit2.rds")

# functions used below: put plot_meas() and binMSSD() in one file and source it
source("PlotMeasures_Function.R")
source("BinVar_Functions.R")

# squared successive difference (as in the MSSD section)
df.LSME <- df.LSME %>%
  group_by(subject) %>% mutate(ssd = c(NA, diff(y)^2)) %>% ungroup()

# sample_ids_isd and facet labels: copy this code from the .qmd (they are objects, not saved)
# sample_ids_isd, facet_labels_isd_only, facet_labels_mssd_only

# ── sample_ids_isd and facet labels (from the .qmd) ───────────────────────────

# iSD (needed for sample_ids_isd; skip if df.LSME from the .rds already has it)
df.LSME <- df.LSME %>%
  group_by(subject) %>%
  mutate(
    iSD  = sd(y, na.rm = TRUE),
    trend     = predict(lm(y ~ time)),
    y_detrend = y - trend,
    diSD      = sd(y_detrend, na.rm = TRUE),
    MSSD = psych::mssd(y, na.rm = TRUE)
  ) %>%
  ungroup()

# pick subjects with highest, lowest, and middle iSD for illustration
isd_vals <- df.LSME %>% distinct(subject, iSD)
sample_ids_isd <- c(
  isd_vals %>% slice_max(iSD, n = 2) %>% pull(subject),
  isd_vals %>% slice_min(iSD, n = 2) %>% pull(subject),
  isd_vals %>% slice_min(abs(iSD - median(iSD, na.rm = TRUE)), n = 2) %>% pull(subject)
)

# labels with iSD only, for the linear trend plot
label_dat_isd_only <- df.LSME %>%
  filter(subject %in% sample_ids_isd) %>%
  distinct(subject, iSD) %>%
  mutate(label = paste0("Subject ", subject, " | iSD = ", round(iSD, 2)))
facet_labels_isd_only <- setNames(label_dat_isd_only$label, label_dat_isd_only$subject)

# labels with MSSD only, for the successive differences plot
label_dat_mssd_only <- df.LSME %>%
  filter(subject %in% sample_ids_isd) %>%
  distinct(subject, MSSD) %>%
  mutate(label = paste0("Subject ", subject, " | MSSD = ", round(MSSD, 2)))
facet_labels_mssd_only <- setNames(label_dat_mssd_only$label, label_dat_mssd_only$subject)

# labels with iSD and diSD, for the detrended residuals plot
label_dat_disd <- df.LSME %>%
  filter(subject %in% sample_ids_isd) %>%
  distinct(subject, iSD, diSD) %>%
  mutate(label = paste0("Subject ", subject,
                        " | iSD = ",  round(iSD,  2),
                        " | diSD = ", round(diSD, 2)))
facet_labels_disd <- setNames(label_dat_disd$label, label_dat_disd$subject)



dat <- df.LSME %>% filter(subject %in% sample_ids_isd)

# A: linear trend, colour = squared residual
pA <- ggplot(dat, aes(time, y)) +
  geom_line(alpha = 0.4) +
  geom_point(aes(color = y_detrend^2)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_color_viridis_c(option = "magma", name = "Residual²") +
  facet_wrap(~subject, labeller = labeller(subject = facet_labels_isd_only)) +
  labs(title = "Fitted linear trend for 6 subjects") +
  theme(strip.text = element_text(size = 6))

# B: successive differences
pB <- ggplot(dat, aes(time, y)) +
  geom_line(alpha = 0.4) +
  geom_point(aes(color = ssd)) +
  scale_color_viridis_c(option = "magma", name = "Succ. Diff²", na.value = "lightgrey") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~subject, labeller = labeller(subject = facet_labels_mssd_only)) +
  labs(title = "Successive differences per subject") +
  theme(strip.text = element_text(size = 6))

# C: DSEM residual SD per subject
u_draws     <- fit2$draws(variables = "u", format = "df")
sigma_draws <- fit2$draws(variables = "sigma", format = "df")
sd_summary <- u_draws %>%
  select(.draw, matches("u\\[.*,4\\]")) %>%
  pivot_longer(-.draw, names_to = "subject", values_to = "u4") %>%
  mutate(subject = as.integer(gsub("u\\[(\\d+),.*\\]", "\\1", subject))) %>%
  left_join(sigma_draws %>% select(.draw, sigma), by = ".draw") %>%
  mutate(sd = exp(sigma + u4)) %>%
  group_by(subject) %>%
  summarise(mean = mean(sd), lo = quantile(sd, .025), hi = quantile(sd, .975))

pC <- ggplot(sd_summary, aes(subject, mean)) +
  geom_point(size = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  labs(title = "DSEM residual SD (posterior mean ± 95% CI)",
       x = "Subject", y = "Residual SD") +
  theme_minimal()

# D: binary MSSD example trajectories (plot_meas returns a ggplot)
binMSSD_real <- apply(real_simdat, 1, binMSSD)
pD <- plot_meas(real_simdat, binMSSD_real, "Binary MSSD") +
  guides(colour = "none") +
  scale_x_discrete(breaks = NULL)



# E: correlation matrix: wrap the ggmatrix so patchwork accepts it
pairs <- GGally::ggpairs(
  df.LSME_wider[, c("iSD","diSD","iCV","MSSD","MSSD_detrend","rMSSD",
                    "tau","sigma","mu","lm_sigma","dsem_sd")],
  upper = list(continuous = GGally::wrap("cor", size = 2.5, title = ""))) +
  theme(axis.text = element_text(size = 4), strip.text = element_text(size = 5))
pE <- wrap_elements(full = GGally::ggmatrix_gtable(pairs))

pF <- ggplot(dat, aes(x = time, y = y_detrend)) +
  geom_point() +
  geom_line(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~subject, labeller = labeller(subject = facet_labels_disd)) +
  labs(title = "Detrended scores (residuals)", y = "Detrended residual")

# layout: 2 x 2 for A-D, and the pairs plot as a separate figure
fig <- (pB | pC) / (pD | pE) +
  plot_annotation(tag_levels = "A") &
  theme_minimal(base_size = 20)

ggsave("figure2.png", fig, width = 14, height = 11, dpi = 300)

 
fig <- (pA | pF) / (pC | pD) +
   plot_annotation(tag_levels = "A") &
   theme_minimal(base_size = 18) &
   theme(plot.tag = element_text(size = 28, face = "bold")) &
   theme(strip.text = element_text(size = rel(0.6)))
 
 ggsave("figure2.png", fig, width = 20, height = 16, dpi = 300)