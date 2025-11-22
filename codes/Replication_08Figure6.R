#===============================================================================
#  File:    08-figure6_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? ..."
#  Purpose: Replicate Figure 6, comparing the ability of different groups
#           to lead the media agenda, and the ability of media to lead others,
#           using 15-day IRFs from a one-time 10 p.p. attention shock.
#  Data In: 
#           - ./onetime-structural-shock-irfs-results.csv
#             (15-day IRFs for one-time 10 percentage point increases)
#  Data Out: 
#           - ./figure6.png
#             (replication of Figure 6)
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(rio)
library(ggplot2)

# DATA
#===============================================================================
# IRF summary results for one-time 10 p.p. shocks (includes pe, lwr, upr, etc.)
db <- import("onetime-structural-shock-irfs-results.csv")


# DATA WRANGLING
#===============================================================================
# Goal: build two panels:
#   (1) Media → (group): effect of media shocks on other groups
#   (2) (group) → Media: effect of group shocks on media

# - 15-day effects, one-time 10 p.p. increase, media as covariate (Media → group)
plot_db_00 <- db %>%
  filter(day == 15) %>% 
  # keep only one-time 10 percentage point changes
  filter(
    data_type == 
      "Effect of a one time 10 percentage point attention increase at day 0"
  ) %>%
  # media is the covariate; outcome is each group
  filter(cov == "Media") %>%
  mutate(data_type = "Media --> (group)") %>%
  # rename for plotting: x = covariate side, y = outcome side
  rename(x = cov, y = out)

# - 15-day effects, one-time 10 p.p. increase, media as outcome ((group) → Media)
plot_db_01 <- db %>%
  filter(day == 15) %>%
  filter(
    data_type == 
      "Effect of a one time 10 percentage point attention increase at day 0"
  ) %>%
  # media is the outcome; covariate is each group
  filter(out == "Media") %>%
  mutate(data_type = "(group) --> Media") %>%
  # rename so x and y roles are symmetric with plot_db_00
  rename(x = out, y = cov)

# - stack both directions in a single dataset
plot_db <- rbind(plot_db_00, plot_db_01)

# - relabel outcome side (y) to readable group names (kept mostly as-is)
plot_db$y <- recode(
  plot_db$y,
  `Democrats in Congress`   = "Democrats in Congress",
  `Republicans in Congress` = "Republicans in Congress",
  `Democratic Supporters`   = "Democratic Supporters",
  `Republican Supporters`   = "Republican Supporters",
  `Attentive Public`        = "Attentive Public",
  `General Public`          = "General Public"
)

# - order outcome groups on the y-axis (top to bottom when flipped)
plot_db$y <- factor(
  plot_db$y,
  levels = rev(c(
    "Democrats in Congress",
    "Republicans in Congress",
    "Democratic Supporters",
    "Republican Supporters",
    "Attentive Public",
    "General Public",
    "Media"
  ))
)

# - order panels by direction of effect (Media → group vs. group → Media)
#   (x is not mapped in aes; we keep factorization for completeness)
plot_db$x <- factor(
  plot_db$x,
  levels = c("Media --> (group)", "(group) --> Media")
)


# PLOT -- FIGURE 6
#===============================================================================
png("figure6.png", width = 1200, height = 400)

ggplot(
  plot_db,
  aes(x = y, y = pe, ymin = lwr, ymax = upr)
) +
  # vertical "bars" showing 95% CI (lwr–upr) for each group
  geom_segment(
    aes(x = y, xend = y, y = lwr, yend = upr),
    size  = 4,
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0, color = "black") +
  facet_grid(~ data_type) +
  coord_flip() +
  # original script includes this vertical line; kept for exact replication
  geom_vline(xintercept = 15) +
  xlab("") +
  scale_y_continuous(
    "\nThe 15-day effect of a one time 10 percentage point increase in attention in day 0",
    expand = c(0, 0.001)
  ) +
  theme(
    panel.spacing    = unit(1.05, "lines"),
    legend.position  = "bottom",
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray90", linetype = "solid"),
    axis.text        = element_text(size = 16),
    axis.text.y      = element_text(hjust = 0),
    strip.text       = element_text(size = 16),
    panel.border     = element_rect(colour = "black", fill = FALSE),
    strip.background = element_rect(colour = "black"),
    axis.title       = element_text(size = 14),
    legend.text      = element_text(size = 16)
  )

dev.off()
