#===============================================================================
#  File:    Replication_04Figure3.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? Measuring Issue Attention 
#           and Agenda Setting by Legislators and the Mass Public Using Social
#           Media Data" (Barberá et al., APSR 2019)
#
#  Original script: 04-figure3.R
#
#  Replication adjustments:
#  ------------------------
#  (1) The original script uses a **nested for-loop** to merge IRF effects for:
#         - Public → Politicians
#         - Politicians → Public
#      This loop creates thousands of intermediate objects and often causes
#      **RStudio to crash** due to memory spikes or environment copying.
#
#  (2) In this replication, we **replace the entire nested loop** (≈70 lines)
#      with a fully vectorized `dplyr` approach. This:
#        • reduces memory load,
#        • removes repeated rbind inside loops,
#        • avoids crashing,
#        • produces the exact same output structure.
#
#  (3) We also reroute output paths to our replication folder:
#         ./images_replication/figure3.png
#      instead of the original: 
#         ./images/figure3.png
#
#  (4) No analytic logic was changed — only implementation.
#      The IRF interpretation, filtering, and plotting remain identical.
#
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(rio)
library(ggplot2)

# DATA
#===============================================================================
# Input produced by our replication of Figure 2 (03-figure2.R)
db <- import("var_replication/onetime-structural-shock-irfs-results.csv")

# DATA WRANGLING
#===============================================================================
# Keep only:
#   • 15-day IRFs
#   • one-time 10 p.p. shocks
#   • exclude Media (not part of Figure 3)
new_db <- db %>%
  filter(
    day == 15,
    grepl("one time", data_type),
    cov != "Media",
    out != "Media"
  )

# Identify whether a group is political (Congress) or part of the public
new_db <- new_db %>%
  mutate(
    cov_type = ifelse(grepl("Congress", cov), "political", "public"),
    out_type = ifelse(grepl("Congress", out), "political", "public")
  )

#===============================================================================
#  REPLICATION CHANGE: Replace original nested loops
#
#  Original authors' method:
#     for each public group:
#        for each political group:
#            extract responses in both directions
#            rbind into master object
#
#  WHY WE CHANGED IT:
#     - heavy object copying inside loops
#     - rbind inside loops is slow and memory-inefficient
#     - frequently caused R to crash during replication
#
#  OUR VECTORIZED VERSION:
#     - no loops
#     - use two filtered data frames and row-bind once
#     - produces the identical dataset with far greater stability
#===============================================================================

# Public → Politicians
pub_to_pol <- new_db %>%
  filter(cov_type == "public", out_type == "political") %>%
  transmute(
    pubgroup  = cov,
    polgroup  = out,
    direction = "Political response to a change in the Public agenda",
    pe, lwr, upr
  )

# Politicians → Public
pol_to_pub <- new_db %>%
  filter(cov_type == "political", out_type == "public") %>%
  transmute(
    pubgroup  = out,
    polgroup  = cov,
    direction = "Public response to a change in the Political agenda",
    pe, lwr, upr
  )

# Combine both into the full dataset for plotting (equivalent to author output)
plot_db <- bind_rows(pub_to_pol, pol_to_pub)

#===============================================================================
#  Label cleaning + factor ordering (unchanged from original logic)
#===============================================================================

plot_db$pubgroup <- recode(
  plot_db$pubgroup,
  `Democratic Supporters` = "Democratic\nSupporters",
  `Republican Supporters` = "Republican\nSupporters",
  `Attentive Public`      = "Attentive\nPublic",
  `General Public`        = "General\nPublic"
)

plot_db$pubgroup <- factor(
  plot_db$pubgroup,
  levels = c(
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic"
  )
)

plot_db$polgroup <- recode(
  plot_db$polgroup,
  `Democrats in Congress`   = "Democrats\nin Congress",
  `Republicans in Congress` = "Republicans\nin Congress"
)

plot_db$polgroup <- factor(
  plot_db$polgroup,
  levels = rev(c("Democrats\nin Congress", "Republicans\nin Congress"))
)

#===============================================================================
#  OUTPUT — FIGURE 3 (replication version)
#===============================================================================

dir.create("images_replication", showWarnings = FALSE)

png("images_replication/figure3.png", width = 1600, height = 400)
ggplot(
  plot_db,
  aes(x = polgroup, y = pe, ymin = lwr, ymax = upr, col = direction)
) +
  geom_segment(
    aes(x = polgroup, xend = polgroup, y = lwr, yend = upr),
    size = 4, alpha = 1
  ) +
  geom_hline(yintercept = 0, color = "red") +
  facet_wrap(~ pubgroup, nrow = 1) +
  coord_flip() +
  xlab("") +
  ylab("\n15-day effect of a one time 10 percentage point increase") +
  scale_color_manual("", values = c("gray70", "gray30")) +
  theme(
    legend.position   = "bottom",
    panel.background  = element_blank(),
    panel.grid.major  = element_line(colour = "gray90"),
    axis.text         = element_text(size = 20),
    strip.text        = element_text(size = 20),
    panel.border      = element_rect(colour = "black", fill = FALSE),
    strip.background  = element_rect(fill = "gray80", color = "black"),
    axis.title        = element_text(size = 16),
    legend.text       = element_text(size = 16),
    axis.text.y       = element_text(hjust = 0)
  )
dev.off()

print("✅ Replication complete: images_replication/figure3.png written.")

