#===============================================================================
#  File:    03-figure2_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? Measuring Issue Attention 
#           and Agenda Setting by Legislators and the Mass Public Using Social
#           Media Data"
#  Purpose: Re-estimate the main VAR model using our replicated 
#           main-time-series data and reproduce Figure 2:
#           - 15-day impulse response functions (IRFs) for one-time and 
#             permanent 10-percentage-point increases in attention.
#
#  Data In (replication version): 
#           - ./data_replication/main-time-series.csv
#
#  Data Out (replication version): 
#           - ./var_replication/var_model-MAIN.Rdata
#           - ./var_replication/var_irfs-MAIN.Rdata
#           - ./var_replication/onetime-structural-shock-irfs-results.csv
#           - ./images_replication/figure2.png
#===============================================================================

# PACKAGES
#===============================================================================
library(dplyr)
library(vars)
library(boot)   # for inv.logit
library(rio)

# (Optional for my local machine only)
# setwd("/Users/zhaoshuti/Desktop/TAT/replication")

# Make sure replication folders exist
dir.create("var_replication", showWarnings = FALSE)
dir.create("images_replication", showWarnings = FALSE)

#===============================================================================
#================ PART A: ESTIMATING VAR and CALCULATING IRFs ==================
#===============================================================================

# DATA
#===============================================================================
# Use the replicated main time-series (not the authors' original)
db <- read.csv("data_replication/main-time-series.csv")

# DATA WRANGLING
#===============================================================================
# - filter out non-political issues
pol_issues <- c(
  3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
  32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51, 
  53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
  89, 93, 96, 97, 99, 100,
  # removed subissues: 27, 56, 11, 74, 38, 59, 17, 26, 35, 42
  # new merged issues:
  101, 102, 103, 104
)

# - use the US-located random sample as "random", to match main text
db <- db %>%
  dplyr::select(-random) %>%
  rename(random = random_us)

variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media")

# - keep only variables of interest (plus date/topic)
db <- db[, c("date", "topic", variables)]

# - logit-transform all time series
for (v in variables) {
  x <- db[, v]
  # set missing tail values to a small positive number before logit
  x[which(is.na(x))] <- 0.01
  logit_x <- log(x / (1 - x))
  db[, v] <- logit_x
}

# - add "topic" as model variable and restrict to political issues
variables <- c(variables, "topic")

maindb <- db %>%
  filter(topic %in% pol_issues)

# - build model.matrix: endogenous variables + topic dummies as exogenous
variables <- c("dem", "rep", "pubdem", "pubrep", "public", "random", "media",
               "topic")

maindb$topic <- as.character(maindb$topic)
mformula <- formula(paste0("~", paste0(variables, collapse = " + ")))
model_data <- model.matrix(mformula, maindb[, variables])
model_data <- model_data[, 2:ncol(model_data)]  # drop intercept

# - split endogenous variables and exogenous topic dummies
X_endogenous <- model_data[, !grepl("topic", colnames(model_data))]
X_exogenous  <- model_data[,  grepl("topic", colnames(model_data))]

# - estimate VAR with 7 lags and issue dummies as exogenous controls
var_model_merged <- VAR(y = X_endogenous, p = 7, exogen = X_exogenous)

# - compute cumulative IRFs up to 60 days
var_irfs_cum_merged <- irf(var_model_merged, n.ahead = 60, cumulative = TRUE)

# PART-A OUTPUT (replication)
#===============================================================================
save(var_model_merged,
     file = "var_replication/var_model-MAIN.Rdata")

save(var_irfs_cum_merged,
     file = "var_replication/var_irfs-MAIN.Rdata")

#===============================================================================
#======== PART B: CALCULATING ONE-TIME and PERMANENT 10-POINT EFFECTS ==========
#===============================================================================

# - load IRFs produced in Part A (replication version)
print(load("var_replication/var_irfs-MAIN.Rdata"))
var_irfs <- var_irfs_cum_merged

# - endogenous variables in the VAR
variables <- names(var_irfs$irf)

# - IRF components of interest: point estimate and 95% CI bounds
elements_to_pull <- c("irf", "Upper", "Lower")

# - extract IRFs into a long data frame
irf_data <- NULL
for (el in elements_to_pull) {
  new_irf_info <- var_irfs[el][[1]]
  for (out in variables) {
    new_irf_var_data <- as.data.frame(new_irf_info[[out]])
    # invert logit and convert to percentage point changes around 0.5
    new_irf_var_data_transf <- as.data.frame(
      sapply(1:ncol(new_irf_var_data), function(j) {
        inv.logit(new_irf_var_data[, j]) - 0.5
      })
    )
    colnames(new_irf_var_data_transf) <- colnames(new_irf_var_data)
    new_irf_var_data_long <- new_irf_var_data_transf %>%
      tidyr::gather(cov, value)
    new_irf_var_data_long$out   <- out
    new_irf_var_data_long$day   <- rep(
      1:nrow(new_irf_var_data),
      length(unique(new_irf_var_data_long$cov))
    )
    new_irf_var_data_long$e_type <- el
    irf_data <- rbind(irf_data, new_irf_var_data_long)
  }
}

# - recode estimate type labels
irf_data$e_type <- dplyr::recode(
  irf_data$e_type,
  `irf`   = "pe",
  `Lower` = "lwr",
  `Upper` = "upr"
)

# MAIN (construct one-time vs structural shocks)
#===============================================================================
new_irf_data <- NULL

variables <- unique(irf_data$cov)

DAYS <- 60

irf_data <- irf_data %>%
  dplyr::filter(day <= (DAYS + 1))

for (covariate in variables) {
  for (outcome in variables) {
    if (covariate != outcome) {

      # scenario matrices for cumulative shocks (point, lwr, upr)
      cov_mat <- array(0, dim = c(DAYS, DAYS, 3))
      out_mat <- array(0, dim = c(DAYS, DAYS, 3))

      # IRFs for covariate responses to own shock
      cov_resp <- irf_data %>%
        dplyr::filter(cov == covariate, out == covariate) %>%
        dplyr::filter(day != 1) %>%   # drop day-0 setup
        dplyr::mutate(day = day - 1)

      # IRFs for outcome responses to covariate shock
      out_resp <- irf_data %>%
        dplyr::filter(cov == covariate, out == outcome) %>%
        dplyr::filter(day != 1) %>%
        dplyr::mutate(day = day - 1)

      # wide format: columns = pe/lwr/upr
      or_cov_resp <- cov_resp %>%
        dplyr::select(day, value, e_type) %>%
        tidyr::spread(e_type, value) %>%
        dplyr::select(-day)

      or_out_resp <- out_resp %>%
        dplyr::select(day, value, e_type) %>%
        tidyr::spread(e_type, value) %>%
        dplyr::select(-day)

      # first shock at day 1
      cov_mat[1, , 1:3] <- as.matrix(or_cov_resp)
      out_mat[1, , 1:3] <- as.matrix(or_out_resp)

      # subsequent structural shocks so covariate attention never > 100%
      for (i in 2:DAYS) {
        cov_att_pe    <- sum(cov_mat[, (i - 1), 2])
        cov_new_shock <- 1 - cov_att_pe

        cov_new_resp <- or_cov_resp[1:(DAYS - (i - 1)), ] * cov_new_shock
        out_new_resp <- or_out_resp[1:(DAYS - (i - 1)), ] * cov_new_shock

        cov_mat[i, i:DAYS, 1:3] <- as.matrix(cov_new_resp)
        out_mat[i, i:DAYS, 1:3] <- as.matrix(out_new_resp)
      }

      # stack results for this covariate→outcome pair
      new_rows <- rbind(
        data.frame(
          cov       = covariate,
          value     = colSums(out_mat[, , 1]),
          out       = outcome,
          day       = 1:DAYS,
          e_type    = "lwr",
          data_type = "structural"
        ),
        data.frame(
          cov       = covariate,
          value     = colSums(out_mat[, , 2]),
          out       = outcome,
          day       = 1:DAYS,
          e_type    = "pe",
          data_type = "structural"
        ),
        data.frame(
          cov       = covariate,
          value     = colSums(out_mat[, , 3]),
          out       = outcome,
          day       = 1:DAYS,
          e_type    = "upr",
          data_type = "structural"
        )
      )

      new_irf_data <- rbind(new_irf_data, new_rows)
    }
  }
}

# combine one-time and structural shocks
irf_data$data_type <- "one_time_shock"

irf_data <- irf_data %>%
  dplyr::filter(day != 1) %>%
  dplyr::mutate(day = day - 1)

all_irf_data <- rbind(irf_data, new_irf_data)

# drop covariate = outcome cases
all_irf_data <- all_irf_data %>%
  dplyr::filter(cov != out)

# make wide with separate columns for pe / lwr / upr
all_irf_data_wide <- all_irf_data %>%
  tidyr::spread(e_type, value)

# scale from 100-pt shocks to 10-pt shocks and 0–100 scale
all_irf_data_wide <- all_irf_data %>%
  dplyr::mutate(value = (value / 10) * 100) %>%
  tidyr::spread(e_type, value)

# label covariates and outcomes with readable names
all_irf_data_wide$cov <- dplyr::recode(
  all_irf_data_wide$cov,
  dem    = "Democrats in Congress",
  rep    = "Republicans in Congress",
  pubdem = "Democratic Supporters",
  pubrep = "Republican Supporters",
  public = "Attentive Public",
  random = "General Public",
  media  = "Media"
)

all_irf_data_wide$out <- dplyr::recode(
  all_irf_data_wide$out,
  dem    = "Democrats in Congress",
  rep    = "Republicans in Congress",
  pubdem = "Democratic Supporters",
  pubrep = "Republican Supporters",
  public = "Attentive Public",
  random = "General Public",
  media  = "Media"
)

all_irf_data_wide$out <- factor(
  all_irf_data_wide$out,
  levels = c(
    "Democrats in Congress",
    "Republicans in Congress",
    "Democratic Supporters",
    "Republican Supporters",
    "Attentive Public",
    "General Public",
    "Media"
  )
)

all_irf_data_wide$cov <- factor(
  all_irf_data_wide$cov,
  levels = c(
    "Democrats in Congress",
    "Republicans in Congress",
    "Democratic Supporters",
    "Republican Supporters",
    "Attentive Public",
    "General Public",
    "Media"
  )
)

all_irf_data_wide$data_type <- dplyr::recode(
  all_irf_data_wide$data_type,
  one_time_shock = "Effect of a one time 10 percentage point attention increase at day 0",
  structural     = "Effect of a structural 10 percentage point attention increase at day 0"
)

# PART-B OUTPUT (replication)
#===============================================================================
write.csv(
  all_irf_data_wide,
  "var_replication/onetime-structural-shock-irfs-results.csv",
  row.names = FALSE
)

#===============================================================================
#======================== PART C: GENERATING FIGURE 2 ==========================
#===============================================================================

final_input <- import("var_replication/onetime-structural-shock-irfs-results.csv")

# keep day-15 responses for plotting
plot_db <- final_input %>%
  dplyr::filter(day == 15)

# pretty labels for plotting
plot_db$cov <- dplyr::recode(
  plot_db$cov,
  `Democrats in Congress`   = "Democrats\nin Congress",
  `Republicans in Congress` = "Republicans\nin Congress",
  `Democratic Supporters`   = "Democratic\nSupporters",
  `Republican Supporters`   = "Republican\nSupporters",
  `Attentive Public`        = "Attentive\nPublic",
  `General Public`          = "General\nPublic"
)

plot_db$out <- dplyr::recode(
  plot_db$out,
  `Democrats in Congress`   = "Democrats\nin Congress",
  `Republicans in Congress` = "Republicans\nin Congress",
  `Democratic Supporters`   = "Democratic\nSupporters",
  `Republican Supporters`   = "Republican\nSupporters",
  `Attentive Public`        = "Attentive\nPublic",
  `General Public`          = "General\nPublic"
)

plot_db$cov <- factor(
  plot_db$cov,
  levels = rev(c(
    "Democrats\nin Congress",
    "Republicans\nin Congress",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic",
    "Media"
  ))
)

plot_db$out <- factor(
  plot_db$out,
  levels = c(
    "Democrats\nin Congress",
    "Republicans\nin Congress",
    "Democratic\nSupporters",
    "Republican\nSupporters",
    "Attentive\nPublic",
    "General\nPublic",
    "Media"
  )
)

plot_db$data_type <- ifelse(
  grepl("one time", plot_db$data_type),
  "The effect of a one time 10 percentage point increase in day 0",
  "The effect of a permanent 10 percentage point increase in day 0"
)

# OUTPUT -- FIGURE 2 (replication)
#===============================================================================
p_fig2 <- ggplot(
  plot_db,
  aes(x = cov, y = pe, ymin = lwr, ymax = upr, col = data_type)
) +
  geom_segment(aes(x = cov, xend = cov, y = lwr, yend = upr), size = 2.5) +
  facet_wrap(~ out, nrow = 1) +
  coord_flip() +
  xlab("") +
  scale_y_continuous(
    "\n15-day Responses (in percentage points)",
    limits = c(0, 8),
    expand = c(0, 0)
  ) +
  scale_color_manual("", values = c("gray60", "gray10")) +
  theme(
    panel.spacing      = unit(1.05, "lines"),
    legend.position    = "bottom",
    panel.background   = element_blank(),
    panel.grid.major   = element_line(colour = "gray90", linetype = "solid"),
    axis.text          = element_text(size = 18),
    axis.text.y        = element_text(hjust = 0),
    strip.text         = element_text(size = 20),
    panel.border       = element_rect(colour = "black", fill = FALSE),
    strip.background   = element_rect(colour = "black"),
    axis.title         = element_text(size = 16),
    legend.text        = element_text(size = 16)
  )

png("images_replication/figure2.png", width = 1600, height = 700)
print(p_fig2)
dev.off()
