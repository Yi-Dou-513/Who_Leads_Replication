#===============================================================================
#  File:    Replication06.R  (based on 06-intercoder-reliability.R)
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#
#  Task in replication:
#    • Evaluate how consistently five human coders classified the 100 topics
#      from the LDA model as “political” vs. “non-political”.
#    • Reproduce the intercoder reliability statistics reported by the authors:
#        – pairwise agreement matrix
#        – average pairwise intercoder reliability (APIR)
#        – Cronbach’s alpha
#        – distribution of consensus labels
#
#  Input:
#    • dataverse_files/topics/topic-list-classification.csv
#      (rows = topics, columns C1–C5 = coder labels)
#
#  Output (printed to console):
#    • 5×5 matrix of pairwise coder agreement
#    • APIR value and number of coder–coder comparisons
#    • Cronbach’s alpha across coders
#    • Frequency table of consensus (majority) categories
#===============================================================================

# PACKAGES
#===============================================================================
library(psych)   # for Cronbach's alpha
library(gtools)  # for combinations() used in APIR function

# DATA
#===============================================================================
# Read topic classification file:
#   • one row per topic (100 topics)
#   • columns C1–C5: each coder’s political / non-political label
d <- read.csv(
  "dataverse_files/topics/topic-list-classification.csv",
  stringsAsFactors = FALSE
)
coders <- c("C1", "C2", "C3", "C4", "C5")


################################################################################
# 1. Pairwise intercoder agreement matrix
################################################################################

# Initialize a 5×5 matrix where cell (i, j) = proportion of topics
# where coder i and coder j gave the same label.
results <- matrix(
  1,
  nrow = length(coders),
  ncol = length(coders),
  dimnames = list(coders, coders)
)

for (row in coders) {
  for (col in coders) {
    x <- d[, row]
    y <- d[, col]
    # For the 100 topics, compute mean of (x[z] == y[z]):
    #  -> proportion of exact agreement between coder row and coder col
    results[row, col] <- mean(sapply(1:100, function(z) x[z] == y[z]) * 1)
  }
}

results
# (Should reproduce the 5×5 agreement matrix shown in the authors' notes.)


################################################################################
# 2. Average Pairwise Intercoder Reliability (APIR)
################################################################################

# APIR function:
#   • var: vector of all coder responses (stacked)
#   • ids: topic ID for each response
#   For each topic, generate all possible coder–coder pairs and calculate
#   the overall proportion of pairs that agree.
apir <- function(var, ids) {
  
  require(gtools)
  response.pairs <- list()
  i <- 1
  
  for (id in ids) {
    # All responses for this topic ID
    resp <- var[ids == id]
    resp <- resp[!is.na(resp)]
    if (length(resp) < 2) {
      # Need at least two coders to form a pair
      i <- i + 1
      next
    }
    # Make responses unique to avoid dropping duplicates in combinations()
    resp <- paste0(resp, "_", 1:length(resp))
    
    # All unordered coder–coder pairs for this topic
    comb <- combinations(length(resp), 2, resp, repeats = TRUE)
    
    # Strip off the “_index” part to get back original labels
    comb <- matrix(sapply(comb, function(x) gsub("_.", "", x)), ncol = 2)
    
    response.pairs[[i]] <- comb
    i <- i + 1
  }
  
  # Stack all topic-level pairs and compute share that agree
  response.pairs <- do.call(rbind, response.pairs)
  apir <- mean(apply(response.pairs, 1, function(x) x[1] == x[2]))
  return(c(round(apir, 2), nrow(response.pairs)))
}

# Build a long data frame: 100 topics × 5 coders = 500 rows
df <- data.frame(
  id        = rep(1:100, 5),           # topic ID
  responses = unlist(c(d[, coders]))   # coder labels stacked
)

apir(df$responses, df$id)  # first element = APIR (≈ 0.83)


################################################################################
# 3. Internal consistency across coders (Cronbach's alpha)
################################################################################

# Treat coder labels as items and topics as observations;
# alpha() measures overall agreement / consistency across coders.
alpha(d[, coders])


################################################################################
# 4. Consensus category for each topic
################################################################################

# For each topic, compute the modal (most common) label across coders.
d$consensus <- NA

mode <- function(x) {
  tab <- table(x[!is.na(x)])
  names(tab)[which.max(tab)]
}

d$consensus <- apply(d[, -1], 1, mode)

# Distribution of consensus labels (e.g., political vs non-political)
table(d$consensus)


################################################################################
# 5. Key objects to inspect after sourcing Replication06.R
################################################################################
# After running `source("Replication06.R")`, we typically look at:
results                       # 5×5 coder agreement matrix
apir(df$responses, df$id)     # APIR (first value) and number of pairs
alpha(d[, coders])            # Cronbach's alpha for 5 coders
table(d$consensus)            # How many topics classified in each category


library(gt)


# === Compute summary metrics from your existing objects ===

# 1. APIR (first element of the vector returned by apir())
apir_val <- apir(df$responses, df$id)[1]

# 2. Cronbach's alpha (raw alpha)
alpha_val <- psych::alpha(d[, coders])$total$raw_alpha
# (Optional) 3. Number of topics labelled as "political" by consensus
consensus_tab <- table(d$consensus)

# Just take the largest cell as "political" count (same as authors)
political_n <- max(consensus_tab)
total_n     <- sum(consensus_tab)

# === Build a mini summary table for slides ===

mini_df <- data.frame(
  Metric = c(
    "Average pairwise agreement (APIR)",
    "Cronbach's alpha",
    "Topics labeled political (consensus)"
  ),
  Value = c(
    round(apir_val, 2),
    round(alpha_val, 2),
    paste0(political_n, " of ", sum(consensus_tab))
  ),
  stringsAsFactors = FALSE
)

mini_gt <- gt(mini_df)

# === Save as PNG for your slides ===

dir.create("images_replication", showWarnings = FALSE)
gtsave(mini_gt, "images_replication/intercoder_reliability_mini.png")


