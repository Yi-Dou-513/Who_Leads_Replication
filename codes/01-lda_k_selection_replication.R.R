#===============================================================================
#  File:    01-lda_k_selection_replication.R
#  Date:    November 2025
#  Project: Replication of "Who Leads? Who Follows? ..."
#  Purpose: 
#    (1) Build a DTM from congressional tweets.
#    (2) Use cross-validated LDA to choose the number of topics K.
#    (3) Fit a final LDA model with the chosen K.
#  Data In:
#    - ./tweets_congress.csv
#      (raw tweet-level text data for members of Congress)
#  Data Out:
#    - ./k_topics_results_cv.Rdata
#      (cross-validated perplexity / log-likelihood for each K)
#    - ./appendix-choosing-k.pdf
#      (plot of model fit vs. number of topics)
#    - ./lda_results-twokenizer.Rdata
#      (final LDA model with chosen K)
#===============================================================================

# PACKAGES
#===============================================================================
library(slam)
library(Matrix)
library(tm)
library(topicmodels)
library(cvTools)
library(reshape)
library(ggplot2)
library(grid)
library(quanteda)
library(readr)


# DATA: BUILDING CORPUS, DFM, AND DTM
#===============================================================================
# - read raw tweet data (one row per tweet)
data <- read.csv("tweets_congress.csv")

# - set seed for reproducibility of the random sample
set.seed(123)

# - construct a quanteda corpus and draw a 1% random sample of documents
dfm <- corpus(data, text_field = "text") %>%
  corpus_sample(size = ceiling(ndoc(.) * 0.01)) %>%  # ~1% of tweets
  # tokenize: remove punctuation and numbers
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  # build document-feature matrix
  dfm() %>% 
  # stem words
  dfm_wordstem() %>% 
  # remove English stopwords
  dfm_remove(stopwords("en"))

# - convert quanteda DFM to sparse matrix (dgCMatrix)
X <- as(dfm, "dgCMatrix")
dimnames(X)[[2]] <- featnames(dfm)

# - convert to tm's DocumentTermMatrix for use with topicmodels/LDA
library(tm)
mat <- as.simple_triplet_matrix(X)
dtm <- as.DocumentTermMatrix(mat, weighting = function(x) weightTf(x))


# FUNCTIONS
#===============================================================================
# cross-validated LDA:
#   - Ntopics: number of topics (K)
#   - dtm:     DocumentTermMatrix
#   - K:       number of folds (default 10)
cvLDA <- function(Ntopics, dtm, K = 10) {
  folds    <- cvFolds(nrow(dtm), K, 1)
  perplex  <- rep(NA, K)
  llk      <- rep(NA, K)
  
  for (i in unique(folds$which)) {
    cat(i, " ")
    which.test  <- folds$subsets[folds$which == i]
    which.train <- {1:nrow(dtm)}[-which.test]
    
    dtm.train <- dtm[which.train, ]
    dtm.test  <- dtm[which.test, ]
    
    # fit LDA with Gibbs sampling on training folds
    lda.fit <- LDA(
      dtm.train,
      k       = Ntopics,
      method  = "Gibbs",
      control = list(verbose = 50L, iter = 100)
    )
    
    # store perplexity and log-likelihood on test fold
    perplex[i] <- perplexity(lda.fit, dtm.test)
    llk[i]     <- logLik(lda.fit)
  }
  
  return(list(K = Ntopics, perplexity = perplex, logLik = llk))
}


# MAIN 1: CROSS-VALIDATED LDA FOR CHOOSING K
#===============================================================================
# - grid of topic numbers to evaluate
K <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130)

results <- list()
i <- 1

for (k in K) {
  cat("\n\n\n##########\n\n\n ", k, "topics", "\n\n\n")
  res <- cvLDA(k, dtm)
  results[[i]] <- res
  # save intermediate results so long runs can be resumed if needed
  save(results, file = "k_topics_results_cv.Rdata")
  i <- i + 1
}

# - final save of all CV results
save(results, file = "k_topics_results_cv.Rdata")


# PLOT: MODEL FIT VS NUMBER OF TOPICS
#===============================================================================
# Build summary data frame of perplexity and log-likelihood across folds
df <- data.frame(
  k    = rep(K, each = 10),
  perp = unlist(lapply(results, "[[", "perplexity")),
  loglk = unlist(lapply(results, "[[", "logLik")),
  stringsAsFactors = FALSE
)

# Normalize metrics to ratios relative to worst values, for comparability
min(df$perp)
df$ratio_perp <- df$perp  / max(df$perp)
df$ratio_lk   <- df$loglk / min(df$loglk)

# Aggregate mean and sd of ratios by K
df <- data.frame(
  cbind(
    aggregate(df$ratio_perp, by = list(df$k), FUN = mean),
    aggregate(df$ratio_perp, by = list(df$k), FUN = sd)$x,
    aggregate(df$ratio_lk,   by = list(df$k), FUN = mean)$x,
    aggregate(df$ratio_lk,   by = list(df$k), FUN = sd)$x
  ),
  stringsAsFactors = FALSE
)

names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")

# Reshape for plotting (long format)
pd  <- melt(df[, c("k", "ratio_perp", "ratio_lk")], id.vars = "k")
pd2 <- melt(df[, c("k", "sd_perp", "sd_lk")],       id.vars = "k")
pd$sd <- pd2$value
levels(pd$variable) <- c("Perplexity", "LogLikelihood")

# Plot ratios of fit statistics vs K, with error bars
p <- ggplot(pd, aes(x = k, y = value, linetype = variable))

pq <- p +
  geom_line() +
  geom_point(
    aes(shape = variable),
    fill = "white",
    shape = 21,
    size  = 1.40
  ) +
  geom_errorbar(
    aes(ymax = value + sd, ymin = value - sd),
    width = 4
  ) +
  scale_y_continuous("Ratio wrt worst value") +
  scale_x_continuous(
    "Number of topics",
    breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)
  ) +
  theme_bw() +
  scale_shape_discrete(guide = "none") +
  scale_linetype_discrete(guide = "none") +
  theme(
    axis.line        = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border     = element_blank(),
    panel.background = element_blank(),
    legend.key.size  = unit(0.5, "cm"),
    legend.position  = c(0.70, .90),
    legend.box.just  = "left",
    legend.direction = "horizontal",
    legend.title     = element_blank()
  ) +
  annotate("text", x = 100, y = 0.86,  label = "Perplexity",    size = 3) +
  annotate("text", x = 100, y = 0.945, label = "logLikelihood", size = 3)

# Show plot in interactive sessions
pq

# Save plot for appendix (choosing K)
ggsave("appendix-choosing-k.pdf", pq, height = 4, width = 6)


# MAIN 2: FINAL LDA MODEL (CONGRESS)
#===============================================================================
# - fit LDA with chosen K (here, 30 topics), using the full dtm
lda.fit <- LDA(
  dtm,
  k       = 30,
  method  = "Gibbs",
  control = list(verbose = 50L, iter = 2000)
)

# - save final fitted LDA model for downstream analysis (topics, gammas, etc.)
save(lda.fit, file = "lda_results-twokenizer.Rdata")
