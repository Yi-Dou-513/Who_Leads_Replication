#===============================================================================
#  File:    Replication04.R (adapted replication of 04-output-data-for-reg.R)
#  Purpose: Reconstruct Congressional group-level topic attention time series
#           using the authors’ LDA model (lda_results-twokenizer.Rdata) together
#           with our preprocessed and sampled tweet corpus.
#
#  NOTE (replication difference):
#    • The original authors built the time series directly from raw daily DFMs,
#      using full tweet archives and pre-generated fls-list.txt / users-list.txt.
#    • In this replication, we rebuild the same structure using:
#           – tweets_congress.csv (preprocessed tweet-level data)
#           – a 1% sample of tweets for feasibility
#           – the authors’ LDA model (same topics, same gamma matrix)
#
#    As a result, some date–topic–party cells will legitimately be NA because
#    not every party has tweets for every (date, topic) combination in the
#    sampled corpus.
#
#  Input:
#      • tweets_congress.csv (tweet text, metadata, dates)
#      • lda_results-twokenizer.Rdata (authors’ LDA model: lda.fit)
#
#  Output:
#      • data_replication/main-time-series-PRE.csv
#        (DEM/REP topic attention before merging subissues)
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
library(tidyverse)
library(lubridate)


#===============================================================================
# 1. Load authors' LDA model (same topics and gamma matrix used in paper)
#===============================================================================
load("lda_results-twokenizer.Rdata")
K <- ncol(lda.fit@beta)      # number of topics (100)


#===============================================================================
# 2. Load congressional tweets + format dates + build quanteda corpus
#===============================================================================
data <- read.csv("tweets_congress.csv")

# Convert Twitter-style timestamp to Date
data$date <- as.POSIXct(data$date,
                        format = "%a %b %d %H:%M:%S %z %Y",
                        tz = "UTC")
data$date <- as.Date(data$date)

corp <- corpus(data, text_field = "text")


#===============================================================================
# 3. Sample 1% of the corpus (replication constraint)
#    NOTE: The original paper uses the full dataset. Sampling leads to some
#          date–topic–party combinations having 0 documents → NA later.
#===============================================================================
set.seed(123)
corp_samp <- corpus_sample(corp, size = ceiling(ndoc(corp) * 0.01))


#===============================================================================
# 4. Tokenize, create DFM, stem, remove stop words
#    (matches typical preprocessing steps in topic modeling pipelines)
#===============================================================================
dfm <- corp_samp %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  dfm() %>%
  dfm_wordstem() %>%
  dfm_remove(stopwords("en"))

meta <- docvars(dfm)     # extract document metadata aligned with DFM rows


#===============================================================================
# 5. Normalize party labels (DEM / REP only)
#===============================================================================
meta$party <- as.character(meta$Party)
meta$party[meta$party %in% c("D","DEM","Democrat","Dem")] <- "DEM"
meta$party[meta$party %in% c("R","REP","Republican","Rep")] <- "REP"


#===============================================================================
# 6. Extract document–topic probabilities (gamma matrix)
#    Check alignment: one gamma row per document in dfm
#===============================================================================
gamma_mat <- lda.fit@gamma
stopifnot(nrow(gamma_mat) == nrow(meta))   # essential check


#===============================================================================
# 7. Combine metadata (date, party) with topic probabilities
#    Melt to long format: each row = one (doc, topic) probability
#===============================================================================
gamma_df <- data.frame(
  date  = meta$date,
  party = meta$party,
  gamma_mat,
  check.names = FALSE
)

df <- reshape::melt(gamma_df, id.vars = c("date", "party"))
names(df)[names(df) == "variable"] <- "topic"
names(df)[names(df) == "value"]    <- "prop"

df$topic <- as.integer(gsub("\\D", "", df$topic))


#===============================================================================
# 8. Keep only Democratic and Republican MCs
#    (Independents and others dropped, just like original analysis)
#===============================================================================
df <- df[df$party %in% c("DEM","REP"), ]


#===============================================================================
# 9. Aggregate: compute daily mean topic probability by party
#    NOTE: If a party has no tweets for a (date, topic), the mean is missing
#          → produces NA, which is expected.
#===============================================================================
df <- aggregate(prop ~ date + topic + party,
                data = df,
                FUN  = mean)

df <- as.data.frame(cast(df, date + topic ~ party))


#===============================================================================
# 10. Rename columns to match authors’ conventions
#===============================================================================
names(df)[names(df) == "DEM"] <- "dem"
names(df)[names(df) == "REP"] <- "rep"


#===============================================================================
# 11. Placeholder variables (filled in later in 05-adding-random-US…)
#===============================================================================
df$public <- NA
df$pubdem <- NA
df$pubrep <- NA
df$media  <- NA
df$random <- NA


#===============================================================================
# 12. Save preliminary group-level time series
#===============================================================================
dir.create("data_replication", showWarnings = FALSE)

write.csv(
  df,
  file = "data_replication/main-time-series-PRE.csv",
  row.names = FALSE
)
