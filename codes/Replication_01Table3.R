#===============================================================================
#  File:    01-table3_replication.R
#  Course:  Text as Data – Replication Exercise
#
#  Original paper:
#    Barbera, Casas, Nagler, Egan, Bonneau, Jost, & Tucker
#    “Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#     by Legislators and the Mass Public Using Social Media Data”
#    American Political Science Review
#
#  Replication goal:
#    Using the authors' final time–series dataset, reproduce the correlations
#    between issue-attention distributions for elites and mass publics that
#    appear in Table 3 of the article.
#
#  This script:
#    1. Loads our replicated version of the main time–series file
#       (data_replication/main-time-series.csv).
#    2. Restricts the data to topics coded as politically relevant.
#    3. Computes correlations between:
#         - Democrats in Congress vs. each public/media group
#         - Republicans in Congress vs. each public/media group
#    4. Prints a 5×2 matrix matching the layout of Table 3.
#
#  Data In:
#    ./data_replication/main-time-series.csv
#
#  Data Out:
#    Printed correlation table (replication of Table 3).
#===============================================================================

# PACKAGES
#===============================================================================
# dplyr: data wrangling (filtering, piping)
# xtable: originally used for LaTeX output (we only print to console here)
library(dplyr)
library(xtable)

# DATA
#===============================================================================
# Load the replicated main time–series file (created in our 04/05 scripts)
db <- read.csv("./data_replication/main-time-series.csv")

# DATA WRANGLING
#===============================================================================
# Vector of topics that the authors coded as "political" issues.
# We mirror their coding and include the four merged topics (101–104).
pol_issues <- c(
  3, 7, 9, 12, 14, 15, 16, 18, 20, 23, 28,
  32, 33, 36, 37, 39, 41, 43, 46, 47, 48, 49, 50, 51,
  53, 58, 62, 63, 64, 66, 67, 70, 75, 81, 83, 85, 88,
  89, 93, 96, 97, 99, 100,
  # subissues removed when we merged topics:
  #   27, 56, 11, 74, 38, 59, 17, 26, 35, 42
  # new merged issues:
  101, 102, 103, 104
)

# Keep only rows corresponding to political topics
db <- db %>%
  filter(topic %in% pol_issues)

# MAIN
#===============================================================================
# We compute the correlation between:
#   - Congressional agendas: dem, rep
#   - Public/media agendas: pubdem, pubrep, public, random_us, media
#
# For each congressional group (Democrats, Republicans), we loop over the
# comparison groups, drop rows with missing values, and store the pairwise
# correlations in a matrix.

# Columns for elites (Congress)
outgroups <- c("dem", "rep")

# Columns for public and media groups
covgroups <- c("pubdem", "pubrep", "public", "random_us", "media")

results <- NULL

for (polgroup in outgroups) {
  new_col <- NULL
  
  for (compgroup in covgroups) {
    # Subset to the two series we want to compare
    comp_df <- data.frame(
      y = db[, polgroup],
      x = db[, compgroup]
    )
    
    # Remove rows with missing values in either series
    comp_df <- na.omit(comp_df)
    
    # Compute Pearson correlation and round to two decimals
    cor_out <- round(cor(comp_df)[2, 1], 2)
    new_cell <- data.frame(cor_out)
    colnames(new_cell) <- polgroup
    rownames(new_cell) <- compgroup
    new_col <- rbind(new_col, new_cell)
  }
  
  # Build up a 5×2 correlation table across the loop
  if (is.null(results)) {
    results <- new_col
  } else {
    results <- cbind(results, new_col)
  }
}

# OUTPUT
#===============================================================================
# Add readable labels for rows (public/media groups) and columns (MCs),
# matching the structure of Table 3 in the paper.
rownames(results) <- c(
  "Democratic Supporters",
  "Republican Supporters",
  "Attentive Public",
  "General Public",
  "Media"
)

colnames(results) <- c(
  "Democrats in Congress",
  "Republicans in Congress"
)

# Print the replicated correlation matrix
print(results)

# Example expected structure (values will depend on our replicated data):
#                       Democrats in Congress Republicans in Congress
# Democratic Supporters                  0.69                    0.51
# Republican Supporters                  0.41                    0.77
# Attentive Public                       0.49                    0.52
# General Public                         0.38                    0.34
# Media                                  0.52                    0.63

