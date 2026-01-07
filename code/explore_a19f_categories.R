# Explore a19f Categories - Empirical Grouping Analysis
# Purpose: Identify which "Steps to Make Friends" categories co-occur

library(here)
library(dplyr)
library(corrplot)
library(ggplot2)

# Load the a19f data directly from the source file
a19f_raw <- read.csv(here("data", "in", "a19f_Master_with_prim_key.csv"), stringsAsFactors = FALSE)

cat("Loaded a19f data:", nrow(a19f_raw), "records\n\n")

# Rename columns to match our naming convention
a19f_raw <- a19f_raw %>%
  rename(
    a19f_response = Response,
    a19f_religious = Religious.Community.Involvement,
    a19f_local_venue = Local.Venue.Visitation,
    a19f_volunteer = Secular.Volunteer.Service,
    a19f_online = Online.Social.Networking,
    a19f_workplace = Workplace.Socializing,
    a19f_athletic = Athletic.Engagement,
    a19f_cultural_hobby = Cultural.Hobby.Participation,
    a19f_neighborhood = Neighborhood.Integration,
    a19f_events_groups = Broader.Event.and.Group.Participation,
    a19f_through_connections = Friendship.Through.Connections,
    a19f_non_engagement = Non.Engagement.Preference,
    a19f_alternative = Alternative.Strategies,
    a19f_not_clear = Not.Clear...Uncodeable.Responses,
    a19f_no_match = No_Match
  )

# Select only the binary coded variables (exclude response text and no_match)
a19f_vars <- c("a19f_religious", "a19f_local_venue", "a19f_volunteer",
               "a19f_online", "a19f_workplace", "a19f_athletic",
               "a19f_cultural_hobby", "a19f_neighborhood", "a19f_events_groups",
               "a19f_through_connections", "a19f_non_engagement",
               "a19f_alternative", "a19f_not_clear")

# Filter to rows with non-empty responses
df_a19f <- a19f_raw %>%
  filter(!is.na(a19f_response) & a19f_response != "") %>%
  select(all_of(a19f_vars))

cat("Observations with a19f data:", nrow(df_a19f), "\n\n")

# -----------------------------------------------------------------------------
# 1. Descriptive Statistics - Frequency of each category
# -----------------------------------------------------------------------------
cat("=== Category Frequencies ===\n")
freq_table <- sapply(df_a19f, function(x) {
  c(n = sum(x == 1, na.rm = TRUE),
    pct = round(100 * mean(x == 1, na.rm = TRUE), 1))
})
freq_df <- as.data.frame(t(freq_table))
freq_df$variable <- rownames(freq_df)
freq_df <- freq_df %>% arrange(desc(n))
print(freq_df)

# -----------------------------------------------------------------------------
# 2. Correlation Matrix
# -----------------------------------------------------------------------------
cat("\n=== Correlation Matrix ===\n")

# Convert to numeric matrix
a19f_matrix <- as.matrix(df_a19f)
a19f_matrix[is.na(a19f_matrix)] <- 0  # Treat NA as 0 for correlation

# Compute Pearson correlations (phi coefficients for binary)
cor_matrix <- cor(a19f_matrix, use = "pairwise.complete.obs")

# Display correlation matrix
print(round(cor_matrix, 2))

# Save correlation plot
pdf(here("out", "a19f_correlation_matrix.pdf"), width = 10, height = 10)
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust",  # Hierarchical clustering order
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.6,
         title = "a19f Categories - Correlations\n(Hierarchically Clustered)",
         mar = c(0, 0, 2, 0))
dev.off()
cat("\nCorrelation plot saved to: out/a19f_correlation_matrix.pdf\n")

# -----------------------------------------------------------------------------
# 3. Hierarchical Clustering
# -----------------------------------------------------------------------------
cat("\n=== Hierarchical Clustering ===\n")

# Distance matrix from correlations
dist_matrix <- as.dist(1 - cor_matrix)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
pdf(here("out", "a19f_dendrogram.pdf"), width = 12, height = 6)
plot(hc, main = "Hierarchical Clustering of a19f Categories",
     xlab = "", sub = "", cex = 0.9)
# Add rectangles for suggested clusters (try 3-5 clusters)
rect.hclust(hc, k = 4, border = "red")
dev.off()
cat("Dendrogram saved to: out/a19f_dendrogram.pdf\n")

# Show cluster assignments for different k values
cat("\nCluster assignments:\n")
for (k in 3:5) {
  cat(sprintf("\n--- %d clusters ---\n", k))
  clusters <- cutree(hc, k = k)
  for (i in 1:k) {
    cat(sprintf("Cluster %d: %s\n", i,
                paste(names(clusters[clusters == i]), collapse = ", ")))
  }
}

# -----------------------------------------------------------------------------
# 4. Principal Components Analysis (as alternative to factor analysis)
# -----------------------------------------------------------------------------
cat("\n=== Principal Components Analysis ===\n")

pca_result <- prcomp(a19f_matrix, scale. = TRUE)
cat("Variance explained by first 5 components:\n")
summary(pca_result)$importance[, 1:min(5, ncol(a19f_matrix))]

# Show loadings for first 3 components
cat("\nPC loadings (first 3 components):\n")
loadings_df <- as.data.frame(pca_result$rotation[, 1:3])
loadings_df$variable <- rownames(loadings_df)
print(loadings_df %>% arrange(desc(abs(PC1))))

# -----------------------------------------------------------------------------
# 5. Co-occurrence Matrix
# -----------------------------------------------------------------------------
cat("\n=== Co-occurrence Analysis ===\n")
cat("Number of times each pair of categories appears together:\n\n")

# Calculate co-occurrence
cooccur <- t(a19f_matrix) %*% a19f_matrix
diag(cooccur) <- 0  # Remove diagonal

# Find top co-occurring pairs
cooccur_df <- as.data.frame(as.table(cooccur))
names(cooccur_df) <- c("var1", "var2", "count")
cooccur_df <- cooccur_df %>%
  filter(var1 < var2) %>%  # Remove duplicates
  arrange(desc(count)) %>%
  head(15)

cat("Top 15 co-occurring pairs:\n")
print(cooccur_df)

cat("\n=== Analysis Complete ===\n")
cat("Review the PDF outputs in out/ folder for visual summaries.\n")
