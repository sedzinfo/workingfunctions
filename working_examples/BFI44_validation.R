library(rwf)
library(psych)
library(ggplot2)
library(reshape2)

data(df_personality)

# Factor structure
factors <- list(
  Extraversion     = c(1, 6, 11, 16, 21, 26, 31, 36),
  Agreeableness    = c(2, 7, 12, 17, 22, 27, 32, 37, 42),
  Conscientiousness= c(3, 8, 13, 18, 23, 28, 33, 38, 43),
  Neuroticism      = c(4, 9, 14, 19, 24, 29, 34, 39),
  Openness         = c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44)
)

# Reverse-scored items (1-6 scale: subtract from 7)
reverse_items <- c(2, 6, 8, 9, 12, 18, 21, 23, 24, 27, 31, 34, 35, 37, 41, 43)

df <- df_personality
for (i in reverse_items) {
  col <- sprintf("pers%02d", i)
  df[[col]] <- 7 - df[[col]]
}

# Build item labels: E01, A02, C03, N04, O05, E06 ...
# Format: dimension prefix + original item number
dim_prefix <- c(
  Extraversion = "E", Agreeableness = "A",
  Conscientiousness = "C", Neuroticism = "N", Openness = "O"
)
item_name_map <- character(44)          # pers01..pers44 -> labelled name
item_dim_map  <- character(44)          # pers01..pers44 -> dimension name
for (f in names(factors)) {
  for (i in factors[[f]]) {
    old <- sprintf("pers%02d", i)
    r   <- if (i %in% reverse_items) "R" else ""
    item_name_map[i] <- sprintf("%s%02d%s", dim_prefix[f], i, r)
    item_dim_map[i]  <- f
  }
}
names(item_name_map) <- sprintf("pers%02d", 1:44)
names(item_dim_map)  <- sprintf("pers%02d", 1:44)

# Rename columns in df
colnames(df) <- item_name_map[colnames(df_personality)]

##########################################################################################
# CORRELATION MATRIX ORGANIZED BY FACTOR
##########################################################################################
factor_order <- unlist(lapply(names(factors), function(f) {
  old_cols <- sprintf("pers%02d", factors[[f]])
  item_name_map[old_cols]
}))

item_labels        <- unlist(lapply(names(factors), function(f) rep(f, length(factors[[f]]))))
names(item_labels) <- factor_order

cor_mat  <- cor(df[, factor_order], use = "pairwise.complete.obs")
cor_long <- melt(cor_mat)
cor_long$Var1 <- factor(cor_long$Var1, levels = factor_order)
cor_long$Var2 <- factor(cor_long$Var2, levels = factor_order)

p_cor <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  theme_bw(base_size = 9) +
  theme(axis.text.x  = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.y  = element_text(size = 7),
        axis.title   = element_blank(),
        panel.grid   = element_blank()) +
  ggtitle("BFI-44 inter-item correlation matrix\nItems labelled by dimension (E/A/C/N/O) + item number; R = reverse-scored")

print(p_cor)

##########################################################################################
# EXPLORATORY FACTOR ANALYSIS — 5 FACTORS
##########################################################################################
efa <- fa(df, nfactors = 5, rotate = "oblimin", fm = "ml", scores = "regression")

print(efa$loadings, cutoff = 0.30, sort = TRUE)

loadings_mat          <- as.data.frame(unclass(efa$loadings))
colnames(loadings_mat)<- paste0("F", 1:5)
loadings_mat$item     <- rownames(loadings_mat)
loadings_mat$expected <- item_labels[loadings_mat$item]

load_long      <- melt(loadings_mat, id.vars = c("item", "expected"),
                       variable.name = "factor", value.name = "loading")
load_long$item <- factor(load_long$item, levels = factor_order)

p_load <- ggplot(load_long, aes(x = factor, y = item, fill = loading)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(abs(loading) >= 0.30, round(loading, 2), "")),
            size = 2.5) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, limits = c(-1, 1), name = "loading") +
  facet_grid(expected ~ ., scales = "free_y", space = "free_y") +
  theme_bw(base_size = 9) +
  theme(strip.text.y = element_text(angle = 0, size = 8),
        axis.text.y  = element_text(size = 7),
        axis.title   = element_blank(),
        panel.grid   = element_blank()) +
  ggtitle("EFA factor loadings (oblimin rotation, 5 factors)\nValues >= |0.30| shown; R = reverse-scored item")

print(p_load)

##########################################################################################
# SUBSCALE SCORES AND INTERCORRELATIONS
##########################################################################################
subscale_scores <- sapply(names(factors), function(f) {
  cols <- item_name_map[sprintf("pers%02d", factors[[f]])]
  rowMeans(df[, cols], na.rm = TRUE)
})
subscale_scores <- as.data.frame(subscale_scores)

cat("\n--- Subscale descriptives ---\n")
print(psych::describe(subscale_scores))

cat("\n--- Subscale intercorrelations ---\n")
print(round(cor(subscale_scores, use = "pairwise.complete.obs"), 2))

##########################################################################################
# PARALLEL ANALYSIS — CONFIRM 5 FACTORS
##########################################################################################
fa.parallel(df, fm = "ml", fa = "fa",
            main = "Parallel analysis scree plot (BFI-44)")
