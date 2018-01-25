expression_df <- read.table("expression_data.tsv", sep = "\t", header = TRUE)

summary_df <- expression_df %>%
  filter(expression_df, gene_biotype == "protein_coding")
  group_by(ensembl_gene_id, gene_name, exons_width_bp, cluster, timepoint) %>%
  summarise(mean_counts = mean(norm_counts)) %>%
  mutate(timepoint = factor(timepoint, levels = c("P6", "P10", "P15", "P21", "P50"))) %>%
  mutate(expr_per_kb = mean_counts / (exons_width_bp / 1000) )

ggplot(summary_df, aes(x = timepoint, y = expr_per_kb, fill = as.character(cluster))) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~cluster, scales = "free_y", nrow = 2)
