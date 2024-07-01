library("readr")
library("dplyr")
library("tidyverse")

##### read file from your path #####
cfos_data <- read_csv("C:/programming_data/cfos_data/arti_cfos_data.csv")
seq_data <- read_csv("C:/programming_data/cfos_data/trimmed_means.csv")

##### for a specific gene #####
# scattering plot for the nth gene
n <- 3
seq_data_n <- seq_data[n,-1]

plot_data <- tibble(
  seq_value = as.numeric(seq_data_n),
  cfos_value = as.numeric(cfos_data)
)

ggplot(plot_data, aes(x = seq_value, y = cfos_value)) +
  geom_point() +
  labs(
    title = paste("Scatter Plot for nth gene: ", n),
    x = "seq_data values",
    y= "cfos_data values"
  ) +
  theme_minimal()

# calculate PCC and p-value
pcc <- cor.test(as.numeric(seq_data_n), as.numeric(cfos_data), method = "pearson")
#print PCC and p-value
pcc$estimate
pcc$p.value


##### VOLCANO #####
pcc_values <- numeric(nrow(seq_data))
p_values <- numeric(nrow(seq_data))

# caculate pcc for all genes to draw volcano plot
for (i in 1:nrow(seq_data)) {
  seq_data_n <- as.numeric(seq_data[i, -1])
  pcc <- cor.test(as.numeric(seq_data_n), as.numeric(cfos_data), method = "pearson")
  pcc_values[i] <- pcc$estimate
  p_values[i] <- pcc$p.value
}

volcano_plot <- tibble(
  correlation = pcc_values,
  neg_log10_pvalue = -log10(p_values)
)

# draw volcano plot
ggplot(volcano_plot, aes(x = correlation, y = neg_log10_pvalue)) +
  geom_point() +
  labs(
    title = "Volcano Plot",
    x = "Pearson Correlation Coefficient",
    y = "-log10(p-value)"
  ) +
  theme_minimal()
