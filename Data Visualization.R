
library(ggplot2)
library(corrplot)

# Load  featured dataset
data_frame <- read.csv("feature_extraction_data.csv")

# 1. Distribution of AskPrice
ggplot(data_frame, aes(x=AskPrice)) +
  geom_histogram(bins=30, fill="blue", alpha=0.7) +
  labs(title="Distribution of Ask Price", x="Ask Price", y="Frequency")

# 2. Scatter plot of kmDriven vs AskPrice
ggplot(data_frame, aes(x=kmDriven, y=AskPrice, color=factor(isLuxury))) +
  geom_point(alpha=0.7) +
  labs(title="kmDriven vs AskPrice", x="kmDriven", y="Ask Price") +
  theme_minimal()

# 3. Correlation matrix
cor_matrix <- cor(data_frame %>% select(-AskPrice))
corrplot(cor_matrix, method="circle")
