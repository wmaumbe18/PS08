library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)



# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime



# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
train <- read_csv("~/Advanced Data Analysis/Rstudio and Git projects/PS08/train.csv")

# YOOGE!
dim(train)



# knn modeling ------------------------------------------------------------
model_formula <- as.formula(Device ~ X + Y + Z)

# Values to use:
n_values <- c(10,50, 100, 150, 200, 250,500,1000, 5000,15000,80000, 100000,500000,1000000, 2500000,5000000)

k_values <- c(2:5)

runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = n*k)
runtime_dataframe




# Time knn here -----------------------------------------------------------

for(i in 1:nrow(runtime_dataframe)){
  n <- runtime_dataframe$n[i]
  k<-runtime_dataframe$k[i]
  samptrain=sample_n(train,n)
  tic()
  model_knn <- caret::knn3(model_formula, data=samptrain, k = k)
  timer_info <- toc()
  runtime_dataframe$runtime[i] <- timer_info$toc - timer_info$tic
  
}




# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=runtime)) +
  geom_line(aes(color=as.factor(k)))+scale_color_discrete(name = "k")+ggtitle("Plot of runtime for a KNN model with 3 predictor variables")
runtime_plot
ggsave(filename="Wayne_Maumbe.png", width=16, height = 9)




# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3
;

#The runtime complexity as a function of n seems to be exponential as suggested by the graph. The graph also roughly shows that even k's have an overall increased runtime than odd ones.
