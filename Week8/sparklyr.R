# Follow http://spark.rstudio.com/ to install sparklyr and spark
# Download Heterogeneity Activity Recognition Data Set from
# http://archive.ics.uci.edu/ml/datasets/Heterogeneity+Activity+Recognition
# Then unzip the file

# Load packages------------------------------------------------------
library(sparklyr)
library(dplyr)

# Create a local connection------------------------------------------
config <- spark_config()
config$spark.driver.memory <- "6g" # 6G memory allocated
sc <- spark_connect(master = "local",
                    config = config)

# Load thedata-------------------------------------------------------
health <- spark_read_csv(sc, "health", 
                         "/Users/xgu/Downloads/Activity recognition exp/Phones_accelerometer.csv")

# Do analysis--------------------------------------------------------
# first a few records
health

# sample some data and load into R
health %>% sample_n(100) %>% collect

# how many records
count(health)

# number of records by Model
health %>%
  group_by(Model) %>%
  count()

# number of records by user?

# By ground truth of action average x, y , z
health %>% 
  group_by(gt) %>%
  summarise(x = mean(x), y = mean(y), z = mean(z))

# Modeling: predict biking-------------------------------------------
# First need create indicator whether biking or not

health_model <- health %>%
  mutate(target = as.numeric(gt == 'bike'))

# check percent of records are bike
health_model %>% summarise(mean(target))

# partition into train and test
# return a list of spark tables
partitions <- health_model %>%
  sdf_partition(training = 0.5, test = 0.5, seed = 111)

# build logistic model
fit <- partitions$training %>%
  ml_logistic_regression(response = "target", features = c("x", "y", "z"))

# prediction
pred <- sdf_predict(fit, partitions$test)

# AUC of the model (~0.61)
ml_binary_classification_eval(pred, 
                              label = "target", 
                              score = "probability", 
                              metric = "areaUnderROC")





