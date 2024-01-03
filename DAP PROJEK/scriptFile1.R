install.packages("keras")
install.packages("dplyr")


# Load necessary libraries
library(keras)
library(dplyr)
keras::install_tensorflow()

# Load the dataset
dataset <- read.csv("heart_failure_clinical_records_dataset.csv")

# Normalize the data
dataset[,1:12] <- scale(dataset[,1:12])

# Splitting the dataset into the Training set and Test seta
set.seed(123)
indexes <- sample(1:nrow(dataset), size = 0.8 * nrow(dataset))
train_set <- dataset[indexes, ]
test_set <- dataset[-indexes, ]

model <- keras_model_sequential() %>%
  keras::layer_dense(units = 16, activation = 'relu', input_shape = c(12)) %>%
  keras::layer_dense(units = 8, activation = 'relu') %>%
  keras::layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

# Fit the model
history <- model %>% keras::fit(
  as.matrix(train_set[,1:12]),
  train_set$DEATH_EVENT,
  epochs = 100,
  batch_size = 10,
  validation_split = 0.2
)

# Predicting the Test set results
y_pred <- model %>% predict(as.matrix(test_set[,1:12]))
y_pred <- ifelse(y_pred > 0.5, 1, 0)

# Evaluate the model on the test set
evaluation_results <- model %>% evaluate(
  as.matrix(test_set[,1:12]),
  test_set$DEATH_EVENT
)

# Extracting the test loss and accuracy using indexing
test_loss <- evaluation_results[1]
test_accuracy <- evaluation_results[2]


print(paste("Test accuracy:", test_accuracy))

save_model_hdf5(model, "my_ANN_model.keras")
