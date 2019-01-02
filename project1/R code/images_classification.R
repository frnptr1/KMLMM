########################## Step 00: Clear and Set Up Enviroment #########################################

rm(list = ls())
gc()


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("aux_functions.R")
source("images_classification_function.R")

load_packages() # this call will load (and install if needed) all libraries needed to our project


cluster <- makeCluster(3)
registerDoParallel(cluster)
set.seed(12347)

########################## Step 01: Parametrize code #########################################

num_categories <- 10
num_images_x_category <- 70


########################## Step 02: Read images #########################################

dataset_path = "../Images Datasets/Carltech"

categories_list <- list.files(dataset_path)
total_num_categories <- length(categories_list)

selected_categories <- sample(categories_list, num_categories)


#HOG Hyperparameters
n_splits <- 5
n_bins <- 20

# Read images and counte HOG of all the images read.

images <- lapply(selected_categories, function(x, n_splits, n_bins)
   read_category_images(x, dataset_path, n_splits, n_bins, num_images_x_category), n_splits = n_splits, n_bins = n_bins)


# try to plot image. 
# rgb_array <- images[[1]]$images[[2]]
# print_image_from_RGB(rgb_array)


# dataset <- generate_dataset_for_SVM_binary_training(images, positive_class = "brontosaurus")

dataset <- generate_dataset_for_SVM_multiclass_training(images)
# sample = sample.split(dataset$target, SplitRatio = .75)
# train = subset(dataset, sample == TRUE)
# 
# test  = subset(dataset, sample == FALSE)
# response = subset (test, select = target)
# test  = subset(dataset, sample == FALSE, select = -target)

## Compute cross validation error with folds. 

tr_size <- nrow(dataset)
num_folds <- 10
tr.CV.error <- train.svm.kCV(dataset, "RBF", 10, num_folds, tr_size)

orig_dataset = dataset

name_update = c(sprintf("V%02d", 1:(dim(dataset)[2]-1)), "target")
colnames(dataset) <- name_update

#save(file = "./datasets/dataset20cat2hogs.Rda", dataset)

mat = train.svm.one.vs.rest.kCV("linear", 10, num_folds, dataset)
mat


# kernel SVM with caret ---------------------------------------------------

#train_SVM_caret
train_SVM_caret <- function (dataset, method){
  
  # choose randomly which rows will be in train set (75% of dataset)
  sample = sample.split(dataset$target, SplitRatio = .75)
  # define the training set
  train = subset(dataset, sample == TRUE)
  
  # define the test set..
  test  = subset(dataset, sample == FALSE)
  # .. save the response in a new array..
  response = subset (test, select = target)
  # .. and remove the response from test set
  test  = subset(dataset, sample == FALSE, select = -target)
  
  # trainControl help us to control the train process
  # method -> what kind of cross validation we want to perform
  #        -> chosen repeatedcd means that we will repeat simple cross validation a number of times
  # number -> the number of folds for cross validation; chosen 5
  # repeats-> how many times repeat cv process; chosen 3
  control <- trainControl(method="repeatedcv", number=5, repeats=3)
  
  print(paste("---------", method, "---------"))
   
  # build the model with optimal params based on accuracy
  # train function from caret package helps us to tune the parameter(s) of the desired ML algorithm
  # it will perform a cross validation, supervised by the trainControl method declared before, and will
  # chose the parameters that will give the highest accuracy
   model = train(target~., data = train, method = method, metric = "Accuracy", maximize = TRUE,
                 preProcess=c("scale", "center"),trControl=control, tuneLength=5, na.action = na.omit )

   # test the model performance over the test set  
   accuracy_error(model)

}

# list of kernelized svm to be processed
kernel_list = list('svmRadialWeights', 'svmLinear', 'svmPoly','svmRadial', 'svmRadialCost')

# control of tuning parameter process

# cross valididation
models = lapply(kernel_list, ksvm_CV)

accuracy_error = function(model){
 paste("Accuracy on test for ", model$method, " = ", 
                     sum(response$target == predict(model, test))/(dim(response)[1]))
}
#sum(response$target == predict(models[[1]], test))/(dim(response)[1])

lapply(models, test_error)
