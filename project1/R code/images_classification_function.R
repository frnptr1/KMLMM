read_category_images <- function(category, dataset_path, n_splits, n_bins, images_xcat,  hogs_number = 1){
   # build category path
   category_path = paste(dataset_path, category, sep = "/")
   # list containing all the images names
   images_list <- list.files(path = category_path)
   # pick a equal number of images per category
   selected_images = sample(images_list, images_xcat, replace = FALSE )
   # build images path to load them
   image_paths <- paste(category_path, selected_images, sep = "/")
   # read them
   images_cat <- lapply(image_paths, read_image)
   # build histogram of gradients on a given image
   if(hogs_number == 1){
     images_hog <- lapply(images_cat, function(x, n_splits, n_bins) 
       HOG(x, cells = n_splits, orientations = n_bins) , n_splits = n_splits, n_bins = n_bins)
     hog <- do.call("rbind", images_hog)
     hog <- as.data.frame(hog)
   }
   else if(hogs_number == 2){
     images_hog <- lapply(images_cat, function(x, n_splits, n_bins) 
        HOG(x, cells = n_splits, orientations = n_bins) , n_splits = n_splits[1], n_bins = n_bins[1])
     images_hog2 <- lapply(images_cat, function(x, n_splits, n_bins) 
       HOG(x, cells = n_splits, orientations = n_bins) , n_splits = (n_splits[2]), n_bins = n_bins[2])
     hog <- do.call("rbind", images_hog)
     hog2 <- do.call("rbind", images_hog2)
     hog <- as.data.frame(hog)
     hog2 <- as.data.frame(hog2)
     hog <- cbind(hog, hog2)
     colnames(hog) <- sprintf("V%d", 1:ncol(hog))
   } else {
     print("hogs_number must be 1 or 2!!")
   }
  
   # retrieve result
   result <- list(images = images_cat,
                  images_hog = hog,
                  images_paths = image_paths, 
                  category = category)
   
}
   

experiment_1_HOG_params <- function(x, selected_categories)
{

  print(x)
  
  # n_splits <- c(x[1], x[3])
  # n_bins <- c(x[2], x[4])

  n_splits <- x[1]
  n_bins <- x[2]
  
  
  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins,n_images), n_splits = n_splits, n_bins = n_bins, n_images=n_images)
  

  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  
  ## Compute cross validation error with folds. 
  
  tr_size <- nrow(dataset)
  tr.CV.accuracy <- train_SVM_caret(dataset, "svmLinear")
  return(tr.CV.accuracy)
  
  
}
experiment_1a_HOG2_params <- function(x, selected_categories)
{
  
  print(x)
  
  print(selected_categories)
  
  n_splits <- c(x[1], x[3])
  n_bins <- c(x[2], x[4])
  
  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins,n_images,2), n_splits = n_splits, n_bins = n_bins, n_images=n_images)
  
  
  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  
  ## Compute cross validation error with folds. 
  
  tr_size <- nrow(dataset)
  tr.CV.accuracy <- train_SVM_caret(dataset, "svmLinear")
  return(tr.CV.accuracy)
  
}

experiment_2_cost_params <- function(x, selected_categories)
{

  print(selected_categories)
  
  cost <- x

  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins,n_images), n_splits = n_splits, n_bins = n_bins, n_images=n_images)
  
  
  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  
  ## Compute cross validation error with folds. 
  
  tr_size <- nrow(dataset)
  tr.CV.accuracy <- train_SVM_caret(dataset, "svmLinear")
  return(tr.CV.accuracy)
}

experiment_6_num_images <- function(x, selected_categories)
{
  
  print(selected_categories)
  
  n_images <- x
  
  print(n_images)
  
  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins, n_images), n_splits = n_splits, n_bins = n_bins, n_images = n_images)
  
  
  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  ## Compute cross validation error with folds. 
  
  tr_size <- nrow(dataset)
  tr.CV.accuracy <- train_SVM_caret(dataset, "svmRadial")
  return(tr.CV.accuracy)
}

experiment_3_num_categories <- function(x)
{
  
  
  selected_categories <- x
  
  print(selected_categories)
  
  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins,n_images), n_splits = n_splits, n_bins = n_bins, n_images=n_images)
  
  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  
  ## Compute cross validation error with folds. 
  
  tr_size <- nrow(dataset)
  num_folds <- 10
  # tr.CV.error <- train.svm.kCV(dataset, "linear", cost, num_folds, tr_size)
  
  tr.CV.error <- train_SVM_caret(dataset, "svmLinear")
  return(tr.CV.error)
  
}


experiment_4_kernels <- function(x, selected_categories)
{
  
  print(selected_categories)
  
  kernel <- x
  
  images <- lapply(selected_categories, function(x, n_splits, n_bins, n_images)
    read_category_images(x, dataset_path, n_splits, n_bins,n_images), n_splits = n_splits, n_bins = n_bins, n_images=n_images)
  
  
  dataset <- generate_dataset_for_SVM_multiclass_training(images)
  
  ## Compute cross validation error with folds. 
  tr_size <- nrow(dataset)
  
  svmGrid <- data.frame(C=rep(100,10), sigma=1:10/100)
  
  # tr.CV.accuracy <- train.svm.kCV(dataset, kernel)
  
  tr.CV.accuracy <- train_SVM_caret(dataset, kernel, svmGrid)
  
  return(tr.CV.accuracy)
  
}





read_image <- function(image_path)
{
  # browser()
  image <- readImage(image_path)
  return(image)
}


print_image_from_RGB<- function(RBG_array)
{
  
  # rgb images
  if (length(dim(RBG_array))==3){
    r <- RBG_array[,,1]
    g <- RBG_array[,,2]
    b <- RBG_array[,,3]
    cols <- rgb(r, g, b, maxColorValue = 1)
    dim(cols) <- dim(r)
    grid.raster(cols, interpolate=FALSE)
  }
  # grayscale images
  if (length(dim(RBG_array))==2){
    cols = gray(RBG_array)
    dim(cols) = dim(RBG_array)
    print(grid.raster(cols, interpolate=FALSE), xlim= c(0, dim()))
  }
}


generate_dataset_for_SVM_multiclass_training <- function(images)
{

  category_names <- unlist(lapply(images, function(x) x$category))
  hogs <- lapply(category_names, function(x, category_names,images) extract_HOGS(x, category_names,images), category_names=category_names,images = images)
  hog_all <- do.call("rbind", hogs)
  return(hog_all)
  
}

extract_HOGS <- function(category_name, categories, images)
{
  class_index <- which(categories == category_name)
  hog<- images[[class_index]]$images_hog
  hog$target <- category_name
  return(hog)
}



#train_SVM_caret
train_SVM_caret <- function (dataset, method, svmGrid = NULL){

  # choose randomly which rows will be in train set (75% of dataset)
  sample = sample.split(dataset$target, SplitRatio = .5)
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
                preProcess=c("scale", "center"),trControl=control, tuneLength=5,  na.action = na.omit )

  
  predictions <- predict(model, test)
  accuracy <-   sum(response$target == predict(model, test))/(dim(response)[1])
  # browser()
  # 
  # file_name_1 <- paste0("conf_matrix_", method, ".csv")
  # file_name_2 <- paste0("accuracy_", method, ".csv")
  # conf_matrix <- confusionMatrix(predict(model, test),as.factor(response$target))
  # write.csv2(conf_matrix$table, file= file_name_1)
  # write.csv2(conf_matrix$byClass, file= file_name_2)
  
  # save(conf_matrix, file = "conf_matrix.RDa")
  # test the model performance over the test set  
  return(accuracy)

}


train.svm.kCV <- function (dataset, which.kernel, mycost=100, num_folds=10)
{
  
  tr_size = nrow(dataset)
  accuracy <- rep(0,num_folds)
  folds <- sample(rep(1:num_folds, length=tr_size), tr_size, replace=FALSE) 
  
  for (i in 1:num_folds) 
  {
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    
    num_x <- dim(train)[2]-1
    
    x_train <- as.matrix(train[,1:num_x])
    t_train <- as.factor(train[,num_x+1])
    
    
    #With package kernlab
    switch(which.kernel,
       linear={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="vanilladot", scaled = TRUE)},
       poly.2={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="polydot", scaled = TRUE , kpar = list(degree=2))},
       poly.3={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="polydot", scaled = TRUE , kpar = list(degree=3))},
       RBF={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="rbfdot", scaled = TRUE)},
       LaplacianRBF={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="laplacedot", scaled = TRUE)},
       stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,1:num_x]
    pred <- predict(model,x_valid)
    t_true <- valid[,num_x+1]
    
    # compute validation error for part 'i'
    accuracy[i] <- sum(pred == t_true)/length(t_true)
  }
  # return average validation error
  sum(accuracy)/length(accuracy)
}




train.svm.one.vs.rest.kCV <- function (which.kernel, mycost = 100, num_folds, img_dataset)
{
  num_models = nlevels(as.factor(img_dataset$target))
  categories = levels(as.factor(img_dataset$target))
  models <- vector(mode="list", length=num_models) #rep(0,num_models)
  
  valid.error <- rep(0,num_folds)
  tr_size = nrow(img_dataset)
  folds <- sample(rep(1:num_folds, length=tr_size), tr_size, replace=FALSE) 
  
  for (i in 1:num_folds) 
  {
  
    train <- img_dataset[folds!=i,] # for building the model (training)
    valid <- img_dataset[folds==i,] # for prediction (validation)
    
    num_x <- dim(train)[2]-1
    
    x_train <- train[,1:num_x]
    t_train <- train[,num_x+1]
    
    for (j in 1:num_models)
    {
      t_cat_train = t_train
      t_cat_train[t_cat_train!=categories[j]] <- 'rest'
      
      # With package kernlab
      switch(which.kernel,
          linear={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="vanilladot", scaled = TRUE)},
          poly.2={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="polydot", scaled = TRUE , kpar = list(degree=2, coef0=1))},
          poly.3={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="polydot", scaled = TRUE , kpar = list(degree=3, coef0=1))},
          RBF={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="rbfdot", scaled = TRUE)},
          LaplacianRBF={model <- ksvm(x_train, t_train, type="C-svc", C=mycost, kernel="laplacedot", scaled = TRUE)},
          stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,1:num_x]
    preds <- 0
    
    # now, since we have few models, we will run the validation samples over each of the models
    # We predict here "one vs. rest" but we will save only the probability of the "one".
    for(j in 1:num_models){
      preds = cbind(preds, attr(predict(models[[j]],x_valid, probability = TRUE),"probabilities")[,categories[j]])
    }
    prob_table = preds[,2:ncol(preds) ]
    colnames(prob_table) <- c(categories)
    
    # classify each object to the class with max probability:
    pred = colnames(prob_table)[max.col(prob_table,ties.method="random")] # can be any of "random" "first" or "last"
    
    t_true <- valid[,num_x+1]
    
    # compute validation error for part 'i'
    accuracy[i] <- sum(pred == t_true)/length(t_true)

  }
  #return average validation error
  sum(accuracy)/length(accuracy)
  }
  
}



