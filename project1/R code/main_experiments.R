########################## Step 00: Clear and Set Up Enviroment #########################################

rm(list = ls())
gc()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("aux_functions.R")
source("images_classification_function.R")

load_packages() # this call will load (and install if needed) all libraries needed to our project

### in image_classification.R ###
cluster <- makeCluster(3)
registerDoParallel(cluster)

### Default parameters ###

dataset_path = "../Images Datasets/Carltech"

n_splits <- 5
n_bins <- 15
cost <- 100

num_categories <- 10
n_images <- 70



# Remove all results from previous experiments. 


#### Experiment 1 - Number of bins and splits of HOG ####

file.remove(file.path("../Experiments_results/hog_parameters", list.files("../Experiments_results/hog_parameters"))) 

set.seed(12457)
source("images_classification_experiment_1.R")
set.seed(1245)
source("images_classification_experiment_1.R")
set.seed(22457)
source("images_classification_experiment_1.R")


#### Experiment 1a - concatination of HOGs - Number of bins and splits of HOG ####

file.remove(file.path("../Experiments_results/hog2_parameters", list.files("../Experiments_results/hog2_parameters"))) 

set.seed(12457)
source("images_classification_experiment_1a.R")
set.seed(1245)
source("images_classification_experiment_1a.R")
set.seed(22457)
source("images_classification_experiment_1a.R")

#### Experiment 2 - SVM cost parameter ####

file.remove(file.path("../Experiments_results/cost_parameters", list.files("../Experiments_results/cost_parameters"))) 

set.seed(12457)
source("images_classification_experiment_2.R")
set.seed(1245)
source("images_classification_experiment_2.R")
set.seed(22457)
source("images_classification_experiment_2.R")




#### Experiment 3 - Number of categories ####

file.remove(file.path("../Experiments_results/num_categories", list.files("../Experiments_results/num_categories"))) 

set.seed(12457)
source("images_classification_experiment_3.R")
set.seed(1245)
source("images_classification_experiment_3.R")
set.seed(22457)
source("images_classification_experiment_3.R")


#### Experiment 4 - Kernel type ####

file.remove(file.path("../Experiments_results/kernel_type", list.files("../Experiments_results/kernel_type"))) 

set.seed(12457)
source("images_classification_experiment_4.R")
set.seed(1245)
source("images_classification_experiment_4.R")
set.seed(22457)
source("images_classification_experiment_4.R")

#### Experiment 5 - Other classification methods ####

file.remove(file.path("../Experiments_results/methods_benchmark", list.files("../Experiments_results/methods_benchmark"))) 

set.seed(12457)
source("images_classification_experiment_5.R")
set.seed(1245)
source("images_classification_experiment_5.R")
set.seed(22457)
source("images_classification_experiment_5.R")


#### Experiment 6 - Number of images ####

file.remove(file.path("../Experiments_results/num_images", list.files("../Experiments_results/num_images"))) 

set.seed(12457)
source("images_classification_experiment_6.R")
set.seed(1245)
source("images_classification_experiment_6.R")
set.seed(22457)
source("images_classification_experiment_6.R")
