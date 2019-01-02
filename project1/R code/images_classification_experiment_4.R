


########################## Step 01: Parametrize experiment #########################################

results_path <- c("../Experiments_results/kernel_type/")


########################## Step 02: Read images #########################################


categories_list <- list.files(dataset_path)

selected_categories <- sample(categories_list, num_categories)


#kernels Hyperparameters
# kernel_list <- c("linear", "poly.2", "poly.3","RBF", "LaplacianRBF")

# kernel_list = c('svmRadialWeights', 'svmLinear', 'svmPoly','svmRadial', 'svmRadialCost')

kernel_list = c('svmLinear', 'svmPoly','svmRadial')

# Compute HOG's with each combination of splits and bins and check performance. 

results <- sapply(kernel_list, function(x, selected_categories) experiment_4_kernels(x, selected_categories) , selected_categories = selected_categories)


results <- data.frame(kernels = kernel_list, tr.CV.accuracy = round(results*100,2))

##### Write results to output file 

file_path_output <- paste(results_path,"Kernel_choice_accuracy_results_binary_classification.txt", sep="")

categories <- paste0(selected_categories[1:2], collapse = "_")

line1 <- paste("Categories: ", paste0(selected_categories, collapse = "_"), "; \n",sep = "")
cat(line1, file = file_path_output, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
write.table(results, file = file_path_output, sep = " ", append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("; \n", file = file_path_output, append = TRUE)


plot_title <- paste(results_path, "Accuracy_for_categories_", categories, ".jpeg", sep = "")

jpeg(plot_title) 
# 2. Create a plot
p <- barplot(results$tr.CV.accuracy, names.arg = results$kernels)
print(p)


#Close the pdf file
dev.off() 
# print(p)





