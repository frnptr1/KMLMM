


########################## Step 01: Parametrize experiment #########################################

results_path <- c("../Experiments_results/cost_parameters/")

########################## Step 02: Read images #########################################


categories_list <- list.files(dataset_path)

selected_categories <- sample(categories_list, num_categories)


#Cost Hyperparameters
cost <- c(0.001, 0.01, 0.1, 1, 5, 10, 50, 100 ,1000)


# Compute HOG's with each combination of splits and bins and check performance. 

results <- sapply(cost, function(x, selected_categories) experiment_2_cost_params(x, selected_categories) , selected_categories = selected_categories)


cost <- data.frame(cost = cost, tr.error = round(results*100,2))



##### Write results to output file 


file_path_output <- paste(results_path,"Cost_hyperparameters_accuracy_results_binary_classification.txt", sep="")

categories <- paste0(selected_categories, collapse = "_")

line1 <- paste("Cost: ", categories, "; \n",sep = "")
cat(line1, file = file_path_output, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
write.table(cost, file = file_path_output, sep = " ", append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("; \n", file = file_path_output, append = TRUE)



plot_title <- paste(results_path, "Accuracy_for_categories_ ", categories, ".jpeg", sep = "")

jpeg(plot_title) 
# 2. Create a plot
p <- plot(cost)
print(p)


#Close the pdf file
dev.off() 
# print(p)





