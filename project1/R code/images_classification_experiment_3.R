


########################## Step 01: Parametrize experiment #########################################

results_path <- c("../Experiments_results/num_categories/")


########################## Step 02: Read images #########################################

categories_list <- list.files(dataset_path)

selected_categories <- sample(categories_list, num_categories)

categories_list <- list()

#Cost Hyperparameters
for (i in seq(2,num_categories, by=2))
{
  categories_list[[i/2]] <- selected_categories[1:i]
}


# Compute HOG's with each combination of splits and bins and check performance. 

results <- sapply(categories_list, function(x) experiment_3_num_categories(x))




results <- data.frame(number_classes = seq(2,num_categories, by=2), tr.error = round(results*100,2))



##### Write results to output file 


file_path_output <- paste(results_path,"Number_of_parameters_accuracy_results_binary_classification.txt", sep="")

categories <- paste0(selected_categories[1:2], collapse = "_")

line1 <- paste("Selected_categories: ", categories, "; \n",sep = "")
cat(line1, file = file_path_output, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
write.table(results, file = file_path_output, sep = " ", append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("; \n", file = file_path_output, append = TRUE)



plot_title <- paste(results_path, "Number_categories_vs_accuracy",categories,".jpeg", sep = "")

jpeg(plot_title) 
# 2. Create a plot
p <- plot(results, type = "l")
print(p)


#Close the pdf file
dev.off() 
# print(p)





