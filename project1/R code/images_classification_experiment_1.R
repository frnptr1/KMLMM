


########################## Step 01: Parametrize experiment #########################################

results_path <- c("../Experiments_results/hog_parameters/")


########################## Step 02: Read images #########################################



categories_list <- list.files(dataset_path)

selected_categories <- sample(categories_list, num_categories)


#HOG Hyperparameters
n_splits <- seq(2, 18, by=3)
n_bins <- seq(6, 22, by=4)

# comb <- data.frame(expand.grid(n_splits, n_bins, n_splits, n_bins))
# comb2 <- comb[comb$Var1 != comb$Var3 | comb$Var2 != comb$Var4,]
# comb <-comb2
# colnames(comb) <- c("n_splits", "n_bins","n_splits", "n_bins" ) 

comb <- data.frame(expand.grid(n_splits, n_bins))
colnames(comb) <- c("n_splits", "n_bins")

# Compute HOG's with each combination of splits and bins and check performance. 

results <- apply(comb, 1, function(x, selected_categories) experiment_1_HOG_params(x, selected_categories) , selected_categories = selected_categories)

comb$tr.CV.accuracy <- round(results*100,2)

comb$n_splits <- as.factor(comb$n_splits)
comb$n_bins <- as.factor(comb$n_bins)






##### Write results to output file 


file_path_output <- paste(results_path,"HOG_hyperparameters_accuracy_results_binary_classification.txt", sep="")

categories <- paste0(selected_categories[1:2], collapse = "_")

line1 <- paste("Categories: ", paste0(selected_categories, collapse = "_"), "; \n",sep = "")
cat(line1, file = file_path_output, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
write.table(comb, file = file_path_output, sep = " ", append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("; \n", file = file_path_output, append = TRUE)



plot_title <- paste(results_path, "Accuracy_for_categories_ ", categories, ".jpeg", sep = "")

jpeg(plot_title) 
# 2. Create a plot
p <- ggplot(data = comb, aes(x=n_splits, y=n_bins, fill=tr.CV.accuracy)) + geom_tile()
print(p)


#Close the pdf file
dev.off() 
# print(p)





