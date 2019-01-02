


########################## Step 01: Parametrize experiment #########################################

results_path <- c("../Experiments_results/hog2_parameters/")


########################## Step 02: Read images #########################################



categories_list <- list.files(dataset_path)

selected_categories <- sample(categories_list, num_categories)


#HOG Hyperparameters
n_splits <- seq(4, 8, by=2)
n_splits2 <- seq(10, 16, by=2)
n_bins <- seq(6, 12, by=6)

comb <- data.frame(expand.grid(n_splits, n_bins, n_splits2, n_bins))
comb2 <- comb[comb$Var1 != comb$Var3 | comb$Var2 != comb$Var4,]
comb <-comb2
colnames(comb) <- c("n_splits", "n_bins","n_splits2", "n_bins2") 

# Compute HOG's with each combination of splits and bins and check performance. 

results <- apply(comb, 1, function(x, selected_categories) experiment_1a_HOG2_params(x, selected_categories) , selected_categories = selected_categories)

comb$tr.CV.accuracy <- round(results*100,2)


combtable <- cbind(paste(comb$n_splits, comb$n_bins, sep = "_"),paste(comb$n_splits2, comb$n_bins2, sep = "_"), comb$tr.CV.accuracy)

colnames(combtable) <- c("v1", "v2", "tr.CV.accuracy") 

combtable = as.data.frame(combtable)

combtable$v1 <-as.factor(combtable$v1)
combtable$v2 <-as.factor(combtable$v2)

##### Write results to output file 


file_path_output <- paste(results_path,"HOG_hyperparameters_accuracy_results_binary_classification.txt", sep="")

categories <- paste0(selected_categories, collapse = "_")

line1 <- paste("Categories: ", paste0(selected_categories, collapse = "_"), "; \n",sep = "")
cat(line1, file = file_path_output, sep = " ", fill = FALSE, labels = NULL, append = TRUE)
write.table(combtable, file = file_path_output, sep = " ", append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
cat("; \n", file = file_path_output, append = TRUE)

plot_title <- paste(results_path, "Accuracy_for_categories_", categories, ".jpeg", sep = "")

jpeg(plot_title) 
# 2. Create a plot

colourCount = length(unique(combtable$tr.CV.accuracy))
getPalette = colorRampPalette(brewer.pal(9, "Blues"))

p <- ggplot(data = combtable, aes(x=v1, y=v2, fill= tr.CV.accuracy))+  scale_fill_manual(values = getPalette(colourCount))+ geom_tile()+ theme_bw() +  labs(title=paste("Accuracy for categories ",length(selected_categories)), y="splits_bins", x="splits_bins")
print(p)

#Close the pdf file
dev.off() 
# print(p)





