setwd("~/Documents/University/FIT3152/A1/")
rm(list = ls())
set.seed(1234567)
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows

# Load the required packages
library(ggplot2)

#1.a
#Dimenstions of Data
dim(cvbase)

#Type of data in database
str(cvbase)

#Slight processing to get rid of NA values to discuss numerical distribution
CV = cvbase
CV[,1:10][is.na(CV[,1:10])] <- 0
CV[,39:44][is.na(CV[,39:44])] <- 0
CV <- na.omit(CV)

# Calculate the number of NA values in each specified column
total_nas = function(df, columns) {
  # Calculate the number of NA values in each specified column
  na_values = sapply(df[, columns], function(x) sum(is.na(x)))
  
  # Create a vector to store the sentences
  sentences = c()
  
  # Create the sentences
  for (i in seq_along(na_values)) {
    sentences = c(sentences, paste0(names(na_values)[i], " contains ", na_values[i], " N/A values"))
  }
  
  # Print all sentences in a single sentence
  print(paste(sentences, collapse = ", "))
}

#Create a percentage distribution bar-chart for specified numerical columns
bar_chart_summary = function(columns, CV, xlab, ylab, titleInput) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()
  
  for (col_num in columns){
    # Calculate the percentage of 1s in each column for the CV dataset
    CV_percentage = sum(CV[, col_num] == 1) / nrow(CV) * 100
    
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = colnames(CV)[col_num],
      Value = CV_percentage
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])
  
  # Plot the bar chart
  ggplot(summary_dataset, aes(x = Variable, y = Value)) +
    geom_bar(stat = "identity", position = position_dodge(), fill = "skyblue") +
    geom_text(aes(label = paste0(round(Value, 1), "%")), vjust = -0.3) +
    labs(x = xlab, y = ylab, title = titleInput) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_text(size = 8, face = "bold")  # Make x-axis variable names smaller and bolded
    )
}

#Create boxplots for specified columns
five_number_summary = function(columns, CV, xlab, ylab, titleInput, xSize) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()
  
  for (col_num in columns){
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = colnames(CV)[col_num],
      Value = CV[, col_num]
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])
  
  ggplot(summary_dataset, aes(x = Variable, y = Value)) +
    geom_boxplot() +
    labs(x = xlab, y = ylab, title = titleInput) + 
    scale_y_continuous(breaks = unique(summary_dataset$Value)) +
    theme(
      plot.title = element_text(hjust = 0.1, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_text(size = xSize, face = "bold"),  # Make x-axis variable names smaller and bolded
      axis.text.y = element_text(size = 10, face = "bold")
    )
}

#Create a percentage distribution bar-chart for specified categorical columns
rank_ordLife_summary = function(columns, CV, xlab, ylab, titleInput) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()
  
  for (col_num in columns){
    # Calculate the percentage of each category in each column for CV dataset
    CV_A = sum(CV[, col_num] == 'A') / nrow(CV) * 100
    CV_B = sum(CV[, col_num] == 'B') / nrow(CV) * 100
    CV_C = sum(CV[, col_num] == 'C') / nrow(CV) * 100
    CV_D = sum(CV[, col_num] == 'D') / nrow(CV) * 100
    CV_E = sum(CV[, col_num] == 'E') / nrow(CV) * 100
    CV_F = sum(CV[, col_num] == 'F') / nrow(CV) * 100
    
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = rep(colnames(CV)[col_num], 6),
      Value = c(CV_A, CV_B, CV_C, CV_D, CV_E, CV_F),
      Category = c('A', 'B', 'C', 'D', 'E', 'F')
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])
  
  ggplot(summary_dataset, aes(x = Variable, y = Value)) +
    facet_grid(~Category) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = paste0(round(Value, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.25) +
    labs(x = xlab, y = ylab, title = titleInput) + 
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_blank()  # Make x-axis variable names smaller and bolded
    )
}

#Frequency Chart for countries
frequency_chart_countries = function(CV) {
  # Count the occurrences of each coded_country
  country_counts = as.data.frame(table(CV$coded_country))
  
  # Rename the columns
  colnames(country_counts) = c("Country", "Count")
  
  # Convert Country to a factor
  country_counts$Country = factor(country_counts$Country, levels = unique(country_counts$Country))
  
  # Create a bar plot
  ggplot(country_counts, aes(x = Country, y = Count)) +
    geom_bar(stat = "identity") +
    labs(x = "Country", y = "Count", title = "Number of Occurrences of Each Coded Country") + 
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels 90 degrees
    )
}

#Use the above defined functions to create all the graphs and find all teh N/A values
bar_chart_summary(1:10, CV, "Variables", "Percentage of Responses", "Distribution of Employment")

five_number_summary(11:14, CV, "Variables", "Response", "Five Number Summary of Isolation Variables", 9)
total_nas(cvbase, 11:14)

five_number_summary(15:17, CV, "Variables", "Response", "Five Number Summary of Loneliness Variables", 10)
total_nas(cvbase, 15:17)

five_number_summary(18, CV, "Variables", "Response", "Five Number Summary of Happy Variable", 14)
five_number_summary(19, CV, "Variables", "Response", "Five Number Summary of Life Satisfaction Variable", 14)
five_number_summary(20, CV, "Variables", "Response", "Five Number Summary of MLQ Variable", 14)
total_nas(cvbase, 18:20)

five_number_summary(21:23, CV, "Variables", "Response", "Five Number Summary of Boredom Variables", 12)
total_nas(cvbase, 21:23)

five_number_summary(24:26, CV, "Variables", "Response", "Five Number Summary of Conspiracy Variables", 12)
total_nas(cvbase, 24:26)

rank_ordLife_summary(27, CV, "rankOrdLife_1", "Percentage", "rankOrdLife_1 Comparison")
rank_ordLife_summary(28, CV, "rankOrdLife_2", "Percentage", "rankOrdLife_2 Comparison")
rank_ordLife_summary(29, CV, "rankOrdLife_3", "Percentage", "rankOrdLife_3 Comparison")
rank_ordLife_summary(30, CV, "rankOrdLife_4", "Percentage", "rankOrdLife_4 Comparison")
rank_ordLife_summary(31, CV, "rankOrdLife_5", "Percentage", "rankOrdLife_5 Comparison")
rank_ordLife_summary(32, CV, "rankOrdLife_6", "Percentage", "rankOrdLife_6 Comparison")
total_nas(cvbase, 27:32)

five_number_summary(33:35, CV, "Variables", "Response", "Five Number Summary of Corona Personal Behaviour Variables", 10)
total_nas(cvbase, 33:35)

five_number_summary(36:38, CV, "Variables", "Response", "Five Number Summary of Corona RadicalAction Variables", 10)
total_nas(cvbase, 36:38)

bar_chart_summary(39:44, CV, "Variables", "Percentage of Responses", "Distribution of Corona Proximity")

five_number_summary(45, CV, "Variables", "Response", "Five Number Summary of Gender", 14)
five_number_summary(46, CV, "Variables", "Response", "Five Number Summary of Age", 14)
five_number_summary(47, CV, "Variables", "Response", "Five Number Summary of Education", 14)
total_nas(cvbase, 45:48)

frequency_chart_countries(CV)

five_number_summary(49:52, CV, "Variables", "Response", "Five Number Summary of Corona ProSocial Behaviour Variables", 11)
total_nas(cvbase, 49:52)


#1.b
#Redifine data for actual pre-processing
CV = cvbase

#Change "tick box" N/As to 0 and tehn remove all N/As
CV[,1:10][is.na(CV[,1:10])] <- 0
CV[,39:44][is.na(CV[,39:44])] <- 0
CV <- na.omit(CV)

# Remove rows with empty strings in 'coded_country'
CV <- CV[CV$coded_country != "", ]

#Chnage all non-Russia countres to "Other" and create new Data ssets for each "group"
CV$coded_country[CV$coded_country != "Russia"] <- "Other"
Russia = CV[CV$coded_country == "Russia",]
Other = CV[CV$coded_country == "Other",]


#2.a

#Create a comparison box-plots for specified numerical columns between Russia and Other
Compare_five_number_summary = function(columns, CV, Russia, Other, xlab, ylab, titleInput, xSize) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()

  for (col_num in columns){
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = colnames(CV)[col_num],
      Value = c(Russia[, col_num], Other[, col_num]),
      Country = rep(c("Russia", "Other"), c(nrow(Russia), nrow(Other)))
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])

  ggplot(summary_dataset, aes(x = Variable, y = Value, fill = Country)) +
  geom_boxplot() +
  labs(x = xlab, y = ylab, title = titleInput) + 
  scale_y_continuous(breaks = unique(summary_dataset$Value)) +
  theme(
    plot.title = element_text(hjust = 0.1, face = "bold"),  # Center, enlarge, and bold the title
    axis.text.x = element_text(size = xSize, face = "bold")  # Make x-axis variable names smaller and bolded
  )
}

#Create a comparison percentage distribution bar-chart for specified numerical columns between Russia and Other
Compare_bar_chart_summary = function(columns, CV, Russia, Other, xlab, ylab, titleInput) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()
  
  for (col_num in columns){
    # Calculate the percentage of 1s in each column for Russia and Other datasets
    Russia_percentage = sum(Russia[, col_num] == 1) / nrow(Russia) * 100
    Other_percentage = sum(Other[, col_num] == 1) / nrow(Other) * 100
    
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = colnames(CV)[col_num],
      Value = c(Russia_percentage, Other_percentage),
      Country = c("Russia", "Other")
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])
  
  ggplot(summary_dataset, aes(x = Variable, y = Value, fill = Country)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = xlab, y = ylab, title = titleInput) + 
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_text(size = 8, face = "bold", angle = 90)  # Make x-axis variable names smaller and bolded
    )
}

#Create a comparison percentage distribution bar-chart for specified categorical columns between Russia and Other
Compare_rank_ordLife_summary = function(columns, CV, Russia, Other, xlab, ylab, titleInput) {
  # Create an empty data frame to store the summaries
  summary_dataset = data.frame()
  
  for (col_num in columns){
    # Calculate the percentage of each category in each column for Russia and Other datasets
    Russia_A = sum(Russia[, col_num] == 'A') / nrow(Russia) * 100
    Russia_B = sum(Russia[, col_num] == 'B') / nrow(Russia) * 100
    Russia_C = sum(Russia[, col_num] == 'C') / nrow(Russia) * 100
    Russia_D = sum(Russia[, col_num] == 'D') / nrow(Russia) * 100
    Russia_E = sum(Russia[, col_num] == 'E') / nrow(Russia) * 100
    Russia_F = sum(Russia[, col_num] == 'F') / nrow(Russia) * 100
    
    Other_A = sum(Other[, col_num] == 'A') / nrow(Other) * 100
    Other_B = sum(Other[, col_num] == 'B') / nrow(Other) * 100
    Other_C = sum(Other[, col_num] == 'C') / nrow(Other) * 100
    Other_D = sum(Other[, col_num] == 'D') / nrow(Other) * 100
    Other_E = sum(Other[, col_num] == 'E') / nrow(Other) * 100
    Other_F = sum(Other[, col_num] == 'F') / nrow(Other) * 100
    
    # Create a temporary data frame with the summaries
    temp_df = data.frame(
      Variable = rep(colnames(CV)[col_num], 6),
      Value = c(Russia_A, Russia_B, Russia_C, Russia_D, Russia_E, Russia_F, Other_A, Other_B, Other_C, Other_D, Other_E, Other_F),
      Category = rep(c('A', 'B', 'C', 'D', 'E', 'F'), 2),
      Country = rep(c("Russia", "Other"), each = 6)
    )
    
    # Add the temporary data frame to the summaries data frame
    summary_dataset = rbind(summary_dataset, temp_df)
  }
  
  # Convert Variable to a factor and specify the levels
  summary_dataset$Variable = factor(summary_dataset$Variable, levels = colnames(CV)[columns])
  
  ggplot(summary_dataset, aes(x = Variable, y = Value, fill = Country)) +
    facet_grid(~Category) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = xlab, y = ylab, title = titleInput) + 
    theme(
      plot.title = element_text(size = 14, face = "bold"),  # Center, enlarge, and bold the title
      axis.text.x = element_blank()  # Make x-axis variable names smaller and bolded
    )
}


#Use the above defined functions to create all the graphs
Compare_bar_chart_summary(1:10, CV, Russia, Other, "Variables", "Percentage of Responses", "Distribution of Employment")
Compare_five_number_summary(11:14, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Isolation Variables", 10)
Compare_five_number_summary(15:17, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Loneliness Variables", 10)
Compare_five_number_summary(18, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Happy Variable", 14)
Compare_five_number_summary(19, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Life Satisfaction Variable", 14)
Compare_five_number_summary(20, CV, Russia, Other, "Variables", "Response", "Five Number Summary of MLQ Variable", 14)
Compare_five_number_summary(21:23, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Boredom Variables", 12)
Compare_five_number_summary(24:26, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Conspiracy Variables", 12)
Compare_rank_ordLife_summary(27, CV, Russia, Other, "rankOrdLife_1", "Percentage", "rankOrdLife_1 Comparison")
Compare_rank_ordLife_summary(28, CV, Russia, Other, "rankOrdLife_2", "Percentage", "rankOrdLife_2 Comparison")
Compare_rank_ordLife_summary(29, CV, Russia, Other, "rankOrdLife_3", "Percentage", "rankOrdLife_3 Comparison")
Compare_rank_ordLife_summary(30, CV, Russia, Other, "rankOrdLife_4", "Percentage", "rankOrdLife_4 Comparison")
Compare_rank_ordLife_summary(31, CV, Russia, Other, "rankOrdLife_5", "Percentage", "rankOrdLife_5 Comparison")
Compare_rank_ordLife_summary(32, CV, Russia, Other, "rankOrdLife_6", "Percentage", "rankOrdLife_6 Comparison")
Compare_five_number_summary(33:35, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Corona Personal Behaviour Variables", 10)
Compare_five_number_summary(36:38, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Corona RadicalAction Variables", 10)
Compare_bar_chart_summary(39:44, CV, Russia, Other, "Variables", "Percentage of Responses", "Distribution of Corona Proximity")
Compare_five_number_summary(45, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Gender", 14)
Compare_five_number_summary(46, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Age", 14)
Compare_five_number_summary(47, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Education", 14)
Compare_five_number_summary(49:52, CV, Russia, Other, "Variables", "Response", "Five Number Summary of Corona ProSocial Behaviour Variables", 11)


#2.b
#Save linear model metrics to a csv file
save_model_metrics = function(file_name, models) {
  # Create an empty data frame to store the metrics
  metrics <- data.frame()
  
  # Loop over the models
  for (i in 1:length(models)) {
    model <- models[[i]]
    model_summary <- summary(model)
    
    # Extract the median residual
    median_residual <- median(model$residuals)
    
    # Extract the multiple R-squared
    multiple_R_squared <- model_summary$r.squared
    
    # Extract the p-value
    p_value <- pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE)
    
    # Create a temporary data frame with the metrics
    temp_df <- data.frame(
      Model = paste0("c19ProSo0", i),
      Median_Residual = signif(median_residual, 4),
      Multiple_R_Squared = signif(multiple_R_squared, 4),
      P_Value = signif(p_value, 4)
    )
    
    # Add the temporary data frame to the metrics data frame
    metrics <- rbind(metrics, temp_df)
  }
  
  # Print the metrics
  print(metrics)
  
  # Save the dataframe as a CSV file
  write.csv(metrics, file = file_name, row.names = FALSE)
}

#Graph inverse p-value
graph_inverse_p_values = function(model) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract the p-values and calculate their inverse
  inverse_p_values <- 1 / summary_model$coefficients[, "Pr(>|t|)"]
  
  # Create a data frame with the variable names and the inverse p-values
  data <- data.frame(Variable = names(inverse_p_values), InversePValue = inverse_p_values)
  
  data$Variable = factor(data$Variable, levels = names(inverse_p_values))
  
  # Add a new variable for the color
  data$Color = "Bars"
  
  model_name <- deparse(substitute(model))
  
  # Create the plot
  ggplot(data, aes(x = Variable, y = InversePValue, fill = Color)) +
    geom_bar(stat = "identity") +
    geom_hline(aes(yintercept = 1/0.01, color = "Line"), linetype = "dashed") +
    scale_fill_manual(values = c("Bars" = "skyblue"), labels = c("Bars" = "Inverse p-values")) +
    scale_color_manual(values = c("Line" = "red"), labels = c("Line" = "Inverse 0.01 signifcance line (bars above = better)")) +
    labs(x = "Variable", y = "Inverse P-Value (bigger = better)", title = paste("Inverse P-Value of each Attribute in", model_name), fill = "Legend", color = "Legend") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
      legend.position = "top"
    ) +
    coord_cartesian(ylim = c(0, 1000))  # Set the y-axis limit
}

# Fit the models
CVR_c19ProSo01 = subset(CV, CV$coded_country == "Russia", select = -c(coded_country, c19ProSo02, c19ProSo03, c19ProSo04, rankOrdLife_6))
Russia_c19ProSo01_fitted = lm(c19ProSo01 ~ ., data = CVR_c19ProSo01)
summary(Russia_c19ProSo01_fitted)

CVR_c19ProSo02 = subset(CV, CV$coded_country == "Russia", select = -c(coded_country, c19ProSo01, c19ProSo03, c19ProSo04, rankOrdLife_6))
Russia_c19ProSo02_fitted = lm(c19ProSo02 ~ ., data = CVR_c19ProSo02)
summary(Russia_c19ProSo02_fitted)

CVR_c19ProSo03 = subset(CV, CV$coded_country == "Russia", select = -c(coded_country, c19ProSo01, c19ProSo02, c19ProSo04, rankOrdLife_6))
Russia_c19ProSo03_fitted = lm(c19ProSo03 ~ ., data = CVR_c19ProSo03)
summary(Russia_c19ProSo03_fitted)

CVR_c19ProSo04 = subset(CV, CV$coded_country == "Russia", select = -c(coded_country, c19ProSo01, c19ProSo02, c19ProSo03, rankOrdLife_6))
Russia_c19ProSo04_fitted = lm(c19ProSo04 ~ ., data = CVR_c19ProSo04)
summary(Russia_c19ProSo04_fitted)

#Save model data and create inverse p-value graphs
save_model_metrics("Russia_Models.csv", list(Russia_c19ProSo01_fitted, Russia_c19ProSo02_fitted, Russia_c19ProSo03_fitted, Russia_c19ProSo04_fitted))
graph_inverse_p_values(Russia_c19ProSo01_fitted)
graph_inverse_p_values(Russia_c19ProSo02_fitted)
graph_inverse_p_values(Russia_c19ProSo03_fitted)
graph_inverse_p_values(Russia_c19ProSo04_fitted)

#2.c
#Graph inverse p-value between Russia and 'Other'
sidebyside_inverse_p_values = function(models, name) {
  # Initialize an empty data frame
  data <- data.frame()
  
  # Loop over the models
  for (model_name in names(models)) {
    # Get the summary of the model
    summary_model <- summary(models[[model_name]])
    
    # Extract the p-values and calculate their inverse
    inverse_p_values <- 1 / summary_model$coefficients[, "Pr(>|t|)"]
    
    # Create a data frame with the variable names, the inverse p-values, and the model name
    data_model <- data.frame(Variable = names(inverse_p_values), InversePValue = inverse_p_values, Model = model_name)
    
    # Add the data from this model to the main data frame
    data <- rbind(data, data_model)
  }
  
  data$Variable = factor(data$Variable, levels = unique(data$Variable))
  
  # Create the plot
  ggplot(data, aes(x = Variable, y = InversePValue, fill = Model)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(aes(yintercept = 1/0.01, color = "Line"), linetype = "dashed") +
    scale_color_manual(values = c("Line" = "red"), labels = c("Line" = "Inverse 0.01 signifcance line (bars above = better)")) +
    labs(x = "Variable", y = "Inverse P-Value (bigger = better)", title = paste("Inverse P-Value of each Attribute in", name), fill = "Model", color = "Legend") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 90, hjust = 1, face = "bold"),
      legend.position = "top"
    ) +
    coord_cartesian(ylim = c(0, 1000))
}

#Fit the models for 'Other'
CVO_c19ProSo01 = subset(CV, CV$coded_country == "Other", select = -c(coded_country, rankOrdLife_6, c19ProSo02, c19ProSo03, c19ProSo04))
Other_c19ProSo01_fitted = lm(c19ProSo01 ~ ., data = CVO_c19ProSo01)
summary(Other_c19ProSo01_fitted)

CVO_c19ProSo02 = subset(CV, CV$coded_country == "Other", select = -c(coded_country, rankOrdLife_6, c19ProSo01, c19ProSo03, c19ProSo04))
Other_c19ProSo02_fitted = lm(c19ProSo02 ~ ., data = CVO_c19ProSo02)
summary(Other_c19ProSo02_fitted)

CVO_c19ProSo03 = subset(CV, CV$coded_country == "Other", select = -c(coded_country, rankOrdLife_6, c19ProSo01, c19ProSo02, c19ProSo04))
Other_c19ProSo03_fitted = lm(c19ProSo03 ~ ., data = CVO_c19ProSo03)
summary(Other_c19ProSo03_fitted)

CVO_c19ProSo04 = subset(CV, CV$coded_country == "Other", select = -c(coded_country, rankOrdLife_6, c19ProSo01, c19ProSo02, c19ProSo03))
Other_c19ProSo04_fitted = lm(c19ProSo04 ~ ., data = CVO_c19ProSo04)
summary(Other_c19ProSo04_fitted)

#Save model data and create inverse p-value graphs for Other and comparison inverse p-value graphs between 'Other' and "russia"
save_model_metrics("Other_Models.csv", list(Other_c19ProSo01_fitted, Other_c19ProSo02_fitted, Other_c19ProSo03_fitted, Other_c19ProSo04_fitted))
graph_inverse_p_values(Other_c19ProSo01_fitted)
graph_inverse_p_values(Other_c19ProSo02_fitted)
graph_inverse_p_values(Other_c19ProSo03_fitted)
graph_inverse_p_values(Other_c19ProSo04_fitted)

sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo01_fitted, Other = Other_c19ProSo01_fitted), "c19ProSo01")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo02_fitted, Other = Other_c19ProSo02_fitted), "c19ProSo02")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo03_fitted, Other = Other_c19ProSo03_fitted), "c19ProSo03")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo04_fitted, Other = Other_c19ProSo04_fitted), "c19ProSo04")


#3.a
#Read clustering data
ClusteringData = read.csv("ClusteringData.csv", header = TRUE)
ClusteringDataScaled = ClusteringData 
ClusteringDataScaled[,2:11] = scale(ClusteringDataScaled[,2:11]) #Scale clustering Data
rownames(ClusteringDataScaled) <- ClusteringDataScaled[,1] # Set row names to be the countries

#Create teh Cluster and plot a dendrogram for the cluster
CDSFit = hclust(dist(ClusteringDataScaled[,2:11]), "ave")
plot(CDSFit, hang = -1)
rect.hclust(CDSFit, k = 50, border = "red") #Red boxes at k-50.

#3.b
#Pre-process data to create new data set only containing clustered countries.
CV_clustering = cvbase

CV_clustering[,1:10][is.na(CV_clustering[,1:10])] <- 0
CV_clustering[,39:44][is.na(CV_clustering[,39:44])] <- 0
CV_clustering <- na.omit(CV_clustering)

#Select only clustered countries
CV_clustering = subset(CV_clustering, coded_country %in% c("Azerbaijan", "Mexico", "Philippines", "Indonesia", "Moldova", "Belarus", "Kazakhstan"))
#Check if it worrked!
table(CV_clustering$coded_country)

#Fit the models
CVC_c19ProSo01 = subset(CV_clustering, select = -c(coded_country, rankOrdLife_6, c19ProSo02, c19ProSo03, c19ProSo04))
Clustered_c19ProSo01_fitted = lm(c19ProSo01 ~ ., data = CVC_c19ProSo01)
summary(Clustered_c19ProSo01_fitted)

CVC_c19ProSo02 = subset(CV_clustering, select = -c(coded_country, rankOrdLife_6, c19ProSo01, c19ProSo03, c19ProSo04))
Clustered_c19ProSo02_fitted = lm(c19ProSo02 ~ ., data = CVC_c19ProSo02)
summary(Clustered_c19ProSo02_fitted)

CVC_c19ProSo03 = subset(CV_clustering, select = -c(coded_country, c19ProSo01, c19ProSo02, c19ProSo04, rankOrdLife_6))
Clustered_c19ProSo03_fitted = lm(c19ProSo03 ~ ., data = CVC_c19ProSo03)
summary(Clustered_c19ProSo03_fitted)

CVC_c19ProSo04 = subset(CV_clustering, select = -c(coded_country, rankOrdLife_6, c19ProSo01, c19ProSo02, c19ProSo03))
Clustered_c19ProSo04_fitted = lm(c19ProSo04 ~ ., data = CVC_c19ProSo04)
summary(Clustered_c19ProSo04_fitted)

# Save model metrics and create inverse p-value graphs
save_model_metrics("Clustered_Models.csv", list(Clustered_c19ProSo01_fitted, Clustered_c19ProSo02_fitted, Clustered_c19ProSo03_fitted, Clustered_c19ProSo04_fitted))
graph_inverse_p_values(Clustered_c19ProSo01_fitted)
graph_inverse_p_values(Clustered_c19ProSo02_fitted)
graph_inverse_p_values(Clustered_c19ProSo03_fitted)
graph_inverse_p_values(Clustered_c19ProSo04_fitted)

#Create comparison inverse p-value graphs between 'Other', "russia" and "Clustered"
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo01_fitted, Other = Other_c19ProSo01_fitted, Cluster = Clustered_c19ProSo01_fitted), "c19ProSo01")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo02_fitted, Other = Other_c19ProSo02_fitted, Cluster = Clustered_c19ProSo02_fitted), "c19ProSo02")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo03_fitted, Other = Other_c19ProSo03_fitted, Cluster = Clustered_c19ProSo03_fitted), "c19ProSo03")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo04_fitted, Other = Other_c19ProSo04_fitted, Cluster = Clustered_c19ProSo04_fitted), "c19ProSo04")

#Create comparison inverse p-value graphs between "russia" and "Clustered" data
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo01_fitted, Cluster = Clustered_c19ProSo01_fitted), "c19ProSo01")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo02_fitted, Cluster = Clustered_c19ProSo02_fitted), "c19ProSo02")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo03_fitted, Cluster = Clustered_c19ProSo03_fitted), "c19ProSo03")
sidebyside_inverse_p_values(list(Russia = Russia_c19ProSo04_fitted, Cluster = Clustered_c19ProSo04_fitted), "c19ProSo04")

#Find teh top 3 strongest prredictorrs for "Clustred" data
print_top_3_predictors <- function(model) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract the p-values
  p_values <- summary_model$coefficients[, "Pr(>|t|)"]
  
  # Exclude the intercept
  p_values <- p_values[-1]
  
  # Sort the p-values in ascending order
  sorted_p_values <- sort(p_values)
  
  # Get the names of the top 3 predictors with the lowest p-values
  top_3_predictors <- names(sorted_p_values)[1:3]
  
  # Get the corresponding p-values
  top_3_p_values <- sorted_p_values[1:3]
  
  # Create a string with predictor names and their p-values
  predictors_and_pvalues <- paste0(top_3_predictors, " (with a p-value of ", signif(top_3_p_values, 3), ")")
  
  # Print the string
  cat(paste(predictors_and_pvalues, collapse = ", "), "\n")
}

print_top_3_predictors(Clustered_c19ProSo01_fitted)
print_top_3_predictors(Clustered_c19ProSo02_fitted)
print_top_3_predictors(Clustered_c19ProSo03_fitted)
print_top_3_predictors(Clustered_c19ProSo04_fitted)

