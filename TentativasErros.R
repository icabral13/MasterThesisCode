install.packages("robustbase")
library(MASS)

##RobustOLS - NOT SOLVED 
independent_vars <- c("BGe1","BGe10","BIt1","BIt10","BSp1","BSp10","BFr1","BFr10","BHn1","BHn10","SpBGe1","SpBGe10","SpBIt1","SpBIt10","SpBSp1","SpBSp10","SpBFr1","SpBFr10","SpBHn1","SpBHn10")

for (i in 1:10) {
  data_subset <- data.frame(FData[, i+1], lag(FData[, 22:26], 1))
  data_subset <- na.omit(data_subset)
  col_name <- colnames(data_subset)[1]  
  formula <- as.formula(paste(col_name, "~ ."))
  
  # Perform robust regression using lmRob
  ISOLS <- rlm(formula, data = data_subset)
  
  print(summary(ISOLS))
  
  summary_df <- tidy(ISOLS)
  summary_df <- summary_df %>% mutate_if(is.numeric, round, digits = 4) 
  
  residuals <- residuals(ISOLS, type = "pearson")
  
  plot(residuals)
  plot(diff(residuals))
  acf(diff(residuals))
  
  print(Box.test(residuals, type = "Ljung-Box")) #autocorre
  print(bptest(ISOLS)) #heteroske
  print(shapiro.test(residuals)) #normality
  
  hist(residuals)
  adf.test(residuals) #stationarity
}
  


#removing OUTLIERS - NOT SOLVED 
library(outliers)
RFData <- data.frame(matrix(NA, nrow = nrow(FData), ncol = 25))

for (i in 1:25) {
  outliers <- grubbs.test(FData[,i+1])$outliers
  print(outliers)
  stats <- boxplot.stats(FData[,i+1])
  lower_fence <- stats$stats[1] - 1.5 * IQR(na.omit(FData[,i+1]))
  upper_fence <- stats$stats[5] + 1.5 * IQR(na.omit(FData[,i+1]))
  print(FData[FData[,i+1] <= lower_fence,i+1])
  print(FData[FData[,i+1] >= upper_fence,i+1])
  RFData[, i+1] <- ifelse(FData[, i+1] >= lower_fence & FData[, i+1] <= upper_fence, FData[, i+1], NA)
}

independent_vars <- c("BGe1","BGe10","BIt1","BIt10","BSp1","BSp10","BFr1","BFr10","BHn1","BHn10","SpBGe1","SpBGe10","SpBIt1","SpBIt10","SpBSp1","SpBSp10","SpBFr1","SpBFr10","SpBHn1","SpBHn10")
for (i in 1:10) {
  data_subset <- data.frame(RFData[, i+1], lag(RFData[, 22:26], 0))
  data_subset <- na.omit(data_subset)
  trainControl <- trainControl(method = "cv", number = 10)
  col_name <- colnames(data_subset)[1]  
  formula <- as.formula(paste(col_name, "~ ."))
  ISOLS <- train(formula, data = data_subset, method = "lm", trControl = trainControl)
  print(summary(ISOLS))
  summary_df <- tidy(ISOLS$finalModel)
  summary_df <- summary_df %>% mutate_if(is.numeric, round, digits = 4) 
  
  residuals <- residuals(ISOLS, type = "pearson")
  
  plot(residuals)
  plot(diff(residuals))
  acf(diff(residuals))
  
  print(Box.test(residuals, type = "Ljung-Box")) #autocorre
  print(shapiro.test(residuals)) #normality
  
  hist(residuals)
  adf.test(residuals) #stationarity
  
}


## INTERACTIONS OLS
FData2 <- FData
for (i in 1:5) {
  FData2[,26+i] <- FData2[,21+i] * FData2[,21+4]
  colnames(FData2)[26+i] <- paste(colnames(FData2)[21+i], "*", colnames(FData2)[21+4])
}

##OLS
independent_vars <- c("BGe1","BGe10","BIt1","BIt10","BSp1","BSp10","BFr1","BFr10","BHn1","BHn10","SpBGe1","SpBGe10","SpBIt1","SpBIt10","SpBSp1","SpBSp10","SpBFr1","SpBFr10","SpBHn1","SpBHn10")
final_table <- data.frame(Independent_Variable = character(),
                          Coefficient = numeric(),
                          P_Value = numeric(),
                          stringsAsFactors = FALSE)
residuals_table <- data.frame(Independent_Variable = character(),
                              Autocorrelation_Test_Statistic = numeric(),
                              Autocorrelation_Test_PValue = numeric(),
                              Normality_Test_Statistic = numeric(),
                              Normality_Test_PValue= numeric(),
                              Homoskedasticity_Test_Statistic= numeric(),
                              Homoskedasticity_Test_PValue = numeric(),
                              stringsAsFactors = FALSE)
for (i in 1:20) {
  data_subset <- data.frame(FData2[, i+1], lag(FData2[, 22:31], 0))
  data_subset <- na.omit(data_subset)
  trainControl <- trainControl(method = "cv", number = 10)
  col_name <- colnames(data_subset)[1]  
  formula <- as.formula(paste(col_name, "~ ."))
  ISOLS <- train(formula, data = data_subset, method = "lm", trControl = trainControl)
  print(summary(ISOLS))
  summary_df <- tidy(ISOLS$finalModel)
  summary_df <- summary_df %>% mutate_if(is.numeric, round, digits = 4) 
  
  residuals <- residuals(ISOLS)
  
  plot(residuals)
  plot(diff(residuals))
  acf(diff(residuals))
  
  print(Box.test(residuals, lag = 10, type = "Ljung-Box"))
  print(bptest(ISOLS$finalModel))
  print(shapiro.test(residuals))
  
  autocorr <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  homskedas <- bptest(ISOLS$finalModel)
  normality <- shapiro.test(residuals)
  
  var_coefficient <- summary_df$estimate[summary_df$term == "TTF"]
  var_pvalue <- summary_df$p.value[summary_df$term == "TTF"]
  
  final_table <- rbind(final_table, data.frame(Independent_Variable = independent_vars[i],
                                               Coefficient = var_coefficient,
                                               P_Value = var_pvalue,
                                               stringsAsFactors = FALSE))
  
  residuals_table <- rbind(residuals_table, data.frame(Independent_Variable = independent_vars[i],
                                                       Autocorrelation_Test_Statistic = autocorr$statistic,
                                                       Autocorrelation_Test_PValue = autocorr$p.value,
                                                       Homoskedasticity_Test_Statistic = homskedas$statistic,
                                                       Homoskedasticity_Test_PValue= homskedas$p.value,
                                                       Normality_Test_Statistic = normality$statistic,
                                                       Normality_Test_PValue = normality$p.value,
                                                       stringsAsFactors = FALSE))
  
  
  latex_table <- xtable(summary_df, digits = 4)
  
  caption <- paste("Summary table for", independent_vars[i])
  caption <- paste("\\caption{", caption, "}", sep = "")
  attr(latex_table, "caption") <- caption
  cat(paste("Summary table for loop", i, ":\n"))
  #print(latex_table)
  cat("\n")
  
}

latex_final_table <- xtable(final_table, digits = 4)
caption_final_table <- "\\caption{Final table with Coefficient and P-Value for the desired variable}"
attr(latex_final_table, "caption") <- caption_final_table
print(latex_final_table)

latex_residuals_table <- xtable(residuals_table, digits = 4)
caption_residuals_table <- "\\caption{Residual tests results}"
attr(latex_residuals_table, "caption") <- caption_residuals_table
print(latex_residuals_table)



