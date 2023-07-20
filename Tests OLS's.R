install.packages("caret")
library(caret)
library(broom)
library(stargazer)
library(xtable)
library(ShapleyValue)
library(lmtest)
library(tidyverse)
library(glmnet)


#Data tests
#In Sample
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
f_table <- data.frame(Independent_Variable = character(),
                      F_Statistic = numeric(),
                      F_PValue = numeric(),
                      R_Squared = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:20) {
  data_subset <- data.frame(FData[, i+1], lag(FData[, 22:26], 0))
  data_subset <- na.omit(data_subset)
  trainControl <- trainControl(method = "cv", number = 10)
  col_name <- colnames(data_subset)[1]  
  formula <- as.formula(paste(col_name, "~ ."))
  ISOLS <- train(formula, data = data_subset, method = "lm", trControl = trainControl)
  print(summary(ISOLS))
  summary_df <- tidy(ISOLS$finalModel)
  summary_df <- summary_df %>% mutate_if(is.numeric, round, digits = 4) 
  
  residuals <- residuals(ISOLS$finalModel)
  #plot(residuals)
  
  #qqplots normality
  #qq <- qqnorm(residuals, main = paste("QQ Plot of Residuals", independent_vars[i]), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")      
  #qqline(residuals, col = "red", lwd = 2, lty = 2)
  
  #png(filename = paste0("QQ_Plot_", independent_vars[i], ".png"), width = 1500, height = 1500, units = "px", res = 300)
  #plot(qq$x, qq$y, main = paste("QQ Plot of Residuals", independent_vars[i]), xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
  #abline(qqline(residuals), col = "red", lwd = 2, lty = 2)
  #dev.off()
  
  # Create autocorrelation plot
  #ACF <- acf(residuals, main = paste("Autocorrelation Plot of Residuals", independent_vars[i]), xlab = "Lag", ylab = "Autocorrelation")
  # Save the autocorrelation plot as a PNG image with significance lines
  #png(filename = paste0("Autocorr_", independent_vars[i], ".png"), width = 1500, height = 1500, units = "px", res = 300)
  #plot(ACF$lag, ACF$acf, type = "h", main = paste("Autocorrelation Plot of Residuals", independent_vars[i]), xlab = "Lag", ylab = "Autocorrelation")
  #abline(h = c(-1.96/sqrt(length(residuals))), col = "blue", lty = 2)
  #abline(h = c(1.96/sqrt(length(residuals))), col = "blue", lty = 2)
  #dev.off()
  
  
  #png(filename = paste0("Residuals_vs_Fitted_", independent_vars[i], ".png"), width = 1700, height = 1500, units = "px", res = 300)
  #plot(fitted(ISOLS), residuals, main = paste("Scatterplot of Residuals vs. Fitted Values", independent_vars[i]),
   #    xlab = "Fitted Values", ylab = "Residuals")
  #dev.off()
  

  Box.test(residuals, lag = 10, type = "Ljung-Box")
  bptest(ISOLS$finalModel)
  shapiro.test(residuals)
  
  autocorr <- Box.test(residuals, lag = 10, type = "Ljung-Box")
  homskedas <- bptest(ISOLS$finalModel)
  normality <- shapiro.test(residuals)
  
  var_coefficient <- summary_df$estimate[summary_df$term == "POL"]
  var_pvalue <- summary_df$p.value[summary_df$term == "POL"]
  
  f_statistic <- summary(ISOLS)$fstatistic[1]
  f_pvalue <- pf(f_statistic, summary(ISOLS)$fstatistic[2], summary(ISOLS)$fstatistic[3], lower.tail = FALSE)
  r_squared <- summary(ISOLS)$r.squared
  
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

  f_table <- rbind(f_table, data.frame(Independent_Variable = independent_vars[i],
                                       F_Statistic = f_statistic,
                                       F_PValue = f_pvalue,
                                       R_Squared = r_squared,
                                       stringsAsFactors = FALSE))
  
  latex_table <- xtable(summary_df, digits = 4)
  
  caption <- paste("Summary table for", independent_vars[i])
  caption <- paste("\\caption{", caption, "}", sep = "")
  attr(latex_table, "caption") <- caption
  cat(paste("Summary table for loop", i, ":\n"))
  #print(latex_table)
  cat("\n")
  
}

latex_f_table <- xtable(f_table, digits = 4)
caption_f_table <- "\\caption{Table with F-statistic, p-value, and R-squared}"
attr(latex_f_table, "caption") <- caption_f_table
print(latex_f_table)

latex_final_table <- xtable(final_table, digits = 4)
caption_final_table <- "\\caption{Final table with Coefficient and P-Value for the desired variable}"
attr(latex_final_table, "caption") <- caption_final_table
print(latex_final_table)

latex_residuals_table <- xtable(residuals_table, digits = 4)
caption_residuals_table <- "\\caption{Residual tests results}"
attr(latex_residuals_table, "caption") <- caption_residuals_table
print(latex_residuals_table)



##LASSO
# Initialize the required packages
library(glmnet)

# Set the lambda values
lambda <- 10^seq(-11, 1, length = 100)

# Iterate over the variables
for (i in 1:20) {
  # Create empty vectors to store coefficients
  L <- c()
  Int <- c()
  vix <- c()
  pol <- c()
  f2s <- c()
  ttf <- c()
  oil <- c()
  
  # Calculate the Lasso model using cv.glmnet
  x <- as.matrix(FData[-1, 22:26])
  y <- as.matrix(FData[-1, i+1])
  n <- nrow(x)
  p <- ncol(x)
  lasso_model <- cv.glmnet(x, y, alpha = 1, n = n, p = p)
  
  # Iterate over the lambda values
  ttf_zero_lambda <- NULL
  for (lamb in lambda) {
    x <- as.matrix(FData[-1, 22:25])
    y <- as.matrix(FData[-1, i+1])
    n <- nrow(x)
    p <- ncol(x)
    model <- glmnet(x, y, alpha = 1, lambda = lamb)
    
    L <- c(L, lamb)
    Int <- c(Int, coef(model)[1])
    vix <- c(vix, coef(model)[2])
    pol <- c(pol, coef(model)[3])
    f2s <- c(f2s, coef(model)[4])
    ttf <- c(ttf, coef(model)[5])
    oil <- c(oil, coef(model)[6])

    
    # Check if ttf is equal to 0 for the first time
    if (is.null(ttf_zero_lambda) && coef(model)[5] == 0) {
      ttf_zero_lambda <- lamb
    }
  }
  
  # Plotting
  #if (i <=10)  {
   # y_min <- -0.4
    #y_max <- 0.2
  #} else {
   # y_min <- -2
    #y_max <- 2
  #}
  # Create the plot with adjusted y-axis limits
  #plot(log(L), Int, type = "l", col = "red", xlab = "log(Lambda)", ylab = "Coefficient Values", main = independent_vars[i], ylim = c(y_min, y_max))
  #lines(log(L), vix, type = "l", col = "blue")
  #lines(log(L), pol, type = "l", col = "green")
  #lines(log(L), f2s, type = "l", col = "orange")
  #lines(log(L), ttf, type = "l", col = "purple")
  #lines(log(L), oil, type = "l", col = "brown")
  
  
  #abline(v = log(lasso_model$lambda.min), col = "black", lty = 2)
  
  # Add legend
  #legend("bottomleft", legend = c("Intersept", "VIX", "POL", "F2S", "TTF", "OIL"), col = c("red", "blue", "green", "orange", "purple", "brown"), lty = 1)
  
  if (!is.null(ttf_zero_lambda)) {
    print(paste("First lambda with ttf=0 for", independent_vars[i], ":", ttf_zero_lambda))
  } else {
    print(paste("No lambda found with ttf=0 for", independent_vars[i]))
  }
}



#SHAPLEY
for (i in 1:20) {
  col_name <- names(FData)[i+1]
  Shap <- shapleyvalue(FData[[col_name]],FData[22:26])
  categories <- c("VIX", "POL", "F2S", "TTF", "OIL")
  values1 <- c(Shap$VIX[1], Shap$POL[1], Shap$F2S[1], Shap$TTF[1], Shap$OIL[1])
  values2 <- c(Shap$VIX[2], Shap$POL[2], Shap$F2S[2], Shap$TTF[2], Shap$OIL[2])
  bar_colors <- c("#FFEF00", "#43C6DB", "#728FCE", "red", "brown")
  plot_values <- barplot(values1, names.arg = categories, horiz = TRUE, xlab = "Value", ylab = "Category", main =  paste("Shapley Values -", independent_vars[i]), xlim = c(0, max(values1)), col = bar_colors)
  text(values1, plot_values, labels = values1, pos = 4, offset = 0.5)
  plot_values <- barplot(values2, names.arg = categories, horiz = TRUE, xlab = "Value", ylab = "Category", main = paste("Shapley Values -", independent_vars[i]), xlim = c(0, max(values2)), col = bar_colors)
  text(values2, plot_values, labels = values2, pos = 4, offset = 0.5)
}


library(DALEX)
library(ggplot2)
library(ggforce)
install.packages("iml")
library(iml)
shapley_values <- list()

for (i in 1:20) {
  col_name <- names(FData)[i+1]
  Shap <- shapleyvalue(FData[[col_name]], FData[22:26])
  shapley_values[[col_name]] <- Shap
}


categories <- c("VIX", "POL", "F2S", "TTF","OIL")

for (i in 1:20) {
  col_name <- names(FData)[i+1]
  Shap <- shapley_values[[col_name]]
  
  values2 <- c(Shap$VIX[2], Shap$POL[2], Shap$F2S[2], Shap$TTF[2], Shap$OIL[2])
  
  # Create a data frame with Group 2 Shapley values
  data <- data.frame(Category = categories,
                     Value = values2)
  
  # Create a force plot
  force_plot <- ggplot(data, aes(x = Category, y = Value, label = round(Value, 2))) +
    geom_segment(aes(xend = Category, yend = 0), linetype = "dashed", color = "gray50") +
    geom_point(size = 4) +
    geom_label(aes(y = 0), color = "blue", fontface = "bold") +
    coord_flip() +
    theme_minimal()
  
  # Save the force plot as a PNG file
  ggsave(filename = paste0("Force_Plot_", col_name, "_Group2.png"), plot = force_plot, dpi = 300)
}


#Out-of Sample 
FData$Date <- as.Date(FData$Date, format = "%d/%m/%y")

start_date1 <- as.Date("2018-01-03")
end_date1 <- as.Date("2020-01-01")
start_date2 <- as.Date("2020-01-02")
end_date2 <- as.Date("2022-01-01")
start_date3 <- as.Date("2022-01-02")
end_date3 <- as.Date("2023-06-01")

set1 <- FData[FData$Date >= start_date1 & FData$Date <= end_date1, 1:26]
set2 <- FData[FData$Date >= start_date2 & FData$Date <= end_date2, 1:26]
set3 <- FData[FData$Date >= start_date3 & FData$Date <= end_date3, 1:26]

Data_list <- list(set1,set2,set3)

##LASSO
lambda <-  10^seq(-11, 1, length = 100)
for (i in 1:20) {
  L <- c()
  ttf1 <- c()
  ttf2 <- c()
  ttf3 <- c()
  for (lamb in lambda){
    L <- c(L,lamb)
    for (j in 1:3) {
      x <- as.matrix((Data_list[[j]][-1, 22:26]))
      y <- as.matrix((Data_list[[j]][-1,i+1]))
      n <- nrow(x)
      p <- ncol(x)
      model <- glmnet(x,y, alpha = 1, lambda = lamb)
      if (j==1){
        ttf1 <- c(ttf1,coef(model)[5])
      }else if (j==2){
        ttf2 <- c(ttf2,coef(model)[5])
      }else{
        ttf3 <- c(ttf3,coef(model)[5])
      }
    }
  } 
  if (i<11){
    y_min <- -0.5
    y_max <- 0.2
  }else{
    y_min <- -1
    y_max <- 1
  }
  
  # Create the plot with adjusted y-axis limits
  plot(log(L), ttf1, type = "l", col = "lightblue", xlab = "log(Lambda)", ylab = "Coefficient Values", main = independent_vars[i], ylim = c(y_min, y_max))
  lines(log(L), ttf2, type = "l", col = "blue")
  lines(log(L), ttf3, type = "l", col = "darkblue")
  
  # Add legend
  legend("bottomleft", legend = c("Before Covid", "Covid", "Ukrainian War"), col = c("lightblue", "blue", "darkblue"), lty = 1)
}
  



#SHAPLEY
for (i in 1:20) {
  values1 <- c()
  values2 <- c()
  for (j in 1:3) {
    col_name <- names(Data_list[[j]])[i+1]
    Shap <- shapleyvalue(Data_list[[j]][[col_name]],Data_list[[j]][22:26])
    values1[j] <- c(Shap$TTF[1])
    values2[j] <- c(Shap$TTF[2])
  }
  categories <- c("Before Covid", "Covid", "Ukrainian War")
  bar_colors <- c("lightblue", "blue", "darkblue")
  par(mar = c(5, 4, 4, 2) + 0.1)
  
  plot_values <- barplot(values1, names.arg = categories, horiz = TRUE, xlab = "Value", ylab = "Category", main =  paste("Shapley Values -", independent_vars[i]), xlim = c(0, max(values1)), col = bar_colors, width = 1)
  text(values1, plot_values, labels = values1, pos = 4, offset = 0.5)
  plot_values <- barplot(values2, names.arg = categories, horiz = TRUE, xlab = "Value", ylab = "Category", main = paste("Shapley Values -", independent_vars[i]), xlim = c(0, max(values2)), col = bar_colors, width = 1)
  text(values2, plot_values, labels = values2, pos = 4, offset = 0.5)
}

FData$Date <- as.Date(FData$Date, format = "%d/%m/%y")

start_date1 <- as.Date("2018-01-03")
end_date1 <- as.Date("2020-01-01")
start_date2 <- as.Date("2020-01-02")
end_date2 <- as.Date("2022-01-01")
start_date3 <- as.Date("2022-01-02")
end_date3 <- as.Date("2023-06-01")

set1 <- FData[FData$Date >= start_date1 & FData$Date <= end_date1, 1:26]
set2 <- FData[FData$Date >= start_date2 & FData$Date <= end_date2, 1:26]
set3 <- FData[FData$Date >= start_date3 & FData$Date <= end_date3, 1:26]

Data_list <- list(set1, set2, set3)

shapley_values <- list()

for (i in 1:20) {
  values1 <- c()
  values2 <- c()
  
  for (j in 1:3) {
    col_name <- names(Data_list[[j]])[i+1]
    Shap <- shapleyvalue(Data_list[[j]][[col_name]], Data_list[[j]][22:26])
    values1[j] <- Shap$TTF[1]
    values2[j] <- Shap$TTF[2]
  }
  
  categories <- c("Before Covid", "Covid", "Ukrainian War")
  
  # Create a data frame with Shapley values
  data <- data.frame(Category = categories,
                     Group1 = values1,
                     Group2 = values2)
  
  # Convert the data frame to long format
  data_long <- tidyr::gather(data, key = "Group", value = "Value", -Category)
  
  # Create a force plot
  force_plot <- ggplot(data_long, aes(x = Category, y = Value, group = Category, color = Group, label = round(Value, 2))) +
    geom_segment(aes(xend = Category, yend = 0), linetype = "dashed", color = "gray50") +
    geom_point(size = 4) +
    geom_label(aes(y = 0, fill = Group), color = "white", fontface = "bold") +
    coord_flip() +
    scale_color_manual(values = c("lightblue", "blue")) +
    scale_fill_manual(values = c("lightblue", "blue")) +
    theme_minimal()
  
  # Save the force plot as a PNG file
  ggsave(filename = paste0("Force_Plot_", col_name, ".png"), plot = force_plot, dpi = 300)
}
