library(corrplot)
install.packages("corrplot")
install.packages("e1071")
install.packages("knitr")
library(knitr)
library(e1071)
library(quantmod)
library(ggplot2)
library(lubridate)
library(tseries)
library(ggplot2)
library(xts)
library(forecast)



#import data
D_DATA <- read_delim("~/Desktop/D_DATA.csv", 
                     delim = ";", escape_double = FALSE, na = "#N/D", 
                     trim_ws = TRUE)

#INTERPOLATE DATA 
variables <- as.list(D_DATA[, -1])

for (i in seq_along(variables)) {
  variable <- variables[[i]]
  # Perform spline interpolation for missing values
  variables[[i]] <- na.spline(variable)
}

# Print the interpolated variables
interpolated_data <- data.frame(D_DATA$Data)
for (i in seq_along(variables)) {
  interpolated_data[i+1] <- variables[[i]]
}
print(interpolated_data)

names<-list("Date","VIX","POL","F2S","TTF","OIL","OIS1","OIS10","BGe1","BGe10","BIt1","BIt10","BSp1","BSp10","BFr1","BFr10","BHn1","BHn10")
colnames(interpolated_data) <- names



# Plotting loop
for (i in 2:6) {  # Assuming columns 2 to 6 contain the data to be plotted
  y_min <- min(interpolated_data[, i], na.rm = TRUE)  # Adjust the column range as per your data
  y_max <- max(interpolated_data[, i], na.rm = TRUE)  # Adjust the column range as per your data
  
  plot(as.Date(interpolated_data$Date, format = "%d/%m/%y"), interpolated_data[[i]], main = names[i],
       col = "#03cffc", type = "l", xlab = "Date", ylab = "Value", ylim = c(y_min, y_max))
}


#DATASTATS
result_table <- data.frame()

for (i in 2:length(interpolated_data)) {
  # Calculate statistics for the column
  col_summary <- summary(interpolated_data[[i]])
  col_mean <- round(col_summary[["Mean"]], 3)
  col_median <- round(col_summary[["Median"]], 3)
  col_min <- round(col_summary[["Min."]], 3)
  col_max <- round(col_summary[["Max."]], 3)
  col_skew <- round(skewness(interpolated_data[[i]],type = 2), 3)
  col_kurt <- round(kurtosis(interpolated_data[[i]], type = 2), 3)
  col_SD <- round(sd(interpolated_data[[i]]), 3)
  
  # Create a data frame with the statistics
  col_stats <- data.frame(
    Variable = colnames(interpolated_data)[i],
    Mean = col_mean,
    Median = col_median,
    S.Deviation = col_SD,
    Minimum = col_min,
    Maximum = col_max,
    Skewness = col_skew,
    Kurtosis = col_kurt
  )
  
  # Append the statistics to the result table
  result_table <- rbind(result_table, col_stats)
}

kable(result_table, format = "latex", caption = "Statistics Table")




#Stationary Tests Explanatory Vars: 
for (i in 2:6) {
  print(colnames(interpolated_data)[i])
  print(PP.test(interpolated_data[[i]]))
  print(adf.test(interpolated_data[[i]]))
  print(kpss.test(interpolated_data[[i]]))
}

ExVars <- data.frame(interpolated_data[1:6])
ExVars[[2]] <- c(NA,diff(interpolated_data[[2]])) 
ExVars[[3]] <- c(NA,diff(log(interpolated_data[[3]]))) 
ExVars[[4]] <- c(NA,diff(log(interpolated_data[[4]]))) 
ExVars[[5]] <- c(NA,diff(log(interpolated_data[[5]])))
ExVars[[6]] <- c(NA,diff(log(interpolated_data[[6]])))

#Stationary tests Ex. Vars
variable <- character()
PP_statistic <- numeric()
PP_p_value <- numeric()
adf_statistic <- numeric()
adf_p_value <- numeric()
kpss_statistic <- numeric()
kpss_p_value <- numeric()

# Perform tests and store the results
for (i in 2:6) {
  column_name <- colnames(ExVars)[i]
  column_values <- ExVars[[i]][-1]
  missing_values <- is.na(column_values) | is.nan(column_values) | is.infinite(column_values)
  
  
  # Perform the tests and store the results in vectors
  PP_result <- PP.test(column_values[!missing_values])
  adf_result <- adf.test(column_values[!missing_values])
  kpss_result <- kpss.test(column_values[!missing_values])
  
  variable <- c(variable, column_name)
  PP_statistic <- c(PP_statistic, PP_result$statistic)
  PP_p_value <- c(PP_p_value, PP_result$p.value)
  adf_statistic <- c(adf_statistic, adf_result$statistic)
  adf_p_value <- c(adf_p_value, adf_result$p.value)
  kpss_statistic <- c(kpss_statistic, kpss_result$statistic)
  kpss_p_value <- c(kpss_p_value, kpss_result$p.value)
}

# Create a data frame from the results
results_exvars <- data.frame(
  Variable = variable,
  PP_Statistic = round(PP_statistic,2),
  PP_P_Value = round(PP_p_value,2),
  ADF_Statistic = round(adf_statistic,2),
  ADF_P_Value = round(adf_p_value,2),
  KPSS_Statistic = round(kpss_statistic,2),
  KPSS_P_Value = round(kpss_p_value,2)
)

# Print the table
kable(results_exvars, format = "latex", caption = "Statinary tests Table")




#Prep IND VARS 
##Difference Yields 
VARS <- data.frame(interpolated_data[,c(1,9:18)])

for (i in 1:10) {
  VARS[[i+1]] <- c(NA, diff(VARS[[i+1]]))
}

##Spreads
Spreads <- data.frame(D_DATA$Data)

for (i in 1:5) {
  Spreads[2*i]<- interpolated_data[[2*i+7]]-interpolated_data[[7]]
  Spreads[2*i+1]<- interpolated_data[[2*i+8]]-interpolated_data[[8]]
}

name <-list("Date","SpBGe1","SpBGe10","SpBIt1","SpBIt10","SpBSp1","SpBSp10","SpBFr1","SpBFr10","SpBHn1","SpBHn10")
colnames(Spreads) <- name

IndVars <- cbind(VARS, Spreads[,-1])


#Stationary tests Ind. Vars
variable <- character()
PP_statistic <- numeric()
PP_p_value <- numeric()
adf_statistic <- numeric()
adf_p_value <- numeric()
kpss_statistic <- numeric()
kpss_p_value <- numeric()

# Perform tests and store the results
for (i in 2:21) {
  column_name <- colnames(IndVars)[i]
  column_values <- IndVars[[i]][-1]
  
  # Perform the tests and store the results in vectors
  PP_result <- PP.test(column_values)
  adf_result <- adf.test(column_values)
  kpss_result <- kpss.test(column_values)
  
  variable <- c(variable, column_name)
  PP_statistic <- c(PP_statistic, PP_result$statistic)
  PP_p_value <- c(PP_p_value, PP_result$p.value)
  adf_statistic <- c(adf_statistic, adf_result$statistic)
  adf_p_value <- c(adf_p_value, adf_result$p.value)
  kpss_statistic <- c(kpss_statistic, kpss_result$statistic)
  kpss_p_value <- c(kpss_p_value, kpss_result$p.value)
}

# Create a data frame from the results
results_ind <- data.frame(
  Variable = variable,
  PP_Statistic = round(PP_statistic,2),
  PP_P_Value = round(PP_p_value,2),
  ADF_Statistic = round(adf_statistic,2),
  ADF_P_Value = round(adf_p_value,2),
  KPSS_Statistic = round(kpss_statistic,2),
  KPSS_P_Value = round(kpss_p_value,2)
)

# Print the table
kable(results_ind, format = "latex", caption = "Statinary tests Table")



#FINAL DATA 
FData <- cbind(IndVars, ExVars[,-1])

#correlation plot
library(reshape2)

cor_matrix <- cor(FData[, -1])

# Convert the correlation matrix to a data frame
cor_df <- melt(cor_matrix)

# Create the correlation matrix plot using ggplot2
ggplot(data = cor_df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = "Variables",
       y = "Variables")

##PLOTS 
#Plots Ind vars
Date <- as.Date(IndVars$Date, format = "%d/%m/%y")
plots <- list("Change Daily Yields Germany","Change Daily Yields Italy","Change Daily Yields Spain", "Change Daily Yields France", "Change Daily Yields Hungary", "Spread Daily Yields Germany","Spread Daily Yields Italy","Spread Daily Yields Spain","Spread Daily Yields France","Spread Daily Yields Hungary")
for (i in 1:5) {
  if (i == 2){
    chart <- data.frame(IndVars[2*i], IndVars[2*i+1])
    chart <- xts(chart, order.by = Date)
    options(repr.plot.width = 6, repr.plot.height = 4)
    graph <- plot(chart, col = c("orange", "red"), main = plots[i], xlab = "Date", ylab = "Values", ylim = c(-0.8, 0.8))
    print(graph)
  }else {
    chart <- data.frame(IndVars[2*i], IndVars[2*i+1])
    chart <- xts(chart, order.by = Date)
    options(repr.plot.width = 6, repr.plot.height = 4)
    graph <- plot(chart, col = c("orange", "red"), main = plots[i], xlab = "Date", ylab = "Values")
    print(graph)
  }
}
for (i in 6:10) {
  chart <- data.frame(IndVars[2*i], IndVars[2*i+1])
  chart <- xts(chart, order.by = Date)
  options(repr.plot.width = 6, repr.plot.height = 4)
  graph <- plot(chart, col = c("orange", "red"), main = plots[i], xlab = "Date", ylab = "Values")
  print(graph)
}

plot(IndVars$BGe1, ExVars$F2S)
#Plots Exp vars
for (i in 1:5) {
  missing_values <- is.na(ExVars[[i+1]]) | is.nan(ExVars[[i+1]]) | is.infinite(ExVars[[i+1]])
  chart <- ExVars[[i+1]][!missing_values]
  chart <- data.frame(Date = Date[!missing_values], Values = chart)
  chart_xts <- xts(chart$Values, order.by = chart$Date)
  options(repr.plot.width = 6, repr.plot.height = 4)
  graph <- plot(chart_xts, col = c("#03cffc"), main = names(ExVars)[i+1], xlab = "Date", ylab = "Values")
  print(graph)
}

#Covid Plots 
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2021-12-31")
IndVars$Date <- as.Date(IndVars$Date, format = "%d/%m/%y")

filtered_data <- IndVars[IndVars$Date >= start_date & IndVars$Date <= end_date, 1:11]
Names <- list("Covid Analysis Germany","Covid Analysis Italy","Covid Analysis Spain", "Covid Analysis France","Covid Analysis Hungary")
for (i in 1:5) {
  if (i==2){
    chart <- ggplot(filtered_data, aes(x = Date)) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i+1]), color = "red", alpha = 0.7) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i]), color = "orange", alpha = 1) +
      labs(title = Names[i], x = "Date", y = "Value") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_blank())+
      ylim(-0.65, 0.65)
    chart <- chart + 
      geom_vline(xintercept = as.numeric(as.Date("2020-02-21")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2021-03-30")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2021-04-09")), linetype = "dashed", color = "blue")
    print(chart)
  }else{
    chart <- ggplot(filtered_data, aes(x = Date)) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i+1]), color = "red", alpha = 0.7) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i]), color = "orange", alpha = 1) +
      labs(title = Names[i], x = "Date", y = "Value") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_blank())
    
    chart <- chart + 
      geom_vline(xintercept = as.numeric(as.Date("2020-02-21")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2020-12-21")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2021-03-30")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2021-04-09")), linetype = "dashed", color = "blue")
    print(chart)
  }
}

#Russian War Plots
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2023-06-01")

filtered_data <- IndVars[IndVars$Date >= start_date & IndVars$Date <= end_date, 1:11]
Names <- list("Ukraine War Analysis Germany","Ukraine War Analysis Italy","Ukraine War Analysis Spain", "Ukraine War Analysis France","Ukraine War Analysis Hungary")

for (i in 1:5) {
  if (i==2){
    chart <- ggplot(filtered_data, aes(x = Date)) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i+1]), color = "red", alpha = 0.7) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i]), color = "orange", alpha = 1) +
      labs(title = Names[i], x = "Date", y = "Value") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_blank())+
      ylim(-0.65, 0.65)
    chart <- chart + 
      geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-04-08")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-06-03")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-06-23")), linetype = "dashed", color = "green") +
      geom_vline(xintercept = as.numeric(as.Date("2022-07-05")), linetype = "dashed", color = "purple") +
      geom_vline(xintercept = as.numeric(as.Date("2022-08-29")), linetype = "dashed", color = "green") +
      geom_vline(xintercept = as.numeric(as.Date("2022-10-01")), linetype = "dashed", color = "purple") +
      geom_vline(xintercept = as.numeric(as.Date("2023-03-15")), linetype = "dashed", color = "blue") 
    print(chart)
  }else{
    chart <- ggplot(filtered_data, aes(x = Date)) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i+1]), color = "red", alpha = 0.7) +
      geom_line(aes_string(y = colnames(filtered_data)[2*i]), color = "orange", alpha = 1) +
      labs(title = Names[i], x = "Date", y = "Value") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray"),
            panel.grid.minor = element_blank())
    
    chart <- chart + 
      geom_vline(xintercept = as.numeric(as.Date("2022-02-24")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-04-08")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-06-03")), linetype = "dashed", color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date("2022-06-23")), linetype = "dashed", color = "green") +
      geom_vline(xintercept = as.numeric(as.Date("2022-07-05")), linetype = "dashed", color = "purple") +
      geom_vline(xintercept = as.numeric(as.Date("2022-08-29")), linetype = "dashed", color = "green") +
      geom_vline(xintercept = as.numeric(as.Date("2022-10-01")), linetype = "dashed", color = "purple") +
      geom_vline(xintercept = as.numeric(as.Date("2023-03-15")), linetype = "dashed", color = "blue") 
    print(chart)
  }
}



