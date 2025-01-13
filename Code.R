# Header ============================================================
# Project: Microeconometrics Final HW
# Authors: Emil Dotchev & Gabe Batres
# Date: 3/1/2025
# Description: Recreate analysis of the paper about racial seggregation
#  by Elizabeth Ananat.
#
# ===================================================================


# Step 0: Setup environment =========================================
# Begin by loading necessary libraries and set the working directory

# Load Required Libraries
library(haven)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(dplyr)

# Import the data
data <- read_dta("ananat2011.dta")

# Step 1: Investigate data ==========================================
# Understand the raw data provided

class(data)
head(data)

# What is the dimension of the data
dim(data)


# Each row is a "Metropolitan Statistical Area" (MSA)
non_numeric_cols <- names(data)[!sapply(data, is.numeric)]
print(sapply(data[non_numeric_cols], function(x) length(unique(x))))
rm(non_numeric_cols)

# Get names of numeric columns
numeric_cols <- names(data)[sapply(data, is.numeric)]

# Create a summary table to see the basics of the data.
summary_table <- data.frame(
  Column_Name = numeric_cols,
  Number_of_Rows = nrow(data),
  Number_of_Unique_Values = sapply(data[numeric_cols], function(x) length(unique(x))),
  Min = round(sapply(data[numeric_cols], min, na.rm = TRUE), 5),
  Max = round(sapply(data[numeric_cols], max, na.rm = TRUE), 5),
  Mean = round(sapply(data[numeric_cols], mean, na.rm = TRUE), 5),
  Median = round(sapply(data[numeric_cols], median, na.rm = TRUE), 5),
  Standard_Deviation = round(sapply(data[numeric_cols], sd, na.rm = TRUE), 5)
)


# Find the columns needed to reproduce the analysis
columns_with_word <- names(data)[grepl("1920", names(data), ignore.case = TRUE)]
print(columns_with_word)

# herf = rdi
# dism1990 = 1990 segregation
# lenper = track_length (per sq km)
# passpc = street-cars per cap.


# Step 2: Scatter Plots with Linear Regression Line =================
# First, create plot for Figure 3

# Use a function to create the plot
create_scatter_plot <- function(df, x_col, y_col) {
  
  # Calculate the slope of the least squares line
  model <- lm(df[[y_col]] ~ df[[x_col]], data = df)
  slope <- coef(model)[2]
  
  # Create the scatter plot
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(color = "darkblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    geom_text(aes(label = name), size = 2, color = "gray", alpha = 0.8,
              hjust = 0.5, vjust = 1.5) +
    xlim(0.2, 1) +
    ylim(0.2, 1) +
    labs(title = paste("Scatter Plot of", x_col, "vs.", y_col),
         x = paste(x_col, " (X-axis)"), 
         y = paste(y_col, " (Y-axis)"),
         caption = "Note: This plot shows the relationship between the two variables.") +
    annotate("text", 
             x = min(df[[x_col]]), y = min(df[[y_col]]), 
             label = paste("Least Squares Line (Red) Slope =", round(slope, 3)), 
             color = "darkred", hjust = 0.1, vjust = 2) +
    theme_bw()
}

# Call the function with the columns
create_scatter_plot(data, "herf", "dism1990")

# Next, create the plot for Figure 4 --------------------------------

# Again, use a function
create_partitioned_scatter_plot <- function(df, x_col, y_col, group_col) {
  
  # Create a new column for the group (top vs bottom)
  df <- df %>%
    mutate(group = ifelse(.data[[group_col]] > median(.data[[group_col]]), "Top", "Bottom"))
  
  # Calculate linear models for each group
  model_top <- lm(df[[y_col]] ~ df[[x_col]], data = df %>% filter(group == "Top"))
  model_bottom <- lm(df[[y_col]] ~ df[[x_col]], data = df %>% filter(group == "Bottom"))
  
  # Get slopes for the labels
  slope_top <- coef(model_top)[2]
  slope_bottom <- coef(model_bottom)[2]
  
  # Create the scatter plot
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]], color = group)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, aes(fill = group), alpha = 0.15) +  # Add confidence bounds
    geom_text(aes(label = name), size = 2, color = "darkgray", alpha = 0.6,
              vjust = 1.2, show.legend = FALSE) +
    labs(title = paste("Partitioned Scatter Plot of", x_col, "vs.", y_col),
         x = paste(x_col, "(X-axis)"),
         y = paste(y_col, "(Y-axis)"),
         color = "Group", fill = "Group",
         caption = "Note: This plot shows the relationship between variables, partitioned by groups.") +
    annotate("text", 
             x = min(df[[x_col]]), y = max(df[[y_col]]), 
             label = paste("Slope (Top) =", round(slope_top, 2)), 
             color = "red3", hjust = 0, vjust = 1) +  # Top line label
    annotate("text", 
             x = min(df[[x_col]]), y = max(df[[y_col]]) - 0.1 * diff(range(data[[y_col]])), 
             label = paste("Slope (Bottom) =", round(slope_bottom, 2)), 
             color = "blue3", hjust = 0, vjust = 1) +  # Bottom line label (adjusted position)
    theme_bw() +
    scale_color_manual(values = c("red3", "blue3")) +  # Set colors for points
    scale_fill_manual(values = c("red3", "blue3"))     # Set colors for lines
}

create_partitioned_scatter_plot(data, "closeness", "dism1990", "herf")  # Replace with actual column names



# Step 3: Tables of ==================

# Table 1

data$area1910 <- data$area1910/1000
data$count1910 <- data$count1910/1000
data$passpc <- data$passpc/1000

# Dependent Variables
vars <- c("dism1990", "area1910", "count1910", "ethseg10", "ethiso10", "black1910", 
          "passpc", "black1920", "ctyliterate1920", "lfp1920", "ctytrade_wkrs1920", 
          "ctymanuf_wkrs1920", "ctyrail_wkrs1920", "incseg")

# Run regressions
models <- lapply(vars, function(var) {
  lm(as.formula(paste(var, "~ herf + lenper")), data)
})

# Compute robust standard errors
robust_ses <- lapply(models,
                     function(model) coeftest(model, 
                                              vcov = vcovHC(model, 
                                                            type = "HC1")))

# Compute means
means <- sapply(vars, function(var) mean(data[[var]], na.rm = TRUE))

means1 <- c("Mean of Dependent Variable", means[1:7])
names(means1) <- NULL

means2 <- c("Mean of Dependent Variable", means[8:14])
names(means2) <- NULL

# Generate the upper table
stargazer(models[1:7],
          omit = "(Constant)",
          title = "Testing RDI as an Instrument",
          align = TRUE,
          type = "latex",  # Use "latex" or "html" for exporting
          se = lapply(robust_ses[1:7], function(se) se[, 2]), # Extract robust SEs from coeftest
          dep.var.labels.include = FALSE,  # To mimic the 'Outcome' labeling
          column.labels = c("1990 dissimilarity index",
                             "Physical area (1910)",
                             "Population (1910)",
                             "Ethnic dissimilarity index (1910)",
                             "Ethnic isolation index (1910)",
                             "Percent black (1910)",
                             "Street-cars per capita (1915)"),
          add.lines = list(means1),
          omit.stat = c("rsq", "adj.rsq", "ser", "f")
)

# Generate the lower table
stargazer(models[8:14],
          omit = "(Constant)",
          title = "Testing RDI as an Instrument",
          align = TRUE,
          type = "text",  # Use "latex" or "html" for exporting
          se = lapply(robust_ses[1:7], function(se) se[, 2]), # Extract robust SEs from coeftest
          dep.var.labels.include = FALSE,  # To mimic the 'Outcome' labeling
          column.labels = c("Percent black (1920)",
                            "Percent literate (1920)",
                            "Labor force participation (1920)",
                            "Percent in trade (1920)",
                            "Percent in manufacturing (1920)",
                            "Percent in railroads (1920)",
                            "1990 income segregation"),
          add.lines = list(means2),
          omit.stat = c("rsq", "adj.rsq", "ser", "f"),
          notes = c("Robust standard errors in parentheses.",
                    "From Cutler-Glaeser-Vigdor data; sample limited to what that dataset provides.",
                    "Calculated from ipums.org; full sample represented."),
)





# EoF
