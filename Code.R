# Header ============================================================
# Project: Microeconometrics Final HW
# Authors: Emil Dotchev & Gabe Batres
# Date: 3/1/2025
# Description: Recreate analysis of the paper about racial seggregation
#  by Elizabeth Ananat.
#
# Explanation of Variables in Data
# name:  City of metropolitan statistical area (MSA)
# herf:  Railroad division index (RDI)
# lenper:  Total track length per square kilometer
# hsdrop_w:  High school drop rate (white)
# hsgrad_w:  High school graduates (white)
# somecoll_w:  Some college education (white)
# collgrad_w:  College graduates (white)
# hsdrop_b:  High school drop rate (black)
# hsgrad_b:  High school graduates (black)
# somecoll_b:  Some college education (black)
# collgrad_b: College graduates (black)
# povrate_w:  Poverty rate for whites
# povrate_b:  Poverty rate for blacks
# mt1proom_w:  Share of households with more than one person per room, White
# mt1proom_b:  Share of households with more than one person per room, Black
# medgrent_w:  Median rent, White
# medgrent_b:  Median rent, Black
# medgrentpinc_w:  Median rent as a percent of income, White
# medgrentpinc_b:  Median rentas a percent of income, Black
# area1910:  Total area - 1910
# passpc:  Per capita street car passengers–1915
# ngov62:  Number of local governments–1962
# manshr:  Manufacturing share–1990
# incseg:  Income segregation–1990
# closeness:  Distance from south
# regdum1:  Northeast region indicator
# regdum2:  Midwest region indicator
# regdum3:  2 cities in Oklahoma, also categorized as Midwest ? (see page 18)
# regdum4:  West region indicator
# ethseg10:  
# ethiso10:  
# ethexp10:  
# count1910:  
# black1910:  
# ctyliterate1920:  
# lfp1920:  
# ctytrade_wkrs1920:  
# ctymanuf_wkrs1920:  
# ctyrail_wkrs1920:  
# count1920:  
# black1920:  
# gini_w:  Gini index for whites
# gini_b:  Gini index for blacks
# p10_w:  
# p50_w:  50th percentile income for whites ?
# p90_w:  
# p10_b:  
# p50_b:  50th percentile income for blacks ?
# p90_b:  
# grsrnt_w:  
# grsrnt_b:  
# lngini_w:  
# lngini_b:  
# herfscore:  
# ln90w90b:  
# ln10w10b:  
# ln90w10b:  
# ln90b10w:  
# mv_st_winus_w:  Percent of residents who are in-migrants, White
# mv_st_winus_b:  Percent of residents who are in-migrants, Black
# lfp_w:  
# lfp_b:  
# dism1990:  Dissimilarity index–1990
# pop1990:  Population–1990
# pctbk1990:  Percent black–1990
# ===================================================================



# Step 0: Setup environment =========================================
# Begin by loading necessary libraries and set the working directory

# Load Required Libraries
library(haven)  # Library to load .dta file
library(dplyr)  # Library for data.frame manipultation
library(ggplot2)  # Library for visualizations
library(stargazer)  # Library for tables
library(lmtest)
library(sandwich)
library(AER)  # Library for 2SLS
library(kableExtra) # for manual tables

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


# Function to search and find the columns needed to reproduce the analysis
find_columns <- function(string) {
  return(
    colnames(data)[grepl(string, names(data), ignore.case = TRUE)]
  )
}

# Example usage
find_columns("gini") # you don't need print the way you defined this function, just run it

# The regdum columns appear to indicate what region the city is in
print(sum(rowSums(data[find_columns("regdum")])))
colSums(data[find_columns("regdum")])


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
    geom_text(aes(label = name), size = 2, color = "darkgray", alpha = 0.6,
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
             label = paste("Slope (Top) =", round(slope_top, 5)), 
             color = "red3", hjust = 0, vjust = 1) +  # Top line label
    annotate("text", 
             x = min(df[[x_col]]), y = max(df[[y_col]]) - 0.1 * diff(range(data[[y_col]])), 
             label = paste("Slope (Bottom) =", round(slope_bottom, 5)), 
             color = "blue3", hjust = 0, vjust = 1) +  # Bottom line label (adjusted position)
    theme_bw() +
    scale_color_manual(values = c("red3", "blue3")) +  # Set colors for points
    scale_fill_manual(values = c("red3", "blue3"))     # Set colors for lines
}

create_partitioned_scatter_plot(data, "closeness", "dism1990", "herf")  # Replace with actual column names


# Step 3: Regression tables of the paper ============================

# Table 1 - Testing RDI as an Instrument ----------------------------
# Divide column by 1,000
data$area1910 <- data$area1910/1000
data$count1910 <- data$count1910/1000
data$passpc <- data$passpc/1000

# Dependent Variables
focus_variables1 <- c("dism1990")
false_variables1 <- c("area1910", "count1910", "ethseg10", "ethiso10", "black1910", "passpc",
                      "black1920", "ctyliterate1920", "lfp1920", "ctytrade_wkrs1920", "ctymanuf_wkrs1920", "ctyrail_wkrs1920",
                      "incseg")
vars <- c(focus_variables1, false_variables1)

# Run OLS regressions for each variable
models <- lapply(vars, function(var) {
  lm(as.formula(paste(var, "~ herf + lenper")), data = data)
})

# Compute robust standard errors
robust_ses <- lapply(models,
                     function(model) coeftest(model, 
                                              vcov = vcovHC(model, 
                                                            type = "HC1")))

# Compute means
means <- sapply(vars, function(var) round(mean(data[[var]], na.rm = TRUE), 3))

means1 <- c("Mean of Dependent Variable", means[1:7])
names(means1) <- NULL

means2 <- c("Mean of Dependent Variable", means[8:14])
names(means2) <- NULL

# Generate the upper table
stargazer(models[1:7],
          omit = "(Constant)",
          title = "Testing RDI as an Instrument",
          align = TRUE,
          type = "latex",  
          se = lapply(robust_ses[1:7], function(se) se[, 2]), 
          dep.var.labels.include = FALSE, 
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
          type = "latex",  # Use "latex" or "html" for exporting
          se = lapply(robust_ses[8:14], function(se) se[, 2]), # Extract robust SEs from coeftest
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
                    "Calculated from ipums.org; full sample represented.")
)


# Table 2 - The Effects of Segregation on Poverty and Inequality among Blacks and Whites ---------------------

# Define a function to run a model and calculate robust statistics
run_model <- function(formula, data, is_ivreg = FALSE) {
  model <- if (is_ivreg) ivreg(formula, data = data) else lm(formula, data = data)
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
}

# Define formulas for the models
ols_formulas <- list(
  lngini_w ~ dism1990,
  povrate_w ~ dism1990,
  lngini_b ~ dism1990,
  povrate_b ~ dism1990
)

iv_formulas <- list(
  lngini_w ~ dism1990 + lenper | herf + lenper,
  povrate_w ~ dism1990 + lenper | herf + lenper,
  lngini_b ~ dism1990 + lenper | herf + lenper,
  povrate_b ~ dism1990 + lenper | herf + lenper
)

falsification_formulas <- list(
  lngini_w ~ herf + lenper,
  povrate_w ~ herf + lenper,
  lngini_b ~ herf + lenper,
  povrate_b ~ herf + lenper
)

# Run the models and store results in lists
ols_results <- lapply(ols_formulas, function(f) run_model(f, data))
iv_results <- lapply(iv_formulas, function(f) run_model(f, data, is_ivreg = TRUE))
falsification_results <- lapply(falsification_formulas, function(f) run_model(f, data %>% filter(closeness <= -400)))

# Function to format coefficients with significance stars
format_with_stars <- function(coef, pval) {
  if (pval < 0.01) {
    return(paste0(round(coef, 3), "***"))
  } else if (pval < 0.05) {
    return(paste0(round(coef, 3), "**"))
  } else if (pval < 0.1) {
    return(paste0(round(coef, 3), "*"))
  } else {
    return(round(coef, 3))
  }
}

# Extract results from models
results <- data.frame(
  Outcome = c("Gini Index", "", "Poverty Rate", ""),
  Whites_OLS = c(
    format_with_stars(coef(ols_results[[1]])["dism1990"], ols_results[[1]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[1]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(ols_results[[2]])["dism1990"], ols_results[[2]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[2]]["dism1990", "Std. Error"], 3), ")")
  ),
  Blacks_OLS = c(
    format_with_stars(coef(ols_results[[3]])["dism1990"], ols_results[[3]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[3]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(ols_results[[4]])["dism1990"], ols_results[[4]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[4]]["dism1990", "Std. Error"], 3), ")")
  ),
  Whites_2SLS = c(
    format_with_stars(coef(iv_results[[1]])["dism1990"], iv_results[[1]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[1]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(iv_results[[2]])["dism1990"], iv_results[[2]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[2]]["dism1990", "Std. Error"], 3), ")")
  ),
  Blacks_2SLS = c(
    format_with_stars(coef(iv_results[[3]])["dism1990"], iv_results[[3]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[3]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(iv_results[[4]])["dism1990"], iv_results[[4]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[4]]["dism1990", "Std. Error"], 3), ")")
  ),
  Whites_Falsification = c(
    format_with_stars(coef(falsification_results[[1]])["herf"], falsification_results[[1]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[1]]["herf", "Std. Error"], 3), ")"),
    format_with_stars(coef(falsification_results[[2]])["herf"], falsification_results[[2]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[2]]["herf", "Std. Error"], 3), ")")
  ),
  Blacks_Falsification = c(
    format_with_stars(coef(falsification_results[[3]])["herf"], falsification_results[[3]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[3]]["herf", "Std. Error"], 3), ")"),
    format_with_stars(coef(falsification_results[[4]])["herf"], falsification_results[[4]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[4]]["herf", "Std. Error"], 3), ")")
  )
)


# Create the kableExtra table
latex_output <- results %>%
  kbl(
    caption = "The Effects of Segregation on Poverty and Inequality among Blacks and Whites", 
    col.names = c("Outcome", 
                  "Whites", "Blacks", 
                  "Whites", "Blacks", 
                  "Whites", "Blacks"),
    booktabs = TRUE,  # Use LaTeX booktabs styling
    format = "latex",
    align = "lcccccc" # Align columns (left for Outcome, center for others)
  ) %>%
  add_header_above(
    c(" " = 1, "OLS" = 2, "2SLS" = 2, "Falsification" = 2) # Group column headers
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Adjust table to fit
    font_size = 10                                    # Optional: Adjust font size
  ) %>%
  column_spec(1, bold = TRUE) %>%                     # Make the first column bold
  row_spec(c(2, 4), italic = TRUE)                   # Italicize rows for standard errors

# Export to tex
writeLines(latex_output, "table2_top.tex")

# Bottom part

# OLS models 
ols_formulas <- list(
  ln90w90b ~ dism1990,
  ln10w10b ~ dism1990,
  ln90w10b ~ dism1990,
  ln90b10w ~ dism1990
)

# IV models 
iv_formulas <- list(
  ln90w90b ~ dism1990 + lenper | herf + lenper,
  ln10w10b ~ dism1990 + lenper | herf + lenper,
  ln90w10b ~ dism1990 + lenper | herf + lenper,
  ln90b10w ~ dism1990 + lenper | herf + lenper
)

# Falsification models 
falsification_formulas <- list(
  ln90w90b ~ herf + lenper,
  ln10w10b ~ herf + lenper,
  ln90w10b ~ herf + lenper,
  ln90b10w ~ herf + lenper
)

ols_results <- lapply(ols_formulas, function(f) run_model(f, data))
iv_results <- lapply(iv_formulas, function(f) run_model(f, data, is_ivreg = TRUE))
falsification_results <- lapply(falsification_formulas, function(f) run_model(f, data %>% filter(closeness <= -400)))

# Extract results from models
results <- data.frame(
  Outcome = c("90 white: 90 black", "", "10 white: 10 black", "", "90 white: 10 black", "", "10 white: 90 black", ""),
  OLS = c(
    format_with_stars(coef(ols_results[[1]])["dism1990"], ols_results[[1]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[1]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(ols_results[[2]])["dism1990"], ols_results[[2]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[2]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(ols_results[[3]])["dism1990"], ols_results[[3]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[3]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(ols_results[[4]])["dism1990"], ols_results[[4]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(ols_results[[4]]["dism1990", "Std. Error"], 3), ")")
  ),
  TwoSLS = c(
    format_with_stars(coef(iv_results[[1]])["dism1990"], iv_results[[1]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[1]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(iv_results[[2]])["dism1990"], iv_results[[2]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[2]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(iv_results[[3]])["dism1990"], iv_results[[3]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[3]]["dism1990", "Std. Error"], 3), ")"),
    format_with_stars(coef(iv_results[[4]])["dism1990"], iv_results[[4]]["dism1990", "Pr(>|t|)"]),
    paste0("(", round(iv_results[[4]]["dism1990", "Std. Error"], 3), ")")
  ),
  Falsification = c(
    format_with_stars(coef(falsification_results[[1]])["herf"], falsification_results[[1]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[1]]["herf", "Std. Error"], 3), ")"),
    format_with_stars(coef(falsification_results[[2]])["herf"], falsification_results[[2]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[2]]["herf", "Std. Error"], 3), ")"),
    format_with_stars(coef(falsification_results[[3]])["herf"], falsification_results[[3]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[3]]["herf", "Std. Error"], 3), ")"),
    format_with_stars(coef(falsification_results[[4]])["herf"], falsification_results[[4]]["herf", "Pr(>|t|)"]),
    paste0("(", round(falsification_results[[4]]["herf", "Std. Error"], 3), ")")
  )
)


# Create the kableExtra table
latex_output <- results %>%
  kbl(
    caption = "The Effects of Segregation on Poverty and Inequality among Blacks and Whites", 
    col.names = c("Outcome", 
                  "White:black ratios", "White:black ratios", 
                  "White:black ratios"),
    booktabs = TRUE,  # Use LaTeX booktabs styling
    format = "latex",
    align = "lcccccc" # Align columns (left for Outcome, center for others)
  ) %>%
  add_header_above(
    c(" " = 1, "OLS" = 1, "2SLS" = 1, "Falsification" = 1) # Group column headers
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "scale_down"), # Adjust table to fit
    font_size = 10                                    # Optional: Adjust font size
  ) %>%
  column_spec(1, bold = TRUE) %>%                     # Make the first column bold
  row_spec(c(2, 4), italic = TRUE)                   # Italicize rows for standard errors

# Export to tex
writeLines(latex_output, "table2_bttm.tex")


# Table 3 — Robustness Checks ---------------------------------------

# 1990 Controls:

# Population
m1 <- ivreg(lngini_w ~ dism1990 + lenper + I(pop1990/1000)| herf + lenper + I(pop1990/1000), data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + I(pop1990/1000)| herf + lenper + I(pop1990/1000), data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + I(pop1990/1000)| herf + lenper + I(pop1990/1000), data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + I(pop1990/1000)| herf + lenper + I(pop1990/1000), data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Percent black
m1 <- ivreg(lngini_w ~ dism1990 + lenper + pctbk1990| herf + lenper + pctbk1990, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + pctbk1990| herf + lenper + pctbk1990, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + pctbk1990| herf + lenper + pctbk1990, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + pctbk1990| herf + lenper + pctbk1990, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Education 
m1 <- ivreg(lngini_w ~ dism1990 + lenper + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b| herf + lenper + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b| herf + lenper + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b| herf + lenper + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b| herf + lenper + hsdrop_w + hsdrop_b + hsgrad_w + hsgrad_b + somecoll_w + somecoll_b + collgrad_w + collgrad_b, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Share employed in manufacturing
m1 <- ivreg(lngini_w ~ dism1990 + lenper + manshr| herf + lenper + manshr, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + manshr| herf + lenper + manshr, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + manshr| herf + lenper + manshr, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + manshr| herf + lenper + manshr, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Labor force participation
m1 <- ivreg(lngini_w ~ dism1990 + lenper + lfp_w + lfp_b| herf + lenper + lfp_w + lfp_b, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + lfp_w + lfp_b| herf + lenper + lfp_w + lfp_b, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + lfp_w + lfp_b| herf + lenper + lfp_w + lfp_b, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + lfp_w + lfp_b| herf + lenper + lfp_w + lfp_b, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Number of local governments
m1 <- ivreg(lngini_w ~ dism1990 + lenper + ngov62| herf + lenper + ngov62, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + ngov62| herf + lenper + ngov62, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + ngov62| herf + lenper + ngov62, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + ngov62| herf + lenper + ngov62, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# 1920 Controls:

# Population
m1 <- ivreg(lngini_w ~ dism1990 + lenper + count1920| herf + lenper + count1920, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + count1920| herf + lenper + count1920, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + count1920| herf + lenper + count1920, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + count1920| herf + lenper + count1920, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Percent black
m1 <- ivreg(lngini_w ~ dism1990 + lenper + black1920| herf + lenper + black1920, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + black1920| herf + lenper + black1920, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + black1920| herf + lenper + black1920, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + black1920| herf + lenper + black1920, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Literacy !
m1 <- ivreg(lngini_w ~ dism1990 + lenper + ctyliterate1920| herf + lenper + ctyliterate1920, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + ctyliterate1920| herf + lenper + ctyliterate1920, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + ctyliterate1920| herf + lenper + ctyliterate1920, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + ctyliterate1920| herf + lenper + ctyliterate1920, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Share employed in manufacturing !!!
m1 <- ivreg(lngini_w ~ dism1990 + lenper + ctymanuf_wkrs1920| herf + lenper + ctymanuf_wkrs1920, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + ctymanuf_wkrs1920| herf + lenper + ctymanuf_wkrs1920, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + ctymanuf_wkrs1920| herf + lenper + ctymanuf_wkrs1920, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + ctymanuf_wkrs1920| herf + lenper + ctymanuf_wkrs1920, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Labor force participation !
m1 <- ivreg(lngini_w ~ dism1990 + lenper + lfp1920| herf + lenper + lfp1920, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + lfp1920| herf + lenper + lfp1920, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

m3 <- ivreg(povrate_w ~ dism1990 + lenper  + lfp1920| herf + lenper + lfp1920, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + lfp1920| herf + lenper + lfp1920, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Control for propensity score
m1 <- ivreg(lngini_w ~ dism1990 + lenper + herfscore| herf + lenper + herfscore, data = data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- ivreg(lngini_b ~ dism1990 + lenper  + herfscore| herf + lenper + herfscore, data = data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- ivreg(povrate_w ~ dism1990 + lenper  + herfscore| herf + lenper + herfscore, data = data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- ivreg(povrate_b ~ dism1990 + lenper  + herfscore| herf + lenper + herfscore, data = data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))


# Table 4 — The Effects of 1990 Segregation on 1990 City Demand ----------------------

# Outcome: Percent of residents who are in-migrants
# OLS
in_w_ols <- lm(mv_st_winus_w ~ dism1990, data)
round(coeftest(in_w_ols, vcov = vcovHC(in_w_ols, type = "HC1")), 3)

in_b_ols <- lm(mv_st_winus_b ~ dism1990, data)
round(coeftest(in_b_ols, vcov = vcovHC(in_b_ols, type = "HC1")), 3)

# IV
in_w_iv <- ivreg(mv_st_winus_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(in_w_iv, vcov = vcovHC(in_w_iv, type = "HC1")), 3)

in_b_iv <- ivreg(mv_st_winus_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(in_b_iv, vcov = vcovHC(in_b_iv, type = "HC1")), 3)

# Falsification: Reduced form effect of RDI among cities far from the South
in_w_f <- lm(mv_st_winus_w ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(in_w_f, vcov = vcovHC(in_w_f, type = "HC1")), 3)

in_b_f <- lm(mv_st_winus_b ~ herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(in_b_f, vcov = vcovHC(in_b_f, type = "HC1")), 3)

# Outcome: Median rent
# OLS
mr_w_ols <- lm(medgrent_w ~ dism1990, data)
round(coeftest(mr_w_ols, vcov = vcovHC(mr_w_ols, type = "HC1")), 0)

mr_b_ols <- lm(medgrent_b ~ dism1990, data)
round(coeftest(mr_b_ols, vcov = vcovHC(mr_b_ols, type = "HC1")), 0)

# IV
mr_w_iv <- ivreg(medgrent_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mr_w_iv, vcov = vcovHC(mr_w_iv, type = "HC1")), 0)

mr_b_iv <- ivreg(medgrent_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mr_b_iv, vcov = vcovHC(mr_b_iv, type = "HC1")), 0)

# Falsification: Reduced form effect of RDI among cities far from the South
mr_w_f <- lm(medgrent_w ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mr_w_f, vcov = vcovHC(mr_w_f, type = "HC1")), 0)

mr_b_f <- lm(medgrent_b ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mr_b_f, vcov = vcovHC(mr_b_f, type = "HC1")), 0)

# Outcome: Median rent as a percent of income
# OLS
mrpi_w_ols <- lm(medgrentpinc_w ~ dism1990, data)
round(coeftest(mrpi_w_ols, vcov = vcovHC(mrpi_w_ols, type = "HC1")), 3)

mrpi_b_ols <- lm(medgrentpinc_b ~ dism1990, data)
round(coeftest(mrpi_b_ols, vcov = vcovHC(mrpi_b_ols, type = "HC1")), 3)

# IV
mrpi_w_iv <- ivreg(medgrentpinc_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mrpi_w_iv, vcov = vcovHC(mrpi_w_iv, type = "HC1")), 3)

mrpi_b_iv <- ivreg(medgrentpinc_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mrpi_b_iv, vcov = vcovHC(mrpi_b_iv, type = "HC1")), 3)

# Falsification: Reduced form effect of RDI among cities far from the South
mrpi_w_f <- lm(medgrentpinc_w ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mrpi_w_f, vcov = vcovHC(mrpi_w_f, type = "HC1")), 3)

mrpi_b_f <- lm(medgrentpinc_b ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mrpi_b_f, vcov = vcovHC(mrpi_b_f, type = "HC1")), 3)

# Outcome: Share of households with more than one person per room
# OLS
mt_w_ols <- lm(mt1proom_w ~ dism1990, data)
round(coeftest(mt_w_ols, vcov = vcovHC(mt_w_ols, type = "HC1")), 3)

mt_b_ols <- lm(mt1proom_b ~ dism1990, data)
round(coeftest(mt_b_ols, vcov = vcovHC(mt_b_ols, type = "HC1")), 3)

# IV
mt_w_iv <- ivreg(mt1proom_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mt_w_iv, vcov = vcovHC(mt_w_iv, type = "HC1")), 3)

mt_b_iv <- ivreg(mt1proom_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(mt_b_iv, vcov = vcovHC(mt_b_iv, type = "HC1")), 3)

# Falsification: Reduced form effect of RDI among cities far from the South
mt_w_f <- lm(mt1proom_w ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mt_w_f, vcov = vcovHC(mt_w_f, type = "HC1")), 3)

mt_b_f <- lm(mt1proom_b ~ herf + lenper, data = data %>% filter(closeness <= -400))
round(coeftest(mt_b_f, vcov = vcovHC(mt_b_f, type = "HC1")), 3)

# Define function to extract robust standard errors
robust_se <- function(model) {
  return(
    sqrt(diag(vcovHC(model, type = "HC1")))
  )
}


# Define a funciton to recreate similar visualizations
table4_generate <- function(w_model, b_model, col_label, cov_label, omit = "(Constant)|lenper") {
  return(
    stargazer(
      w_model, b_model,
      se = list(robust_se(w_model), robust_se(b_model)),  # Use robust SE
      type = "text",
      title = "OLS, IV, and Falsification Analysis for Table 4",
      dep.var.labels = c("White", "Black"),
      column.labels = col_label,
      column.separate = 2,  # Group each dependent variable into its own block
      covariate.labels = cov_label,
      omit = omit,  # Omit other variables (intercept, control variables)
      omit.stat = c("rsq", "adj.rsq", "ser", "f"),  # Omit statistics
      notes = "Dependent variable: Segregation. Additional controls omitted for brevity.",  # Add note
      align = TRUE
    )
  )
}
# Summarize results using stargazer
#  - The stargazer package cannot directly create the same tables,
#    so I am breaking up the output and recombining later in the paper.
# OLS Row
table4_generate(in_w_ols, in_b_ols, "In-Migration", "OLS")
table4_generate(mr_w_ols, mr_b_ols, "Median Rent", "OLS")
table4_generate(mrpi_w_ols, mrpi_b_ols, "Rent Income", "OLS")
table4_generate(mt_w_ols, mt_b_ols, "Room", "OLS")

# IV Row
table4_generate(in_w_iv, in_b_iv, "In-Migration", "Instrument Variable")
table4_generate(mr_w_iv, mr_b_iv, "Median Rent", "Instrument Variable")
table4_generate(mrpi_w_iv, mrpi_b_iv, "Rent Income", "Instrument Variable")
table4_generate(mt_w_iv, mt_b_iv, "Room", "Instrument Variable")

# Falsification Row
table4_generate(in_w_f, in_b_f, "In-Migration", "Falsification")
table4_generate(mr_w_f, mr_b_f, "Median Rent", "Falsification")
table4_generate(mrpi_w_f, mrpi_b_f, "Rent Income", "Falsification")
table4_generate(mt_w_f, mt_b_f, "Room", "Falsification")


# Table 5 — The Effects of 1980 Dissimilarity on Human Capital of 22- to 30-Year-Olds in 1980 --------------

# Outcome: Share who are high school dropouts
# OLS
hsd_w_ols <- lm(hsdrop_w ~ dism1990 + regdum1 + regdum2 + regdum3, data)
round(coeftest(hsd_w_ols, vcov = vcovHC(hsd_w_ols, type = "HC1")), 3)

hsd_b_ols <- lm(hsdrop_b ~ dism1990 + regdum1 + regdum2 + regdum3, data)
round(coeftest(hsd_b_ols, vcov = vcovHC(hsd_b_ols, type = "HC1")), 3)

# IV
hsd_w_iv <- ivreg(hsdrop_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(hsd_w_iv, vcov = vcovHC(hsd_w_iv, type = "HC1")), 3)
hsd_b_iv <- ivreg(hsdrop_b ~ dism1990 + lenper + regdum1 + regdum2 + regdum3| herf + lenper + regdum1 + regdum2 + regdum3, data = data)
round(coeftest(hsd_b_iv, vcov = vcovHC(hsd_b_iv, type = "HC1")), 3)

# Falsification: Reduced form effect of RDI among cities far from the South
hsd_w_f <- ivreg(hsdrop_w ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(hsd_w_f, vcov = vcovHC(hsd_w_f, type = "HC1")), 3)
hsd_b_f <- ivreg(hsdrop_b ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(hsd_b_f, vcov = vcovHC(hsd_b_f, type = "HC1")), 3)

# Outcome: Share who are high school graduates
# OLS
hsg_w_ols <- lm(hsgrad_w ~ dism1990, data)
round(coeftest(hsg_w_ols, vcov = vcovHC(hsg_w_ols, type = "HC1")), 0)
hsg_b_ols <- lm(hsgrad_b ~ dism1990, data)
round(coeftest(hsg_b_ols, vcov = vcovHC(hsg_b_ols, type = "HC1")), 0)
# IV
hsg_w_iv <- ivreg(hsgrad_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(hsg_w_iv, vcov = vcovHC(hsg_w_iv, type = "HC1")), 0)
hsg_b_iv <- ivreg(hsgrad_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(hsg_b_iv, vcov = vcovHC(hsg_b_iv, type = "HC1")), 0)
# Falsification: Reduced form effect of RDI among cities far from the South
hsg_w_f <- ivreg(hsgrad_w ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(hsg_w_f, vcov = vcovHC(hsg_w_f, type = "HC1")), 0)
hsg_b_f <- ivreg(hsgrad_b ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(hsg_b_f, vcov = vcovHC(hsg_b_f, type = "HC1")), 0)

# Outcome: Share who have some college
# OLS
sc_w_ols <- lm(somecoll_w ~ dism1990, data)
round(coeftest(sc_w_ols, vcov = vcovHC(sc_w_ols, type = "HC1")), 3)
sc_b_ols <- lm(somecoll_b ~ dism1990, data)
round(coeftest(sc_b_ols, vcov = vcovHC(sc_b_ols, type = "HC1")), 3)
# IV
sc_w_iv <- ivreg(somecoll_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(sc_w_iv, vcov = vcovHC(sc_w_iv, type = "HC1")), 3)
sc_b_iv <- ivreg(somecoll_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(sc_b_iv, vcov = vcovHC(sc_b_iv, type = "HC1")), 3)
# Falsification: Reduced form effect of RDI among cities far from the South
sc_w_f <- ivreg(somecoll_w ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(sc_w_f, vcov = vcovHC(sc_w_f, type = "HC1")), 3)
sc_b_f <- ivreg(somecoll_b ~ dism1990 + lenper | herf + lenper, data = data %>% filter(closeness < -400))
round(coeftest(sc_b_f, vcov = vcovHC(sc_b_f, type = "HC1")), 3)

# Outcome: Share who are college graduates
# OLS
cg_w_ols <- lm(collgrad_w ~ dism1990, data)
round(coeftest(cg_w_ols, vcov = vcovHC(cg_w_ols, type = "HC1")), 3)
cg_b_ols <- lm(collgrad_b ~ dism1990, data)
round(coeftest(cg_b_ols, vcov = vcovHC(cg_b_ols, type = "HC1")), 3)
# IV
cg_w_iv <- ivreg(collgrad_w ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(cg_w_iv, vcov = vcovHC(cg_w_iv, type = "HC1")), 3)
cg_b_iv <- ivreg(collgrad_b ~ dism1990 + lenper | herf + lenper, data = data)
round(coeftest(cg_b_iv, vcov = vcovHC(cg_b_iv, type = "HC1")), 3)
# Falsification: Reduced form effect of RDI among cities far from the South
cg_w_f <- lm(collgrad_w ~ dism1990 + lenper, data = data %>% filter(closeness < -400))
round(coeftest(cg_w_f, vcov = vcovHC(cg_w_f, type = "HC1")), 3)
cg_b_f <- lm(collgrad_b ~ dism1990 + lenper, data = data %>% filter(closeness < -400))
round(coeftest(cg_b_f, vcov = vcovHC(cg_b_f, type = "HC1")), 3)


# Summarize results using stargazer
#  - The stargazer package cannot directly create the same tables,
#    so I am breaking up the output and recombining later in the paper.
# OLS Row
table4_generate(hsd_w_ols, hsd_b_ols, "high school dropouts", "OLS")
table4_generate(hsg_w_ols, hsg_b_ols, "high school graduates", "OLS")
table4_generate(sc_w_ols, sc_b_ols, "some college", "OLS")
table4_generate(cg_w_ols, cg_b_ols, "college graduates", "OLS")

# IV Row
table4_generate(hsd_w_iv, hsd_b_iv, "high school dropouts", "Instrument Variable")
table4_generate(hsg_w_iv, hsg_b_iv, "high school graduates", "Instrument Variable")
table4_generate(sc_w_iv, sc_b_iv, "some college", "Instrument Variable")
table4_generate(cg_w_iv, cg_b_iv, "college graduates", "Instrument Variable")

# Falsification Row
table4_generate(hsd_w_f, hsd_b_f, "high school dropouts", "Falsification")
table4_generate(hsg_w_f, hsg_b_f, "high school graduates", "Falsification")
table4_generate(sc_w_f, sc_b_f, "some college", "Falsification")
table4_generate(cg_w_f, cg_b_f, "college graduates", "Falsification")


# Step 4: Additional Analysis =======================================
  

# Step 5: Results & Conclusions =====================================
# (Add your code for summarizing and presenting results here)




# EoF
