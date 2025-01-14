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
# hsdrop_w:  
# hsgrad_w:  
# somecoll_w:  
# collgrad_w:  
# hsdrop_b:  
# hsgrad_b:  
# somecoll_b:  
# collgrad_b:  
# povrate_w:  
# povrate_b:  
# mt1proom_w:  
# mt1proom_b:  
# medgrent_w:  
# medgrent_b:  
# medgrentpinc_w:  
# medgrentpinc_b:  
# area1910:  Total area - 1910?
# passpc:  Per capita street car passengers–1915
# ngov62:  Number of local governments–1962
# manshr:  Manufacturing share–1990
# incseg:  Income segregation–1990 ?
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
# gini_w:  
# gini_b:  
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
# mv_st_winus_w:  
# mv_st_winus_b:  
# lfp_w:  Labor force participation (white)
# lfp_b:  Labor force participation (black)
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

# Import the data
data <- read_dta("ananat2011.dta")

# Step 1: Investigate data ==========================================

# Understand the data provided

class(df)
head(df)

# What is the dimension of the data
dim(df)


# Each row is a "Metropolitan Statistical Area" (MSA)
non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
print(sapply(df[non_numeric_cols], function(x) length(unique(x))))
rm(non_numeric_cols)

# Get names of numeric columns
numeric_cols <- names(df)[sapply(df, is.numeric)]

# Create a summary table to see the basics of the data.
summary_table <- data.frame(
  Column_Name = numeric_cols,
  Number_of_Rows = nrow(df),
  Number_of_Unique_Values = sapply(df[numeric_cols], function(x) length(unique(x))),
  Min = round(sapply(df[numeric_cols], min, na.rm = TRUE), 5),
  Max = round(sapply(df[numeric_cols], max, na.rm = TRUE), 5),
  Mean = round(sapply(df[numeric_cols], mean, na.rm = TRUE), 5),
  Median = round(sapply(df[numeric_cols], median, na.rm = TRUE), 5),
  Standard_Deviation = round(sapply(df[numeric_cols], sd, na.rm = TRUE), 5)
)

# Function to search and find the columns needed to reproduce the analysis
find_cols <- function(string) {
  return(
    colnames(data)[grepl(string, names(data), ignore.case = TRUE)]
  )
}

# Example usage
find_cols("gini")

# The regdum columns appear to indicate what region the city is in
print(sum(rowSums(data[find_cols("regdum")])))
colSums(data[find_cols("regdum")])


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

# Table 1  ----------------------------------------------------------------------------

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


# Table 2 -----------------------------------------------------------------------

# Top part
m1 <- lm(lngini_w ~ dism1990, data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- lm(povrate_w ~ dism1990, data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- lm(lngini_b ~ dism1990, data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- lm(povrate_b ~ dism1990, data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))


m5 <- ivreg(lngini_w ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m5, vcov = vcovHC(m5, type = "HC1"))
m6 <- ivreg(povrate_w ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m6, vcov = vcovHC(m6, type = "HC1"))
m7 <- ivreg(lngini_b ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m7, vcov = vcovHC(m7, type = "HC1"))
m8 <- ivreg(povrate_b ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m8, vcov = vcovHC(m8, type = "HC1"))

m9 <- lm(lngini_w ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m9, vcov = vcovHC(m9, type = "HC1"))
m10 <- lm(povrate_w ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m10, vcov = vcovHC(m10, type = "HC1"))
m11 <- lm(lngini_b ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m11, vcov = vcovHC(m11, type = "HC1"))
m12 <- lm(povrate_b ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m12, vcov = vcovHC(m12, type = "HC1"))

# Bottom part
m1 <- lm(ln90w90b ~ dism1990, data)
coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
m2 <- lm(ln10w10b ~ dism1990, data)
coeftest(m2, vcov = vcovHC(m2, type = "HC1"))
m3 <- lm(ln90w10b ~ dism1990, data)
coeftest(m3, vcov = vcovHC(m3, type = "HC1"))
m4 <- lm(ln90b10w ~ dism1990, data)
coeftest(m4, vcov = vcovHC(m4, type = "HC1"))


m5 <- ivreg(ln90w90b ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m5, vcov = vcovHC(m5, type = "HC1"))
m6 <- ivreg(ln10w10b ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m6, vcov = vcovHC(m6, type = "HC1"))
m7 <- ivreg(ln90w10b ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m7, vcov = vcovHC(m7, type = "HC1"))
m8 <- ivreg(ln90b10w ~ dism1990 + lenper | herf + lenper, data = data)
coeftest(m8, vcov = vcovHC(m8, type = "HC1"))

m9 <- lm(ln90w90b ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m9, vcov = vcovHC(m9, type = "HC1"))
m10 <- lm(ln10w10b ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m10, vcov = vcovHC(m10, type = "HC1"))
m11 <- lm(ln90w10b ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m11, vcov = vcovHC(m11, type = "HC1"))
m12 <- lm(ln90b10w ~ herf + lenper, data %>% filter(closeness <= -400))
coeftest(m12, vcov = vcovHC(m12, type = "HC1"))


# Table 3 -----------------------------------------------------------------------

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


# Table 5 -----------------------------------------------------------------------





