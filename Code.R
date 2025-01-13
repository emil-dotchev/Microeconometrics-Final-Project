# Load data and packages

library(haven)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)


data <- read_dta("ananat2011.dta")
summary(data)

columns_with_word <- names(data)[grepl("1920", names(data), ignore.case = TRUE)]
print(columns_with_word)

# herf = rdi
# dism1990 = 1990 segregation
# lenper = track_length (per sq km)
# passpc = street-cars per cap.

# Figure 3

# Create plot
ggplot(data, aes(x = herf, y = dism1990)) +
  geom_point(color = "blue", size = 2) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  geom_text(aes(label = name, vjust = -0.5), color = "blue", size = 3) +
  labs(x = "RDI", y = "1990 segregation", title = "Figure 3") +
  theme_minimal() + 
  xlim(0.2, 1) +
  ylim(0.2, 1)


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
          type = "text",  # Use "latex" or "html" for exporting
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






