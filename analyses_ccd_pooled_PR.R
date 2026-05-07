## Library calls  #####
# POOLED EFFECTS STANDARD ERRORS FOR COUNTRY AND RESPONDENT
# Load required libraries


library(tidyverse)        # Data wrangling & plotting
library(haven)            # Read Stata files
library(broom)            # Convert model objects to tidy data frames
library(cregg)            # AMCEs and MMs for conjoint experiments
library(survey)           # Survey-weighted regression models
library(scales)           # For nice ggplot scales
library(marginaleffects)  # Compute marginal effects
library(ggforce)          # Extra ggplot facets
library(ggdist)           # Distribution plots
library(patchwork)        # Combine ggplot figures
library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(ggforce)
library(tictoc)
library(emmeans)
library(performance)
library(tools)

##Global stuff ----  

# Set output directory

anonymyzed_path1 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/data and scripts/"
anonymyzed_path2 = "C:/Users/100722gsc/OneDrive - Erasmus University Rotterdam/Postdoc/SUBMISSIONS/IAFS/IJPOR/new output/"

dataset_rep = anonymyzed_path1
gdrive_code = ""

clean = F

output_wd = paste0(anonymyzed_path2,
                   "Clean_", clean, "/POOLED_with_sd/")


if (!dir.exists(output_wd)) {
  dir.create(output_wd, recursive = TRUE)
}
# Load dataset
if(clean==T)
{
  data = readRDS(paste0(anonymyzed_path1,
                        "/dataset_PR.RDS"))
}
if(clean ==F)
{
  data = readRDS(paste0(anonymyzed_path1,
                        "/dataset_PR_notclean.RDS"))
}


z_alpha=1.96 #95% significance
z_alpha_99=2.575 #99% significance
z_alpha_90=1.645 #90% significance

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

## General MM ################


formula = ccd_chosen_rw ~ ccd_gender+
  ccd_age+ccd_religion+ccd_citysize+ccd_profession+
  ccd_consc+ccd_openness+ ccd_neuroticism+
  ccd_restaurant+ccd_transport+ccd_pet#+ ccd_country # fixed effects

model_svy <- svyglm(
  formula,
  design = design
)

# 3. Generate a weight variable that is simply the proportion of each category
# Define the conjoint variables
conjoint_vars <- c("ccd_gender", "ccd_age","ccd_religion", 
                   "ccd_citysize","ccd_profession",
                   "ccd_consc","ccd_openness","ccd_neuroticism",
                   "ccd_restaurant","ccd_transport", "ccd_pet"
)



# Compute frequency weights (proportion of each category in the dataset)
data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})

tictoc::toc() 

ATEs_mock = data |>
  cj(ccd_chosen_rw ~ ccd_gender+
       ccd_age+ccd_religion+ccd_citysize+ccd_profession+
       ccd_consc+ccd_openness+ ccd_neuroticism+
       ccd_restaurant+ccd_transport+ccd_pet,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
  
}

out_file <- paste0(output_wd, "mm_clustSE.rds")

# Create directory if it does not exist
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE.rds"))

## General AMCES ################
###


ATEs_mock <- data |>
  cj(ccd_chosen_rw ~ ccd_gender+
       ccd_age+ccd_religion+ccd_citysize+ccd_profession+
       ccd_consc+ccd_openness+ ccd_neuroticism+
       ccd_restaurant+ccd_transport+ccd_pet,
     id = ~respid,
     estimate = "amce")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs_mock$estimate = ifelse(ATEs_mock$estimate != 0, NA, 0)
ATEs_mock$std.error = NA
ATEs_mock$lower = NA
ATEs_mock$upper = NA

ATEs = ATEs_mock

amces = as.data.frame(summary(model_svy)$coefficients)[-1, ]

amces_clean <- amces %>%
  mutate(
    feature = str_extract(rownames(.), "^ccd_[a-z]+"), # extracts 'ccd_gender', 'ccd_age', etc.
    level = str_remove(rownames(.), "^ccd_[a-z]+")     # removes the feature name, leaving the level
  ) %>%
  mutate(
    feature = str_trim(feature),
    level = str_trim(level)
  ) %>%
  rename(
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic_value = `t value`
  ) %>%
  select(feature, level, estimate, std.error)

ATEs_filled <- ATEs_mock %>%
  left_join(amces_clean, by = c("feature", "level")) %>%
  mutate(
    estimate = coalesce(estimate.y, estimate.x),
    std.error = coalesce(std.error.y, std.error.x)
  ) %>%
  select(outcome, statistic, feature, level, estimate, std.error, lower, upper)

ATEs = ATEs_filled

ATEs$lower = ATEs$estimate - z_alpha*ATEs$std.error
ATEs$upper = ATEs$estimate + z_alpha*ATEs$std.error

ATEs$lower90 = ATEs$estimate - z_alpha_90*ATEs$std.error
ATEs$upper90 = ATEs$estimate + z_alpha_90*ATEs$std.error

ATEs$lower99 = ATEs$estimate - z_alpha_99*ATEs$std.error
ATEs$upper99 = ATEs$estimate + z_alpha_99*ATEs$std.error

# I need to have the same format as the others
ATEs$z = NA
ATEs$p = NA

saveRDS(ATEs, file = paste0(output_wd,"amce_clustSE.rds"))




## Interactions #####
### Interacted sociodemos ####
#### MM #####


data$interacted_sociodemos = interaction(#data$ccd_age, 
  data$ccd_profession,
  data$ccd_religion,
  sep =",\n")

formula = ccd_chosen_rw ~ interacted_sociodemos + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_sociodemos")


data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_sociodemos,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
  
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_sociodemos_jobreligion.rds"))


### Interacted psycho ####

#### MM #####

data$interacted_psycho = interaction(data$ccd_consc, 
                                     data$ccd_openness, 
                                     data$ccd_neuroticism, 
                                     sep =",\n")


formula = ccd_chosen_rw ~ interacted_psycho + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_psycho")


data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_psycho,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_psycho.rds"))

#### MM ####

data$interacted_cultural = interaction(data$ccd_restaurant, 
                                       data$ccd_transport, 
                                       sep =",\n")

formula = ccd_chosen_rw ~ interacted_cultural + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_cultural")

data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_cultural,
     id = ~respid,
     estimate = "mm")


ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
  
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_cultural.rds"))


### Interacted, cross1 ####

#### MM ####


data$interacted_cross1 = interaction(data$ccd_religion, 
                                     data$ccd_transport, 
                                     sep =",\n")

formula = ccd_chosen_rw ~ interacted_cross1 + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_cross1")


data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_cross1,
     id = ~respid,
     estimate = "mm")


ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
  
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_cross1.rds"))

### Interacted, cross2 ####
#### MM ####

data$interacted_cross2 = interaction(data$ccd_profession, 
                                     data$ccd_transport, 
                                     sep =",\n")

formula = ccd_chosen_rw ~ interacted_cross2 + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_cross2")


data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_cross2,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
  
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_cross2.rds"))

###Interacted, cross3 ####
#### MM ####

data$interacted_cross3 = interaction(data$ccd_profession, 
                                     data$ccd_transport,
                                     data$ccd_consc,
                                     sep =", ")

formula = ccd_chosen_rw ~ interacted_cross3 + ccd_country

design <- svydesign(
  ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
  data = data  # Use the dataset
)

model_svy <- svyglm(
  formula,
  design = design
)

conjoint_vars <- c("interacted_cross3")


data1 <- data %>%
  group_by(across(all_of(conjoint_vars))) %>%  # Group by all conjoint variables
  mutate(cells = n() / nrow(data)) %>%  # Compute proportion
  ungroup()

tictoc::tic() 
marginal_means_list <- lapply(conjoint_vars, function(var) {
  avg_predictions(model_svy, by=var#, #newdata = data1, wt ="cells"
  ) %>%
    rename(value = !!sym(var)) %>%  # Rename the grouping variable to "value"
    mutate(term = var)  # Add a column specifying the variable
})


ATEs_mock = data |>
  cj(ccd_chosen_rw ~ interacted_cross3,
     id = ~respid,
     estimate = "mm")

ATEs_mock$lower99 = NA
ATEs_mock$upper99 = NA
ATEs_mock$lower90 = NA
ATEs_mock$upper90 = NA

ATEs = ATEs_mock

mm = marginal_means_list

mm <- bind_rows(mm)

for(i in unique(mm$value))
{
  #ATEs[ATEs$level == i, ]$estimate = mm[mm$value == i, ]$estimate
  ATEs[ATEs$level == i, ]$std.error = mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$lower = mm[mm$value == i, ]$conf.low
  ATEs[ATEs$level == i, ]$upper = mm[mm$value == i, ]$conf.high
  
  ATEs[ATEs$level == i, ]$lower90 = mm[mm$value == i, ]$estimate - z_alpha_90*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper90 = mm[mm$value == i, ]$estimate + z_alpha_90*mm[mm$value == i, ]$std.error
  
  ATEs[ATEs$level == i, ]$lower99 = mm[mm$value == i, ]$estimate - z_alpha_99*mm[mm$value == i, ]$std.error
  ATEs[ATEs$level == i, ]$upper99 = mm[mm$value == i, ]$estimate + z_alpha_99*mm[mm$value == i, ]$std.error
}


saveRDS(ATEs, file = paste0(output_wd,"mm_clustSE_cross3.rds"))

## Attribute importance weights ####

conjoint_vars <- c("ccd_gender", "ccd_age","ccd_religion", 
                   "ccd_citysize","ccd_profession",
                   "ccd_consc","ccd_openness","ccd_neuroticism",
                   "ccd_restaurant","ccd_transport", "ccd_pet"
)


importances = data.frame(Attribute=tools::toTitleCase(gsub("ccd_", "", conjoint_vars)),
                         AIC=rep("",11), Pseudo_R2=rep("",11),
                         mcfadden_R2=rep("",11))

for(i in 1:length(conjoint_vars))
{
  
  variable = conjoint_vars[i]
  
  formula = as.formula(paste("ccd_chosen_rw ~", variable))
  
  design <- svydesign(
    ids = ~ ccd_country + respid,  # Clustered standard errors by country & respondent
    data = data  # Use the dataset
  )
  
  model_svy <- svyglm(
    formula,
    design = design
  )
  
  
  importances$AIC[i] = model_svy$aic
  
  importances$Pseudo_R2[i] = 1-model_svy$deviance/model_svy$null.deviance
  
  importances$mcfadden_R2[i] = r2(model_svy, type = "mcfadden_adj")[1]
  
  
  ####let's normalize them
}

importances$AIC = as.numeric(importances$AIC)
importances$Pseudo_R2 = as.numeric(importances$Pseudo_R2)

max_aic = max(importances$AIC)
min_aic = min(importances$AIC)

max_Pseudo_R2 = max(importances$Pseudo_R2)
min_Pseudo_R2 = min(importances$Pseudo_R2)


###Compute the global pseudo rsquared 
model_svy <- svyglm(
  formula = ccd_chosen_rw ~ ccd_gender+
    ccd_age+ccd_religion+ccd_citysize+ccd_profession+
    ccd_consc+ccd_openness+ ccd_neuroticism+
    ccd_restaurant+ccd_transport+ccd_pet,
  design = design
)

total_Rsquared = 1-model_svy$deviance/model_svy$null.deviance


importances$contribution_R2 = importances$Pseudo_R2/total_Rsquared

importances_tot = importances
importances_tot$country = "POOL"

# I now repeat for each country

for(country in c("CZ", "FR", "IT", "SW"))
{
  
  
  importances = data.frame(Attribute=tools::toTitleCase(gsub("ccd_", "", conjoint_vars)),
                           AIC=rep("",11), Pseudo_R2=rep("",11),
                           mcfadden_R2=rep("",11))
  
  design <- svydesign(
    ids = ~ respid,  # Clustered standard errors by country & respondent
    data = data[data$ccd_country == country, ]  # Use the dataset
  )
  
  for(i in 1:length(conjoint_vars))
  {
    
    variable = conjoint_vars[i]
    
    formula = as.formula(paste("ccd_chosen_rw ~", variable))
    
    model_svy <- svyglm(
      formula,
      design = design
    )
    
    
    importances$AIC[i] = model_svy$aic
    
    importances$Pseudo_R2[i] = 1-model_svy$deviance/model_svy$null.deviance
    
    importances$mcfadden_R2[i] = r2(model_svy, type = "mcfadden_adj")[1]
    
    
    ####let's normalize them
  }
  
  importances$AIC = as.numeric(importances$AIC)
  importances$Pseudo_R2 = as.numeric(importances$Pseudo_R2)
  
  max_aic = max(importances$AIC)
  min_aic = min(importances$AIC)
  
  max_Pseudo_R2 = max(importances$Pseudo_R2)
  min_Pseudo_R2 = min(importances$Pseudo_R2)
  
  model_svy <- svyglm(
    formula = ccd_chosen_rw ~ ccd_gender+
      ccd_age+ccd_religion+ccd_citysize+ccd_profession+
      ccd_consc+ccd_openness+ ccd_neuroticism+
      ccd_restaurant+ccd_transport+ccd_pet,
    design = design
  )
  
  total_Rsquared = 1-model_svy$deviance/model_svy$null.deviance
  
  
  importances$contribution_R2 = importances$Pseudo_R2/total_Rsquared
  
  importances$country = country
  
  importances_tot  = rbind(importances_tot, importances)
}

saveRDS(importances_tot, file = paste0(output_wd,"importance_weights.rds"))








