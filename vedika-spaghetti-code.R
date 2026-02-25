#### EXPERIMENTAL GLMMS
library(tidyverse)
library(readxl)



########______DATA CLEANING (TEMP)_________________

## Read in data
turtle <- read_excel(here::here("data", "fishnet_illumination_raw.xlsx"))


## Need columns for: gillnet pair, biomass BPUE, biomass BPUE number, biomass CPUE, biomass CPUE number, species, treatment (control, illuminated), associated treatment value, Total secondary target fish biomass CPUE		Market value of total target fish value MVPUE		Haulback time per 100 m net	
turtle
cols <- names(turtle)
turtle_data <- turtle[-1,] # remove header

# Pull treatment columns (control are even, illum are odd)
control_cols <- seq(2, 28, by = 2) 
illum_cols <- seq(3, 29, by = 2) 

# Assign metric labs (control values)
metric_labs <- cols[control_cols]

# Create tidy data frame by looping through control and treatment groups 
turtle_tidy <- map2_dfr(control_cols, illum_cols, function(control, ill){ 
  metric <- cols[control] # metric name from named control column
  tibble(
    gillnet_pair <- rep(turtle_data[[1]],2), # return treatment groups twice... extract as data frame!!
    treatment <- rep(c("Control", "Illuminated"), each = nrow(turtle_data)), # assignment label for each row
    metric = metric, # treatment group label
    value = c(turtle_data[[control]], turtle_data[[ill]])
  ) 
}
)
  
# Take a look at data frame
glimpse(turtle_tidy)
head(turtle_tidy) 



# Separate to wide format (need separate columns for each metric)
turtle_tidy <- turtle_tidy %>%
  pivot_wider(names_from = metric, values_from = value)

# Update column names for glmm
turtle_tidy <- turtle_tidy %>% rename(turtle_biomass_bpue = "Loggerhead turtle biomass BPUE",  treatment = `treatment <- ...`, gill_net_pair = `gillnet_pair <- rep(turtle_data[[1]], 2)` )

# Write to csv
write_csv(turtle_tidy, "data/fishnet_illumination_wide_vedika.csv")
  
########_______EXPERIMENT WITH GLMM________________
library(mgcv)
library(glmmTMB)
# Update column names
## authors mention using bpue turtle as response, treatment group is fixed effect, pair id random effect
## distribution is tweedie, family is log

## Really good resource for fitting random effects!! https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
#ERROR: names(dat) <- object$term : 
# 'names' attribute [1] must be the same length as the vector [0]
turtle_glmm <- gam(turtle_biomass_bpue ~ treatment + s(gill_net_pair, bs = "re"), 
                   method = "REML", 
                   #family = tweedie(link = "log"),
                   family = tw(),
                   data = turtle_tidy)

## Robustness checks!! 
# Overdispersion? 
# ICC? 

?tw()
