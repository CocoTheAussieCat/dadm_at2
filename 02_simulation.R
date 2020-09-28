# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue",
                     "patchwork",
                     "readxl", 
                     "glue")

# Call libraries
for (package in package_list) {
  library(package, character.only = T)
}

# Clean up
rm(package_list, package)


## PLOT FORMATTING ------------------------------------------------------------
dog_pal <- c('#fef0d9','#fdcc8a','#fc8d59','#d7301f')

### FUNCTIONS -----------------------------------------------------------------
invTriangleCDF <- function(P, value_min, value_ml, value_max){
  
  # returns value of outcome from triangular distribution given probability
  # Inputs  - P = probability, float between 0 and 1
  #         - value_min = min value  can take
  #         - value_ml = most likely value P can take
  #         - value_max = maximum value P can take
  # Returns - value
  
  P_value_ml <- (value_ml-value_min)/(value_max-value_min)
  
  value <- ifelse(P < P_value_ml,
                  value_min + sqrt(P*(value_ml-value_min)*(value_max-value_min)),
                  value_max - sqrt((1-P)*(value_max-value_ml)*(value_max-value_min)))
  
  return(value)
}

SimulateVariables <- function(assumptions) {
  # Simulates all assumptions for one year
  # Inputs    - assumptions = assumptions as matrix
  # Outputs   - simulation = n rows of all assumptions randomly generated
  
  simulation <- as.data.frame(matrix(nrow=n, ncol=nrow(assumptions)))
  names(simulation) <- assumptions$Description
  
  for (i in 1:ncol(simulation)) {
    rand <- runif(n)
    simulation[, i] <- invTriangleCDF(rand, 
                                      value_min = assumptions$min[i],
                                      value_ml = assumptions$ml[i],
                                      value_max = assumptions$max[i])
  }
  
  
  # Add assumptions that do not have min-ml-max ranges
  simulation <- simulation %>% 
    mutate(apple_fee = fee_assumptions$value[1],
           google_fee = fee_assumptions$value[2],
           paypal_fixed = fee_assumptions$value[3],
           paypal_var = fee_assumptions$value[4],
           spend_per_service = fee_assumptions$value[5])
  
  return(simulation)
}

### ASSUMPTIONS ---------------------------------------------------------------

# Dogs in addressable market
dogs <- tribble(~dog_1, ~dog_2, ~dog_3, ~dog_4,
                     150648, 1933244, 3869760, 5114330)

# Total number of pet service providers in Aus
pet_cos <- 80000

# Revenue, cost and fee assumptions (all taken from Excel spreadsheet)
rev_assumptions <- read_xlsx(here::here('data', 'assumptions.xlsx'), sheet = 'revenue')

cost_assumptions <- read_xlsx(here::here('data', 'assumptions.xlsx'), sheet = 'costs')

fee_assumptions <- read_xlsx(here::here('data', 'assumptions.xlsx'), sheet = 'fees')


### SIMULATE VARIABLES --------------------------------------------------------

# Number of simulations
n = 10000

# Set seed for reproducible results
set.seed(120)

# Put all min-ml-max assumptions into matrix
assumptions <- rbind(rev_assumptions, cost_assumptions)

# Create randomly generated assumptions n times for each year
assumptions_1 <- SimulateVariables(assumptions)
assumptions_2 <- SimulateVariables(assumptions)
assumptions_3 <- SimulateVariables(assumptions)
assumptions_4 <- SimulateVariables(assumptions)

### YEARLY FINANCIAL MODELS ---------------------------------------------------

## Year 1 
# Only year with dev costs & google play fixed cost but no staff salaries
year_1 <- assumptions_1 %>% 
  mutate(year = 1,
         num_dogs = dogs$dog_1,
         service_providers = pet_cos * dogs$dog_1/dogs$dog_4,
         rev_b2c = num_dogs * app_penetration * spend_per_dog * 
           pct_spend_on_app * pct_service_fee,
         rev_listing = service_providers * service_penetration * listing_fee*12,
         rev_ad = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev = dev_cost,
         cost_deploy = cloud_cost + apple_fee + google_fee + domain_cost,
         cost_transactions = rev_b2c/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c * paypal_var,
         cost_marketing = social_ad + event_ad + store_ad,
         cost_staff = 0) %>% 
  mutate(rev = rev_b2c + rev_listing + rev_ad,
         cost = cost_dev + cost_deploy +cost_transactions
        + cost_marketing + cost_staff,
         gross_profit = rev-cost)

## Year 2 
# Staff = 1 sales, 1 senior dev
year_2 <- assumptions_2 %>% 
  mutate(year = 2,
         num_dogs = dogs$dog_2,
         service_providers = pet_cos * dogs$dog_2/dogs$dog_4,
         rev_b2c = num_dogs * app_penetration * spend_per_dog 
          * pct_spend_on_app * pct_service_fee,
         rev_listing = service_providers * service_penetration * listing_fee * 12,
         rev_ad = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev = 0,
         cost_deploy = (cloud_cost + apple_fee + google_fee + domain_cost) 
          * dogs$dog_2/dogs$dog_1,
         cost_transactions = rev_b2c/pct_service_fee/spend_per_service * paypal_fixed 
          + rev_b2c * paypal_var,
         cost_marketing = (social_ad + event_ad + store_ad) 
          * dogs$dog_2/dogs$dog_1,
         cost_staff = (sales_sal + sr_dev_sal)*1.095) %>% 
  mutate(rev = rev_b2c + rev_listing + rev_ad,
         cost = cost_dev + cost_deploy + cost_transactions 
          + cost_marketing + cost_staff,
         gross_profit = rev-cost)

## Year 3
# Staff = 1 sales, 1 head of sales, 1 senior dev, 1 junior dev
year_3 <- assumptions_3 %>% 
  mutate(year = 3,
         num_dogs = dogs$dog_3,
         service_providers = pet_cos * dogs$dog_3/dogs$dog_4,
         rev_b2c = num_dogs * app_penetration * spend_per_dog 
         * pct_spend_on_app * pct_service_fee,
         rev_listing = service_providers * service_penetration * listing_fee * 12,
         rev_ad = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev = 0,
         cost_deploy = (cloud_cost + apple_fee + google_fee + domain_cost) 
         * dogs$dog_3/dogs$dog_1,
         cost_transactions = rev_b2c/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c * paypal_var,
         cost_marketing = (social_ad + event_ad + store_ad) 
         * dogs$dog_3/dogs$dog_1,
         cost_staff = (sales_sal + sr_dev_sal + market_sal + jr_dev_sal)*1.095) %>% 
  mutate(rev = rev_b2c + rev_listing + rev_ad,
         cost = cost_dev + cost_deploy + cost_transactions 
         + cost_marketing + cost_staff,
         gross_profit = rev-cost)


## Year 4
# Staff = as for year 3
year_4 <- assumptions_4 %>% 
  mutate(year = 4,
         num_dogs = dogs$dog_4,
         service_providers = pet_cos * dogs$dog_4/dogs$dog_4,
         rev_b2c = num_dogs * app_penetration * spend_per_dog 
         * pct_spend_on_app * pct_service_fee,
         rev_listing = service_providers * service_penetration * listing_fee * 12,
         rev_ad = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev = 0,
         cost_deploy = (cloud_cost + apple_fee + google_fee + domain_cost) 
         * dogs$dog_4/dogs$dog_1,
         cost_transactions = rev_b2c/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c * paypal_var,
         cost_marketing = (social_ad + event_ad + store_ad) 
         * dogs$dog_4/dogs$dog_1,
         cost_staff = (sales_sal + sr_dev_sal + market_sal + jr_dev_sal)*1.095) %>% 
  mutate(rev = rev_b2c + rev_listing + rev_ad,
         cost = cost_dev + cost_deploy + cost_transactions 
         + cost_marketing + cost_staff,
         gross_profit = rev-cost)

### FINANCIAL MODEL -----------------------------------------------------------

# Join annual models
financials <- year_1[, c('year', 'rev', 'cost', 'gross_profit')] %>% 
  rbind(year_2[, c('year', 'rev', 'cost', 'gross_profit')]) %>% 
  rbind(year_3[, c('year', 'rev', 'cost', 'gross_profit')]) %>% 
  rbind(year_4[, c('year', 'rev', 'cost', 'gross_profit')])


### FUNCTIONISING

# Year 4 rev PDF
RevPDFPlot <- function(df, yr) {
  # Creates PDF of revenue for selected year
  # Inputs  - df = financials dataframe
  # Outputs - plots save to plots folder
  
  description <- case_when(yr == 1 ~'Inner Sydney',
                           yr == 2 ~'Sydney + Melbourne',
                           yr == 3 ~'All Major Cities',
                           yr == 4 ~'Australia-Wide')
  
  plot_title = glue('Year {year} Revenue ({desc})', year = yr, desc = description)
  file_name = glue('rev_{year}.png', year = yr)
  
  df %>% 
    filter(year == yr) %>% 
    ggplot() +
    geom_density(aes(rev/10^6), colour = dog_pal[4], fill = dog_pal[4], alpha = 0.50) +
    labs(title = plot_title,
         x = 'A$ million',
         y = '') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_x_continuous(limits = c(0, 10), breaks = c(seq(0, 10, 2))) +
    theme(panel.grid.minor = element_blank()) +
    ggsave(here::here('plots', file_name), width = 8, height = 8*9/16)
}


rev_1 <- RevPDFPlot(financials, 1)
rev_2 <- RevPDFPlot(financials, 2)
rev_3 <- RevPDFPlot(financials, 3)
rev_4 <- RevPDFPlot(financials, 4)

# Patch together for report
rev_plots <- rev_1/rev_2 /rev_3 /rev_4
ggsave(here::here('plots', 'rev_pdf.png'), rev_plots, width = 8, height = 8)


CostPDFPlot <- function(df, yr) {
  # Creates PDF of revenue for selected year
  # Inputs  - df = financials dataframe
  # Outputs - plots save to plots folder
  
  description <- case_when(yr == 1 ~'Inner Sydney',
                           yr == 2 ~'Sydney + Melbourne',
                           yr == 3 ~'All Major Cities',
                           yr == 4 ~'Australia-Wide')
  
  plot_title = glue('Year {year} Costs ({desc})', year = yr, desc = description)
  file_name = glue('cost_{year}.png', year = yr)
  
  df %>% 
    filter(year == yr) %>% 
    ggplot() +
    geom_density(aes(cost/10^6), colour = dog_pal[3], fill = dog_pal[3], alpha = 0.50) +
    labs(title = plot_title,
         x = 'A$ million',
         y = '') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_x_continuous(limits = c(0, 3), breaks = c(seq(0, 3, 0.5))) +
    theme(panel.grid.minor = element_blank()) +
    ggsave(here::here('plots', file_name), width = 8, height = 8*9/16)
}


cost_1 <- CostPDFPlot(financials, 1)
cost_2 <- CostPDFPlot(financials, 2)
cost_3 <- CostPDFPlot(financials, 3)
cost_4 <- CostPDFPlot(financials, 4)

# Patch together for report
cost_plots <- cost_1/cost_2 /cost_3 /cost_4
ggsave(here::here('plots', 'cost_pdf.png'), cost_plots, width = 8, height = 8)

GPPDFPlot <- function(df, yr) {
  # Creates PDF of revenue for selected year
  # Inputs  - df = financials dataframe
  # Outputs - plots save to plots folder
  
  description <- case_when(yr == 1 ~'Inner Sydney',
                           yr == 2 ~'Sydney + Melbourne',
                           yr == 3 ~'All Major Cities',
                           yr == 4 ~'Australia-Wide')
  
  plot_title = glue('Year {year} Gross Profit ({desc})', year = yr, desc = description)
  file_name = glue('cost_{year}.png', year = yr)
  
  df %>% 
    filter(year == yr) %>% 
    ggplot() +
    geom_density(aes(gross_profit/10^6), colour = dog_pal[2], fill = dog_pal[2], alpha = 0.50) +
    labs(title = plot_title,
         x = 'A$ million',
         y = '') +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(), axis.text.y = element_blank()) +
    scale_x_continuous(limits = c(-1, 7), breaks = c(seq(-1, 7, 1))) +
    theme(panel.grid.minor = element_blank()) +
    ggsave(here::here('plots', file_name), width = 8, height = 8*9/16)
}


gp_1 <- GPPDFPlot(financials, 1)
gp_2 <- GPPDFPlot(financials, 2)
gp_3 <- GPPDFPlot(financials, 3)
gp_4 <- GPPDFPlot(financials, 4)

# Patch together for report
gp_plots <- gp_1/gp_2 /gp_3 /gp_4
ggsave(here::here('plots', 'gp_pdf.png'), gp_plots, width = 8, height = 8)

