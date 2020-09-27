# PACKAGE AND LIBRARY INSTALLATION ------------------------------------------
package_list <- list("tidyverse",
                     "here",
                     "scales",
                     "RColorBrewer",
                     "glue",
                     "patchwork",
                     "readxl")

# Call libraries
for (package in package_list) {
  library(package, character.only = T)
}

# Clean up
rm(package_list, package)


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
  mutate(num_dogs_1 = dogs$dog_1,
         service_providers = pet_cos * dogs$dog_1/dogs$dog_4,
         rev_b2c_1 = num_dogs_1 * app_penetration * spend_per_dog * 
           pct_spend_on_app * pct_service_fee,
         rev_listing_1 = service_providers * service_penetration * listing_fee*12,
         rev_ad_1 = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev_1 = dev_cost,
         cost_deploy_1 = cloud_cost + apple_fee + google_fee + domain_cost,
         cost_transactions_1 = rev_b2c_1/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c_1 * paypal_var,
         cost_marketing_1 = social_ad + event_ad + store_ad,
         cost_staff_1 = 0) %>% 
  mutate(rev_1 = rev_b2c_1 + rev_listing_1 + rev_ad_1,
         cost_1 = cost_dev_1 + cost_deploy_1 +cost_transactions_1 
        + cost_marketing_1 + cost_staff_1,
         gross_profit_1 = rev_1-cost_1)

## Year 2 
# Staff = 1 sales, 1 senior dev
year_2 <- assumptions_2 %>% 
  mutate(num_dogs_2 = dogs$dog_2,
         service_providers = pet_cos * dogs$dog_2/dogs$dog_4,
         rev_b2c_2 = num_dogs_2 * app_penetration * spend_per_dog 
          * pct_spend_on_app * pct_service_fee,
         rev_listing_2 = service_providers * service_penetration * listing_fee * 12,
         rev_ad_2 = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev_2 = 0,
         cost_deploy_2 = (cloud_cost + apple_fee + google_fee + domain_cost) 
          * dogs$dog_2/dogs$dog_1,
         cost_transactions_2 = rev_b2c_2/pct_service_fee/spend_per_service * paypal_fixed 
          + rev_b2c_2 * paypal_var,
         cost_marketing_2 = (social_ad + event_ad + store_ad) 
          * dogs$dog_2/dogs$dog_1,
         cost_staff_2 = (sales_sal + sr_dev_sal)*1.095) %>% 
  mutate(rev_2 = rev_b2c_2 + rev_listing_2 + rev_ad_2,
         cost_2 = cost_dev_2 + cost_deploy_2 + cost_transactions_2 
          + cost_marketing_2 + cost_staff_2,
         gross_profit_2 = rev_2-cost_2)

## Year 3
# Staff = 1 sales, 1 head of sales, 1 senior dev, 1 junior dev
year_3 <- assumptions_3 %>% 
  mutate(num_dogs_3 = dogs$dog_3,
         service_providers = pet_cos * dogs$dog_3/dogs$dog_4,
         rev_b2c_3 = num_dogs_3 * app_penetration * spend_per_dog 
         * pct_spend_on_app * pct_service_fee,
         rev_listing_3 = service_providers * service_penetration * listing_fee * 12,
         rev_ad_3 = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev_3 = 0,
         cost_deploy_3 = (cloud_cost + apple_fee + google_fee + domain_cost) 
         * dogs$dog_3/dogs$dog_1,
         cost_transactions_3 = rev_b2c_3/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c_3 * paypal_var,
         cost_marketing_3 = (social_ad + event_ad + store_ad) 
         * dogs$dog_3/dogs$dog_1,
         cost_staff_3 = (sales_sal + sr_dev_sal + market_sal + jr_dev_sal)*1.095) %>% 
  mutate(rev_3 = rev_b2c_3 + rev_listing_3 + rev_ad_3,
         cost_3 = cost_dev_3 + cost_deploy_3 + cost_transactions_3 
         + cost_marketing_3 + cost_staff_3,
         gross_profit_3 = rev_3-cost_3)


## Year 4
# Staff = as for year 3
year_4 <- assumptions_4 %>% 
  mutate(num_dogs_4 = dogs$dog_4,
         service_providers = pet_cos * dogs$dog_4/dogs$dog_4,
         rev_b2c_4 = num_dogs_4 * app_penetration * spend_per_dog 
         * pct_spend_on_app * pct_service_fee,
         rev_listing_4 = service_providers * service_penetration * listing_fee * 12,
         rev_ad_4 = service_providers * service_penetration * pct_ad * ad_fee * 12) %>% 
  mutate(cost_dev_4 = 0,
         cost_deploy_4 = (cloud_cost + apple_fee + google_fee + domain_cost) 
         * dogs$dog_4/dogs$dog_1,
         cost_transactions_4 = rev_b2c_4/pct_service_fee/spend_per_service * paypal_fixed 
         + rev_b2c_4 * paypal_var,
         cost_marketing_4 = (social_ad + event_ad + store_ad) 
         * dogs$dog_4/dogs$dog_1,
         cost_staff_4 = (sales_sal + sr_dev_sal + market_sal + jr_dev_sal)*1.095) %>% 
  mutate(rev_4 = rev_b2c_4 + rev_listing_4 + rev_ad_4,
         cost_4 = cost_dev_4 + cost_deploy_4 + cost_transactions_4 
         + cost_marketing_4 + cost_staff_4,
         gross_profit_4 = rev_4-cost_4)

### FINANCIAL MODEL -----------------------------------------------------------

financials <- year_1[, c('rev_1', 'cost_1', 'gross_profit_1')] %>% 
  cbind(year_2[, c('rev_2', 'cost_2', 'gross_profit_2')]) %>% 
  cbind(year_3[, c('rev_3', 'cost_3', 'gross_profit_3')]) %>% 
  cbind(year_4[, c('rev_4', 'cost_4', 'gross_profit_4')])


# Year 4 rev PDF
financials %>% 
  ggplot() +
  geom_density(aes(rev_4/10^6), colour = 'blue', fill = 'blue', alpha = 0.5) +
  labs(title = 'Year 4 Revenue (Australia wide roll out)',
       x = 'A$ million',
       y = 'Density function') +
  theme_minimal() +
  # scale_x_continuous(limits = c(20, 38), breaks = c(seq(20, 38, 1))) +
  # scale_y_continuous(limits = c(0, 0.2), breaks = c(seq(0, 0.2, 0.05))) +
  theme(panel.grid.minor = element_blank()) +
  ggsave(here::here('plots', 'rev_4.png'), width = 8, height = 8*9/16)

# Year 4 gross profit PDF
financials %>% 
  ggplot() +
  geom_density(aes(gross_profit_4/10^6), colour = 'darkgreen', fill = 'darkgreen', alpha = 0.5) +
  labs(title = 'Year 4 Gross Profit (Australia wide roll out)',
       x = 'A$ million',
       y = 'Density function') +
  theme_minimal() +
  # scale_x_continuous(limits = c(20, 38), breaks = c(seq(20, 38, 1))) +
  # scale_y_continuous(limits = c(0, 0.2), breaks = c(seq(0, 0.2, 0.05))) +
  theme(panel.grid.minor = element_blank()) +
  ggsave(here::here('plots', 'gp_4.png'), width = 8, height = 8*9/16)

