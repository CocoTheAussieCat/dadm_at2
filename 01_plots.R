### PACKAGE AND LIBRARY INSTALLATION ----------------------------------------
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

### PLOT FORMATTING -----------------------------------------------------------
dog_pal <- c('#b2182b','#d6604d','#f4a582','#fddbc7','#e0e0e0','#bababa','#878787','#4d4d4d')

dog_theme <- theme_minimal() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, face = 'italic', hjust = 0))

plot_width = 6
plot_height = plot_width*6/9

ama_source <- 'Source: Pets in Australia: National survey of pets and people, 2019'

### DOG SPEND PLOT ----------------------------------------------------------
spend_org <- read_xlsx(here::here('data', 'dog_spend.xlsx'))

spend <- spend_org %>%
  mutate(Category = as_factor(Category),
         Category = fct_lump(Category, 10, 'Other')) %>% 
  group_by(Category) %>% 
  arrange(desc(Spend)) %>% 
  glimpse()

spend %>%
  ggplot(aes(reorder(Category, Spend), Spend)) +
  geom_segment(aes(yend = 0, xend = Category), colour = dog_pal[2]) +
  geom_point(colour = dog_pal[2], size = 2) +
  geom_text(aes(label = round(Spend, 0)), nudge_y = 25, size = 3) +
  labs(title = 'Spending per dog per year (A$)',
       x = "",
       y = "",
       caption = ama_source) +
  coord_flip() +
  dog_theme +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank()) +
  ggsave(here::here('plots', 'dog_spend.png'), width = plot_width, height = plot_height)

  

  