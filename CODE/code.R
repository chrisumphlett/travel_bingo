library(tidyverse)
library(ggimage)

bingo_items <- read_csv("DATA/bingo_items.csv") %>%
  mutate(id = sample(nrow(.)), # randomly assign row numbers 
         x = ceiling(id / 5), # assign to one of 5 columns
         y = id %% 5, #assign to one of 5 rows
         image_url = if_else(id == 12, NA_character_, image_url), # middle square should have no image
         free = if_else(id == 12, "FREE", NA_character_)) %>% # middle square should say FREE
  filter(id <= 25) # keep only 25 of the items

ggplot(bingo_items, aes(x = x, y = y)) +
  geom_text(aes(label = free)) +
  geom_image(aes(image = image_url), size = 0.15) +
  scale_x_continuous(limits = c(0.5, 5.5)) +
  scale_y_continuous(limits = c(-0.8, 4.8)) +
  geom_vline(aes(xintercept = 4.5)) + 
  geom_vline(aes(xintercept = 1.5)) +
  geom_vline(aes(xintercept = 2.5)) +
  geom_vline(aes(xintercept = 3.5)) +
  geom_hline(aes(yintercept = 0.5)) + 
  geom_hline(aes(yintercept = 1.5)) +
  geom_hline(aes(yintercept = 2.5)) +
  geom_hline(aes(yintercept = 3.5)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=3)) +
  ggtitle("Travel Bingo\n")
  
