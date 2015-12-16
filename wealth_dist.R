library(dplyr)
library(ggplot2)
library(tidyr)

dec_dist <- read.csv('Wealth Distribution Data.csv',
                     header = TRUE, sep = ',', stringsAsFactors = FALSE)

dec_dist2 <- dec_dist %>%
  select(-4, -10)

new_names <- 1:9
new_names <- new_names * 10
new_names <- sapply(new_names, paste, '%', sep = '')
new_names <- c('Country', new_names)

names(dec_dist2) <- new_names

# only full data countries
dec_dist_nona <- na.omit(dec_dist2)

# pivot data
pivoted <- dec_dist_nona %>%
  gather(decile, pct_pop, 2:length(names(dec_dist_nona))) %>%
  mutate(decile = as.numeric(gsub('%', '', as.character(decile))) / 100,
         pct_pop = pct_pop / 100)

# plot data (line plot)
ggplot(data = pivoted, aes(decile, pct_pop, group = Country))+
  stat_smooth(method = loess, se = FALSE, color = 'darkgray')+
  stat_smooth(data = pivoted[pivoted$Country == 'US',],
              method = loess, se = FALSE, size = 1,
              aes(decile, pct_pop, group = Country, color = 'red'))+
  scale_x_continuous(limits = c(0.1, 0.95),
                     breaks = seq(0.1, 0.9, 0.1))+
  xlab('Wealth Decile')+
  ylab('Proportion of Total Population')+
  geom_text(data = pivoted[pivoted$decile == 0.9,],
            aes(label = Country, hjust = 0),
            size = 3, color = 'darkgray')

# stacked bar chart by country
pivoted_ninety <- pivoted %>%
  filter(decile == '90%')

ggplot(data = pivoted_ninety)+
  geom_bar(aes(x = Country, y = pct_pop), stat = 'identity') +
  ylim(0, 100)
