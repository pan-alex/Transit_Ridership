# setwd('R/Transit_Ridership')
library(ggplot2)
theme_set(theme_bw())
library(lubridate)
library(tidyverse)
library(gganimate)

edmonton <- read.csv('Data/edmonton_ridership.csv')

edmonton <- edmonton %>%
    mutate(DateTime = mdy_hms(as.character(DateTime)), 
           Month = month(DateTime),
           DateTime = YEAR + Month / 12) %>%
    rename(Year = YEAR) %>%
    arrange(DateTime)

    
p1 <- ggplot(edmonton) +
    geom_path(aes(x = DateTime, y = MONTH_RIDERSHIP / 1000000,
                  cumulative = TRUE, frame = DateTime),    # gganimate-related
              alpha = 0.6, size = 0.8) +
    ylim(c(0, 8.5)) +
    # scale_x_date(date_breaks = "1 year") + 
    scale_x_continuous(breaks = seq(2005, 2017), minor_breaks = waiver()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab('') + ylab('Mean Monthly Ridership (millons)') +
    ggtitle('Edmonton Transit Service Monthly Ridership')
    
p1

# gganimate(p1, file = 'edmonton_datetime.gif', interval = 0.1, title_frame = FALSE)
gganimate(p1, interval = 0.1, title_frame = FALSE)


# Convert Month integers to ordered factors with the month name (e.g. 1 -> 'Jan')
edmonton$Month_name = factor(month.abb[edmonton$Month], 
                             levels = month.abb, ordered = T)
                                       
p2 <- ggplot(edmonton) +
    geom_line(aes(x = Month_name, y = MONTH_RIDERSHIP / 1000000, 
                  group = Year, col = Year,
                  cumulative = TRUE, frame = Year),    # gganimate-related
              alpha = 0.6) +
    geom_point(aes(x = Month_name, y = MONTH_RIDERSHIP / 1000000, 
                   group = Year, col = Year,
                   cumulative = TRUE, frame = Year),   # gganimate-related
               alpha = 0.6) +
    ylim(c(0, 8.5)) +
    xlab('') + ylab('Monthly Ridership (millions)') +
    ggtitle('Edmonton Transit Service Monthly Ridership')
    
# gganimate(p2, file = 'edmonton_by_year.gif', interval = 1)
gganimate(p2, interval = 1)
