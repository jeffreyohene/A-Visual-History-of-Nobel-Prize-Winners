# Loading in required libraries
library(tidyverse)

# Reading in the Nobel Prize data
nobel <- read_csv('datasets/nobel.csv')

# Taking a look at the first couple of winners
head(nobel,n=2)

# Counting the number of (possibly shared) Nobel Prizes handed
# out between 1901 and 2016
nobel %>%
summarize(total_prizes=n())

# Counting the number of prizes won by male and female recipients.
nobel %>%
  group_by(sex)%>%
  summarize(prizes_by_sex=n())


# Counting the number of prizes won by different nationalities.
nobel %>%
  group_by(birth_country)%>%
  summarize(prizes_by_nationality=n())%>%
  arrange(desc(
      prizes_by_nationality))%>%
  slice(1:20)
 

# Calculating the proportion of USA born winners per decade
prop_usa_winners <- nobel%>% 
    mutate(usa_born_winner = birth_country == 'United States of America',
           decade = floor(year / 10) * 10 )%>% 
    group_by(decade)%>%
    summarize(proportion= mean(usa_born_winner,
                               na.rm = TRUE))

# Display the proportions of USA born winners per decade
prop_usa_winners

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)


# Plotting USA born winners
ggplot(prop_usa_winners,
      aes(x=decade,
          y=proportion))+
       geom_point()+ 
         geom_line()+
         scale_y_continuous(labels=scales::percent)+
         labs(x='Decade',
             y='Proportion',
             title='Proportion of American Winners per Decade')+
         theme_bw()



# Calculating the proportion of female laureates per decade
prop_female_winners <- nobel %>%
    mutate(female_winner=sex=='Female',
          decade= 
           floor(year / 10) * 10 ) %>%
    group_by(decade,
             category) %>%
    summarize(proportion= 
              mean(female_winner,
                   na.rm = TRUE))

# Plotting the proportion of female laureates per decade
ggplot(prop_female_winners,
      aes(x=decade,
          y=proportion,color=category))+
       geom_point()+ 
         geom_line()+
         scale_y_continuous(labels=scales::percent)+
         labs(x='Decade',
             y='Proportion',
             title='Proportion of Female Winners by Category per Decade')+
         theme_bw()


# Picking out the first woman to win a Nobel Prize
nobel %>%
    filter(sex=='Female')%>%
    slice(1)


# Selecting the laureates that have received 2 or more prizes.
nobel %>%
    group_by(full_name) %>% 
    count()  %>% 
    filter(n >= 2)

# Loading the lubridate package
library(lubridate)

# Calculating the age of Nobel Prize winners
nobel_age <- nobel %>%
    mutate(age=year-year(birth_date))
    
ggplot(nobel_age,aes(year,age)) +  #Plotting age of winners
    geom_point() +
    geom_smooth() +
    labs(x='Year',
             y='Age',
             title='Age of Winner When They Won')+
    theme_bw()

# Same plot as above, but faceted by the category of the Nobel Prize
ggplot(nobel_age,aes(year,age)) +
    geom_point() +
    geom_smooth(se=FALSE) +
    facet_wrap(~category)+
    theme_bw()+
    labs(x='Year',
             y='Age',
             title='Age of Winners by Award Category')


# The oldest winner of a Nobel Prize as of 2016
nobel_age %>% 
  top_n(1,age)

# The youngest winner of a Nobel Prize as of 2016
nobel_age %>%
  top_n(1,desc(age))


# The name of the youngest winner of the Nobel Prize as of 2016
youngest_winner <- 'Malala Yousafzai'                                  
     