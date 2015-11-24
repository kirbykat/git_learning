require("dplyr")
gapminder<-read.csv("data/gapminder-FIveYearData.csv")

#without dplyr
mean(gapminder$lifeExp[gapminder$continent=="Asia"])

#Select - selects a subset of columns
#Filter - selects a subset of rows

#group_by + summarize
#group_by - splits dataframe into multiple parts (or "grouped dataframes") (e.g. by continent)
#summarize - can then apply any function in R to all "grouped dataframes" e.g., to all continents...

#Pipes #Shortcut for pipe in R studio Ctrl + Shift + M

means_by_continent <- gapminder %>% 
  group_by(continent) %>% #in  a pipe, knows what data to use based on previous row
  summarize(mean_lifeexp=mean(lifeExp),
            mean_gdp=mean(gdpPercap))

means_by_continent <- gapminder %>% 
  group_by(continent,year) %>% #note, here is grouping by continent AND year
  summarize(mean_lifeexp=mean(lifeExp),
            mean_gdp=mean(gdpPercap))


meanpop_by_continent<-gapminder %>%
  group_by(country)%>%
  summarize(mean_pop=mean(pop))


means_by_continent <- gapminder %>% 
  group_by(continent) %>% 
  summarize(mean_lifeexp=mean(lifeExp),
            stdev_lifeexp=sd(lifeExp), #here, also calculating sd
            se_lifeexp=sd(lifeExp)/sqrt(n()), #standard error
            mean_gdp=mean(gdpPercap),
            stdev_gdp=sd(gdpPercap),
            se_gdp=sd(gdpPercap)/sqrt(n()))

#using mutate
means_by_continent <- gapminder %>% 
  mutate(gdp_billion=gdpPercap*pop/10^9) %>% #allows you to recalculate/rename a variable within a pipe, which may allow you to save on ram
  group_by(continent) %>%
  summarize(mean_lifeexp=mean(lifeExp),
            mean_gdp_billions=mean(gdp_billion))


#using mutate AFTER group by + summarize
means_by_continent2 <- gapminder %>% 
  group_by(continent) %>%
  summarize(mean_lifeexp=mean(lifeExp),
            mean_gdp=mean(gdpPercap),
            mean_pop=mean(pop)) %>%
            mutate(gdp_billion=mean_gdp/mean_pop*10^9) #NOTE, gives different answer, may not be technically correct

#Average life exp. in 2002 for two randomly selected countries from each continent,
#new functions
#sample_n
#arrange

means_special <- gapminder %>% 
  filter(year==2002) %>%
  group_by(continent) %>%
  sample_n(2) %>%
  summarize(mean_lifeexp=mean(lifeExp)) %>%
  arrange(desc(continent))
  
means_special <- gapminder %>% 
  filter(year==2002) %>%
  group_by(continent) %>%
  sample_n(2) %>%
  summarize(mean_lifeexp=mean(lifeExp)) %>%
  arrange(desc(continent))

means_special2 <- gapminder %>% 
  filter(year==2002) %>%
  group_by(continent,country) %>%
  summarize(mean_lifeexp=mean(lifeExp)) %>%
  group_by(continent) %>%
  arrange(desc(continent,country)) %>%
  sample_n(2) 
  

