library(rsample)
library(UpSetR)
library(dplyr)

# https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html
# https://cran.r-project.org/web/packages/UpSetR/vignettes/queries.html

# NOTE YOU MUST PASS UPSET() A DATAFRAME!  IT WON'T WORK ON TIBBLES

# need to create category dummies to feed into upset()
starwars %>% count(species, eye_color) %>% spread(key = eye_color, value = n) %>%
        map2_dfc(.x = ., .y = names(.), 
                 .f = ~ tibble(!!sym(.y) := .x) %>% mutate(!!sym(.y) := ifelse(is.na(!!sym(.y)), 0, !!sym(.y)))) %>%
        data.frame() %>% upset(.)

# can use unite() to merge variables and create broader groupings
starwars %>% unite(col = skin_eye_color, skin_color, eye_color) %>% 
        count(species, skin_eye_color) %>% spread(key = skin_eye_color, value = n) %>%
        map2_dfc(.x = ., .y = names(.), .f = ~ tibble(!!sym(.y) := .x) %>% 
                         mutate(!!sym(.y) := ifelse(is.na(!!sym(.y)), 0, !!sym(.y)))) %>%
        data.frame() %>% upset(.)


#################################################################################################################
#################################################################################################################
#################################################################################################################


head(attrition)

attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% upset()

# add text.scale
# text.scale format is c(intersection size title, intersection size tick labels, set size title, set size tick labels, 
# set names, numbers above bars)
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", 
              text.scale = c(1.3, 1.3, 1, 1, 2, 0.75)) 

# order by freq
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = "freq")

# order by degree and freq (not well represented with attrition data, also seems to order by freq then by degree, despite connotation otherwise)
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = c("degree", "freq"))

# specify sets (could do the same with filter of initial tbl)
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., sets = c("college", "sales"), point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = "freq")

# pick n largest sets
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., nsets = 2, point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = "freq")

# group by sets, which groups together all sets including each given set in turn
# can also use cutoff to specify the max # of intersections to show involving each set
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        upset(data = ., group.by = "sets", cutoff = 3, point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = "freq")

# can also show empty intersections
attrition %>% mutate(male = ifelse(Gender == "Male", 1, 0),
                     college = ifelse(Education == "College", 1, 0),
                     sales = ifelse(Department == "Sales", 1, 0)) %>% select(male, college, sales) %>% data.frame() %>% 
        mutate(sales = ifelse(male == 1, 1, 0)) %>%
        upset(data = ., point.size = 3.5, line.size = 2, 
              mainbar.y.label = "Set Intersections", sets.x.label = "Sets of interest", order.by = "freq", empty.intersections = "on")


############################################################


movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
                   header = T, sep = ";")
head(movies)

# use intersects attribute to color certain intersections using query function
# note active parameter indicates whether frequency bar is colored, or whether colored icon goes atop frequency bar
upset(movies, queries = list(list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T), 
                             list(query = intersects, params = list("Drama"), color = "red", active = F), 
                             list(query = intersects, params = list("Action", "Drama"), active = T)))


# use elements attribute to visualize what share of frequency bars reflects a subpopulation
# must specify each element explicitly
movies %>% tabyl(AvgRating) %>% arrange(desc(n)) %>% head()
distinct_avg_ratings <- movies %>% tabyl(AvgRating) %>% arrange(desc(n)) %>% slice(1:100)
dim(distinct_avg_ratings)

# experimenting with different elements
upset(movies, queries = list(
        # list(query = elements, params = list("AvgRating", 3.5, 4.1), color = "blue", active = T), 
        # list(query = elements, params = list("AvgRating", 3, 3.58, 4, 2, 3.5, 3.25), color = "blue", active = T),
        list(query = elements, params = list("AvgRating", distinct_avg_ratings$AvgRating), color = "blue", active = T),
        # list(query = elements, params = list("ReleaseDate", 1980), color = "red", active = F),
        # list(query = elements, params = list("ReleaseDate", 1990), color = "green", active = F),
        list(query = elements, params = list("ReleaseDate", 1980, 1990, 2000), color = "red", active = F)))

# use expression attribute to subset data fed into upset plot (could do the same with filter of initial tbl)
upset(movies, queries = list(list(query = intersects, params = list("Action", "Drama"), active = T), 
                             list(query = elements, params = list("ReleaseDate", 1980, 1990, 2000), color = "red", active = F)), 
      expression = "AvgRating > 3 & Watches > 100")

# add query legend with option query.legend
# note that you can add query.name, or leave blank and get default query title
upset(movies, query.legend = "top", 
      queries = list(list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T, query.name = "Funny action"), 
                     list(query = intersects, params = list("Drama"), color = "red", active = F), 
                     list(query = intersects, params = list("Action", "Drama"), active = T, query.name = "Emotional action")))


# add boxplot summary
upset(movies, boxplot.summary = c("AvgRating", "ReleaseDate"))



