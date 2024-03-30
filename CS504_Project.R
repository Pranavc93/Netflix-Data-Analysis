#set the working directory
getwd()
setwd("C:/Users/DELL/OneDrive/Desktop/CS504")
getwd()

#install the necessary packages
install.packages("tidyverse")
install.packages("plotly")
install.packages("lubridate")
install.packages("dplyr")

library('tidyverse')
library('plotly')
library('lubridate')
library('dplyr')

#Read the dataset
titles <- read.csv('netflix_titles.csv', header = TRUE, na.strings = c("","NA"))
head(titles)

#Data cleanup - Remove uniformative cloumns like show_id and description
titles <- subset(titles, select = -c(show_id, description))
head(titles)
#Count of missing values in each column
data.frame("variable"=c(colnames(titles)), "missing values count"=sapply(titles, function(x) sum(is.na(x))), row.names=NULL)
#Director, cast, country, release year, duration are having missing values
#Since rating is a categorical variable, with 14 different values this attribute can be filled on the basis of mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#Get the most frequently occurring value from the ratings column and replace the incorrect values in the ratings column
titles$rating[is.na(titles$rating)] <- getmode(titles$rating)
getmode(titles$rating)
view(titles)
titles$rating[titles$rating == '66 min'] <- 'TV-MA'
titles$rating[titles$rating == '74 min'] <- 'TV-MA'
titles$rating[titles$rating == '84 min'] <- 'TV-MA'
view(titles)
#For movies and TV shows which do not have a Director, Country and Cast, fill NAvalues with 'No Director', 'No Country', 'No Cast'
titles$director <- titles$director %>% replace_na('No Director')
titles$cast <- titles$cast %>% replace_na('No Cast')
titles$country <- titles$country %>% replace_na('No Country')
#There are 3 values in duration which are blank, but those values are present in the ratings column
titles$duration <- titles$duration %>% replace_na('74 min')
view(titles)
#Date value cannot be imputed in any way and we cannot drop those rows because we would lose data
titles$date_added <- titles$date_added %>% replace_na('Not Available')

#Export to CSV file
write.csv(titles, "C:\\Users\\DELL\\OneDrive\\Desktop\\CS504\\Titles_cleaned.csv", row.names=TRUE)
#Date Format
titles$date_added <- as.Date(titles$date_added, format = "%B %d, %Y")

#Count of missing values in each column
data.frame("variable"=c(colnames(titles)), "missing values count"=sapply(titles, function(x) sum(is.na(x))), row.names=NULL)

#Type of Content - Donut Chart
library(ggplot2)
content_type <- titles %>% group_by(type) %>% summarise(
  count = n()
)

fig1 <- plot_ly(content_type, labels = ~type, values = ~count,marker = list(colors = c("#bd3939", "#399ba3")))
fig1 <- fig1 %>% add_pie(hole = 0.6)
fig1 <- fig1 %>% layout(title = 'Amount Of Netflix Content By Type',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig1
#To find out the total amount of content produced by each country, strings in country variable and count the total occurence of each country on its own.
s <- strsplit(titles$country, split = ", ")
titles_countries <- data.frame(type = rep(titles$type, sapply(s, length)), country = unlist(s))
titles_countries$country <- as.character(gsub(",","",titles_countries$country))
view(titles_countries)

content_type <- na.omit(titles_countries) %>%
  group_by(country, type) %>%
  summarise(count = n())
w <- reshape(data=data.frame(content_type),idvar="country",
             v.names = "count",
             timevar = "type",
             direction="wide") %>% arrange(desc(count.Movie)) %>%
  top_n(10)
w
view(w)
names(w)[2] <- "count_movie"
names(w)[3] <- "count_tv_show"
w <- w[order(desc(w$count_movie+w$count_tv_show)),] 

fig2 <- plot_ly(w, x = w$country, y = ~count_movie, type = 'bar', name = 'Movie', marker = list(color = '#bd3939'))
fig2 <- fig2 %>% add_trace(y = ~count_tv_show, name = 'TV Show',marker = list(color = '#399ba3'))
fig2 <- fig2 %>% layout(xaxis=list(categoryorder = "array", categoryarray = w$country, title="Country"), yaxis = list(title = 'Amount of content'), barmode = 'stack', title = 'Top 11 Countries By The Amount Of The Produced Content')

fig2

#Growth in content over the years from 2008 to 2020
titles_by_date_full <- titles %>% group_by(date_added) %>% 
  summarise(added_today = n()) %>% 
  mutate(total_number_of_content = cumsum(added_today), type = "Total")

titles_by_date <- titles %>% group_by(date_added, type) %>% 
  summarise(added_today = n()) %>% 
  ungroup() %>% 
  group_by(type) %>%
  mutate(total_number_of_content = cumsum(added_today))

full_data <- rbind(as.data.frame(titles_by_date_full), as.data.frame(titles_by_date))

fig3 <- plot_ly(full_data, x = ~date_added, y = ~total_number_of_content, 
        mode = 'lines', type = 'scatter',
        color = ~type, colors = c("#bd3939",  "#9addbd", "#399ba3")) %>% 
  layout(yaxis = list(title = 'Count'), 
         xaxis = list(title = 'Date'), 
         title = "Growth in Content over the Years", margin = list(t = 54),
         legend = list(x = 100, y = 0.5))
fig3

#Which month to release new content
titles_month <- titles[, c("type","title","date_added")]
titles_month %>% mutate(date_added = ymd(date_added), Month = format_ISO8601(date_added, precision = "ym"))
                              

view(titles_month)
class(titles$date_added)
x_month <- format(titles$date_added, "%m")
x_month
titles_month <- cbind(titles_month, x_month)
view(titles_month)
class(x_month)

x_month <- as.numeric(x_month)
x_month_name <- month.name[x_month]
titles_month <- cbind(titles_month, x_month_name)
view(titles_month)
titles_month <- titles_month %>% group_by(x_month_name) %>% summarise(count_month = n())
view(titles_month)
titles_month <- drop_na(titles_month)
#Sort by ascending order of count
titles_month <- arrange(titles_month, -titles_month$count_month)
view(titles_month)

#Funnel chart to view content in each month
fig4 <- plot_ly() 
fig4 <- fig4 %>%
  add_trace(
    type = "funnel",
    y = titles_month$x_month_name,
    x =  titles_month$count_month)
fig4 <- fig4 %>%
  layout(yaxis = list(categoryarray = titles_month$x_month_name))

fig4
#Rating Distribution
titles_rating <- titles %>% group_by(titles$rating) %>% summarise(count = n())
view(titles_rating)

fig5 <- plot_ly()
fig5 <- fig5 %>%
  add_trace(
    type = 'bar',
    y = titles_rating$`titles$rating`,
    x = titles_rating$count
  )
fig5 %>% layout(yaxis = list(title = 'Ratings'), xaxis = list(title = 'Count of Content'), title = 'Frequency of TV Show and Movie Ratings')

#Genre distribution - Bubble Chart
titles_genre <- titles %>% group_by(titles$listed_in) %>% summarise(count =n())

fig6 <- plot_ly(titles_genre, x = ~titles_genre$`titles$listed_in`, y = ~count, text = ~titles_genre$`titles$listed_in`, type = 'scatter', mode = 'markers', color = ~count, colors = 'Reds',
               marker = list(size = ~count, opacity = 0.5))
fig6 <- fig6 %>% layout(title = 'Most Popular Genre',
                      xaxis = list(showgrid = FALSE),
                      yaxis = list(showgrid = FALSE))

fig6 <- fig6 %>% layout(xaxis = list(title = 'Genre'), yaxis = list(title = 'Count of Genres'))
fig6

#To find out the duration of the movies or TV Shows - Split the original data frame to separate TV shows from Movies
titles_movie <- titles[titles$type == 'Movie', ]
view(titles_movie)
titles_tv <- titles[titles$type == 'TV Show', ]
view(titles_tv)
#Generate graph for duration
mov_duration_cntry <- na.omit(titles_movie[titles_movie$type == "Movie",][,c("country", "duration")])

s_dur <- strsplit(mov_duration_cntry$country, split = ", ")
duration_full <- data.frame(duration = rep(mov_duration_cntry$duration,
                                           sapply(s_dur, length)),
                            country = unlist(s_dur))
duration_full$duration <- as.numeric(gsub(" min","", duration_full$duration))





titles_movie_subset <- duration_full[duration_full$country %in% 
                                        c("United States", "India", "United Kingdom",
                                          "Canada", "France", "Japan", "Spain", "South Korea",
                                          "Mexico", "Australia", "China", "Taiwan"),]
fig7 <- plot_ly(titles_movie_subset, y = ~duration, color = ~country, type = "box") %>%
  layout(xaxis = list(title = "Country"), 
         yaxis = list(title = 'Duration (in min)'),
         title = "Box-Plots of Movie Duration in Top 12 Countries", margin = list(t = 54),
         legend = list(x = 100, y = 0.5))
fig7  
#Similarly for TV Shows

titles_show_subset <- titles_tv[titles_tv$country %in%
                                 c("United States", "India", "United Kingdom",
                                   "Canada", "France", "Japan", "Spain", "South Korea",
                                   "Mexico", "Australia", "China", "Taiwan"),]
view(titles_show_subset)
titles_show_subset <- transform(titles_show_subset, titles_show_subset$duration = as.numeric(titles_show_subset$duration))

str(titles_show_subset)

fig8 <- plot_ly(titles_show_subset, y = ~duration, color = ~country, type = "box") %>%
  layout(xaxis = list(title = "Country"), 
         yaxis = list(title = 'Duration (Seasons)'),
         title = "Box-Plots of TV Show Duration in Top 12 Countries", margin = list(t = 54),
         legend = list(x = 100, y = 0.5))
fig8
#Flexdashboard
install.packages("flexdashboard", type = "source")
install.packages("flexdashboard")
library(flexdashboard)
install.packages("shiny")
library(shiny)

install.packages('rsconnect')

