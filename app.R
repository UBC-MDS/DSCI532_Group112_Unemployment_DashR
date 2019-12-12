library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashTable)
library(tidyverse)
library(scales)
library(plotly)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Load the data
unemply_df_year <- read_csv("data/unemply_df_year.csv")
unemply_df_month <- read_csv("data/unemply_df_month.csv")

# Some wrangling
df <- unemply_df_year %>%
  select(-c(count, rate, X1))

df <- df  %>%
  spread(key = 'year', value = 'total')

######### SELECTION COMPONENTS ################

#Range year slider (get the years from the dataset to make ticks on the slider)
yearMarks <- map(unique(unemply_df_year$year), as.character)
names(yearMarks) <- unique(unemply_df_year$year)
yearRangeSlider <- dccRangeSlider(
  id = 'yearrange',
  marks = yearMarks,
  min = 2000,
  max = 2010,
  step=1,
  value = list(2003, 2005)
)

# Select the single year for plot 3
yearSlider <- dccSlider(
  id = 'year',
  marks = yearMarks,
  min = 2000,
  max = 2009,
  value = 2000
)

#Dropdown to select industries interested 
all_industries <- unique(unemply_df_year$industry)
industryDropdown <- dccDropdown(
  id = 'industry',
  options = map(
    unique(unemply_df_year$industry), function(x){
    list(label=x, value=x)
  }),
  value = unique(unemply_df_year$industry), #Selects all by default
  multi = TRUE
)

#Radio button to select statistic type
statRadioButton <- dccRadioItems(
  id = 'stat',
  options=list(
    list(label = "Rate", value = "rate"),
    list(label = "Count", value = "count")
  ),
  value = "rate"
)

# Plot 1

make_plot_1 <- function(year_range = c(2003, 2005), stat = "rate"){  
  new_df <- df %>%
    select(industry)
  if(stat == "rate"){
    new_df$rate <- unlist(round((df[as.character(year_range[2])] - 
                                   df[as.character(year_range[1])]) / df[as.character(year_range[1])], 2))
    new_df <- new_df %>%
      mutate(colour = ifelse(rate < 0, "type1", "type2"))
    
    p <- ggplot(new_df, aes(industry, rate, colour = colour)) + 
      geom_segment(aes(xend = industry, y = 0, yend = rate)) +
      geom_point(size = 2) + 
      coord_flip() + 
      scale_y_continuous(labels = percent_format(accuracy = 1L)) +
      labs(x = '', y = 'Percentage Change')+
      theme_bw() + 
      theme(legend.position = "none")   
  } else {
    new_df$count <- unlist(round((df[as.character(year_range[2])] - df[as.character(year_range[1])])))
    new_df <- new_df %>%
      mutate(colour = ifelse(count < 0, "type1", "type2"))
    
    p <- ggplot(new_df, aes(industry, count, colour = colour)) + 
      geom_segment(aes(xend = industry, y = 0, yend = count)) +
      geom_point(size = 2) + 
      coord_flip() +
      labs(x = ' ', y = 'Absolute Change')+
      theme_bw() + 
      theme(legend.position = "none") 
  }
  gp <- ggplotly(p, width = 800, height = 500) %>%
    config(displayModeBar = FALSE)
  return(gp)
}

# Plot 2

make_plot_2 <- function(industries = all_industries, stat = "rate"){
  new_df <- unemply_df_year %>%
    filter(industry %in% industries)
  if(stat == "rate"){
    p <- ggplot(new_df, aes(factor(year), rate, colour = industry, group = industry)) + 
      geom_line() + 
      geom_point() + 
      scale_y_continuous(labels = percent_format(accuracy = 1L)) + 
      labs(x = '', y = 'Rate', colour = 'Industry') +
      theme_bw()
  } else {
    p <- ggplot(new_df, aes(factor(year), count, colour = industry, group = industry)) + 
      geom_line() + 
      geom_point() + 
      labs(x = '', y = 'Count', colour = 'Industry') +
      theme_bw()
  }  
  gp <- ggplotly(p, width = 800, height = 500) %>%
    config(displayModeBar = FALSE)
  return(gp)
}

# Plot 3

make_plot_3 <- function(industries = all_industries, year_desired = 2000, stat = "rate"){
  avg_df <- unemply_df_month %>%
    group_by(month) %>%
    summarize(rate = mean(rate),
              count = mean(count))
  
  new_df <- unemply_df_month %>%
    filter(year == year_desired,
           industry %in% industries)

  if(stat == "rate"){
    p <- ggplot(new_df, aes(factor(month), rate, colour = industry, group = industry)) + 
      geom_line() + 
      geom_point() +
      scale_y_continuous(labels = percent_format(accuracy = 1L)) + 
      scale_x_discrete(breaks = seq_along(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      labs(x = '', y = 'Rate', colour = 'Industry') +
      theme_bw()
  } else {
    p <- ggplot(new_df, aes(factor(month), count, colour = industry, group = industry)) + 
      geom_line() + 
      geom_point() + 
      scale_x_discrete(breaks = seq_along(1:12), labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
      labs(x = '', y = 'Count', colour = 'Industry') +
      theme_bw()
  }
  gp <- ggplotly(p, width = 800, height = 500) %>%
    config(displayModeBar = FALSE)
  return(gp)
}

# Create tabs and put graph in each tab

tabs <- dccTabs(id='tabs', value='tab-1', children=list(
  dccTab(label='Job Growth Across Industries', value='tab-1'),
  dccTab(label='Unemployment Throughout The Years', value='tab-2'),
  dccTab(label='Seasonal Unemployment', value='tab-3')
))

graph1 <- dccGraph(
  id = 'tab1-graph',
  figure = make_plot_1()
)
graph2 <- dccGraph(
  id = 'tab2-graph',
  figure = make_plot_2()
)
graph3 <- dccGraph(
  id = 'tab3-graph',
  figure = make_plot_3()
)

#Contents for each tab

content1 <- htmlDiv(list(
    htmlIframe(height=5, width=10, style=list(borderWidth = 0)), #space
    graph1,
    #selection components go here
    htmlLabel('Select a year range:'),
    yearRangeSlider,
    htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
    htmlLabel('Select a statistic:'),
    statRadioButton,
    #end selection components
    htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
    dccMarkdown("[Data Source](https://observablehq.com/@randomfractals/vega-datasets)")
  ))

content2 <- htmlDiv(list(
  htmlIframe(height=5, width=10, style=list(borderWidth = 0)), #space
  graph2,
  #selection components
  htmlIframe(height=15, width=10, style=list(borderWidth = 0)), #space
  htmlLabel('Select a statistic:'),
  statRadioButton,
  htmlLabel('Select industries:'),
  industryDropdown,
  #end selection components
  htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
  dccMarkdown("[Data Source](https://observablehq.com/@randomfractals/vega-datasets)")
))

content3 <- htmlDiv(list(
  htmlIframe(height=5, width=10, style=list(borderWidth = 0)), #space
  graph3,
  htmlIframe(height=15, width=10, style=list(borderWidth = 0)),
  #selection components go here
  htmlLabel('Select a statistic:'),
  statRadioButton,
  htmlLabel('Select industries:'),
  industryDropdown,
  htmlLabel('Select a year:'),
  yearSlider,
  #end selection components
  htmlIframe(height=20, width=10, style=list(borderWidth = 0)), #space
  dccMarkdown("[Data Source](https://observablehq.com/@randomfractals/vega-datasets)")
))

# Title features

pageTitle <- htmlH1(
  'Exploring Unemployment Across Industries',
  style = list(
    textAlign = 'center',
    color = '#00008b'
  )
)

pageSubTitle <- htmlH5(
  'These graphs display a framework for countries to examine their unemployment rates/counts across industries.',
  style = list(
    textAlign = 'center',
    color = '#00008b'
  )
)

markdown_text <- "
Text explaining more about the graph
"

app$layout(htmlDiv(list(
    htmlH1(pageTitle),
    htmlDiv(children = pageSubTitle),
    dccMarkdown(children=markdown_text),
    dccTabs(id="tabs", value='tab-1', children=list(
    dccTab(label='Job Growth Across Industries', value='tab-1'),
    dccTab(label='Unemployment Throughout The Years', value='tab-2'),
    dccTab(label='Seasonal Unemployment', value='tab-3')
  )),
  htmlDiv(id='tabs-content')
)))

#Callbacks

app$callback(output('tabs-content', 'children'),
             params = list(input('tabs', 'value')),
             function(tab){
               if(tab == 'tab-1'){
                 return(content1)}
               else if(tab == 'tab-2'){
                 return(content2)} 
               else if(tab == 'tab-3'){
                   return(content3)} 
             }
             
)
app$callback(
  # update figure of tab1-graph
  output=list(id = 'tab1-graph', property='figure'),
  
  # based on values of year, industries components
  params=list(input(id = 'yearrange', property='value'),
              input(id = 'stat', property='value')),
  
  # this translates your list of params into function arguments
  function(year_value, stat) {
    make_plot_1(year_value, stat)
  })

app$callback(
  # update figure of tab2-graph
  output=list(id = 'tab2-graph', property='figure'),
  
  # based on values of year, industries components
  params=list(input(id = 'industry', property='value'),
              input(id = 'stat', property='value')),
  
  # this translates your list of params into function arguments
  function(industry, stat) {
    make_plot_2(industry, stat)
  })

app$callback(
  # update figure of tab3-graph
  output=list(id = 'tab3-graph', property='figure'),
  
  # based on values of year, industries components
  params=list(input(id = 'industry', property='value'),
              input(id = 'year', property='value'),
              input(id = 'stat', property='value')),
  
  # this translates your list of params into function arguments
  function(industry, year, stat) {
    make_plot_3(industry, year, stat)
  })


app$run_server()

