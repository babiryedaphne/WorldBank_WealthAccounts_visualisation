#Daphne Babirye Assignment MscDSA
#DATA VISUALIZATION ASSIGNMENT
#--------------------------------------------------------------
library(shiny)

# Installing required libraries
install.packages('shinythemes') # installing shiny themes
library(shinythemes)

library(tidyverse)# key library for data analysis and visualisation

# libraries for working with maps
library(maps)
library(mapproj)
library(maptools)
library(sf)

# working with dates

library(lubridate)

# installing packages 
library(gganimate)# for animation
library(tweenr)#to create smooth transitions of animations between states of data.
library(magick)#image processing
library(gifski)# creating gif
library(magrittr) #pipe operator
library(countrycode)
library(grid)#  flexibility in layout of graphics
#library(rtools)       

install.packages('plotly')# for animations
library(plotly)

devtools::install_github("ellisp/ggflags")
library(ggflags) # to include country flags. 

#Source: Kamil Szymkowski(2020), https://medium.com/@szymkowskidev/animated-bar-race-ranking-in-r-e63440f149da



#----------------------------------------------------------------
#Data Source

# Uploading data
library(readxl)

#Uploading CWON2021 Country Tool - Full Dataset (1995 -2018)
#Source: World Bank 2021

cwon <- read_xlsx('CWON2021 Country Tool - Full Dataset.xlsx',sheet = 'country', skip = 1,na = '..')

str(cwon)# checking the structure of variables

# 5,232rows and 34 variables
# the date column is numeric. It will be changed to date type



#---------------------------------------------------------------
########### Data Cleaning #################

#Dealing with missing values

library(VIM)

#Visualising missing values

cwon_mis <- aggr(cwon)


#counting missing values in each column of cwon

missing<- data.frame(sapply(cwon, function(x) sum(is.na(x))))
sum(is.na(cwon))# 17767 total number of missing values

library(xlsx)# to write the missing values data to an excel file

write.xlsx(missing,'missing_values.xlsx', row.names=TRUE)

#the countries without total wealth data were dropped.
# This is because didn't have data for one of the components of total wealth which is the basis for analysis

cwon <- cwon %>% 
  drop_na(totwealth)


# The new data frame has 3,556 observations
#-----------------------------------------------------------------------------
# Summary of the dataset
library(summarytools) # for summary and distributions of data
cwon_summary<-dfSummary(cwon,
                        graph.magnif = 0.75, 
                        na.col = FALSE, 
                        varnumbers = FALSE,  
                        valid.col = FALSE,  
                        report.title = "Summary of the Changing Wealh of Nations data set"
                        
)



# summary statistics 
summary(cwon)  # summary of the data

# descriptive statistics 
library(psych)# for describe function

cwon_stats <-describe(cwon)
write.xlsx(cwon_stats,'cwon_stats.xlsx')# save file to excel



#-------------------------------------------------------------
#CALCULATIONS

#calculating wealth per  capita. A new column has been created

cwon <-cwon %>%
  mutate(wealthPerCapita =totwealth/pop)

#computing proportion of total world wealth per country

cwon<-cwon %>%
  group_by(year)%>%
  mutate(w_share = (totwealth/sum(totwealth))*100)


#Calculating the annual country ranks in terms of total wealth
cwon <-cwon %>%
  group_by(year) %>%
  mutate(w_rank =dense_rank(desc(totwealth)))


# Adding country codes to use the ggflags  and maps in visualization

cwon$codes <- tolower(countrycode(cwon$wb_code, origin = 'wb', destination = 'iso2c'))

#Adding continent column (in lower case)

cwon$continent <- countrycode(cwon$codes, "iso2c", "continent")

#--------------------------------------------------------------------
# share of population 

cwon<-cwon%>%
  group_by(year)%>%
  mutate(pop_share = (pop/sum(pop))*100)


#----------------------------------------------------------------
#world Development indicators data
# to upload indicators like life expectancy, GDP and total world aggregates
install.packages('WDI')
library(WDI)

wdi_select <- WDI(country ='all',indicator = c('SP.POP.TOTL','NY.GDP.MKTP.KD','SP.DYN.LE00.IN','NY.GDP.PCAP.KD',
                                               'SP.POP.GROW', 'NY.GDP.MKTP.CD'),start = 1995, end = 2018 )

#change the country codes to lower case for easy merging with cwon dataset
wdi_select$iso2c <- tolower(wdi_select$iso2c)

# checking the structure
str(wdi_select)

#data summary
wdi_summary<-dfSummary(wdi_select,
                       graph.magnif = 0.75, 
                       na.col = FALSE, 
                       varnumbers = FALSE,  
                       valid.col = FALSE,  
                       report.title = "Summary of Selected World Bank Development Indicators"
                       
)

#summary statistics

wdi_stats <- describe(wdi_select)
write.xlsx(wdi_stats,'wdi_stats.xlsx')


# Checking for missing values 
wdiMissing<- data.frame(sapply(wdi_select, function(x) sum(is.na(x))))
# There  are very many missing values across the different variables

sum(is.na(wdi_select))

write.xlsx(wdiMissing,'wdiMissing.xlsx')# write to an excel file 

#Visualising missing values

wdi_mis <- aggr(wdi_select)#1577 missing values of 6,384 variables 

# Dealing with missing values

# Deleted with after joining the datasets


#------------------------------------------------------------------
# merging world development indicators and changing wealth of nations (cwon) dataset
cwon_merge <- cwon %>% inner_join(wdi_select,
                                  by = c('codes' = tolower('iso2c'), 'year' = 'year' )
)



#--------------------------------------------------------------
#Delete missing values
#  imputation might not provide the right picture of the data

sum(is.na(cwon_merge))#165 missing values

#checking variables with missing values

merge_mis <- data.frame(sapply(cwon_merge, function(x) sum(is.na(x))))


cwon_merge <- cwon_merge  %>% 
  drop_na()
# 3450 observations

# Descriptive statistics of cwon merged  dataset

cwon_merge_stats<- describe(cwon_merge)

#write file to excel
write.xlsx(cwon_merge_stats,'cwon_merge_stats.xlsx')


#-----------------------------------------------------------------
# world data (total wealth, population,GDP,life expectancy)

world_data <- subset(wdi_select, country == 'World')



# computing world total wealth by year.
cwon_world <- cwon%>%
  group_by(year)%>%
  summarise(world_wealth = sum(totwealth))

#combine wealth and development world indicators

world_indicators <- world_data %>% 
  inner_join(cwon_world,
             by = c('year' = 'year')
  )


write.xlsx(world_indicators, 'world_indicators.xlsx')
# limitations: WDI use USD 2015 constant prices while cwon uses US$ 2018 constant prices

#options(scipen = 999) # used in preventing R from displaying numbers in scientific format
#options(scipen = 0)

# import definitions table
def <- read.xlsx('definitions.xlsx', sheetIndex = 1)




#------------------------------------------------------------------------------
library(plotly)
# EXPLORATORY DATA ANALYSIS
###Wealth Composition 2018

# 1. Trasform wealth into a factor variable of its components
#compute the data required for the chart
totwealth_factors<- pivot_longer(cwon_select, cols = c(pk, nk,hc,nfa), names_to = 'totwealth_fc', values_to = 'totwealth_values')
propdata<- totwealth_factors%>%
  filter(year==2018)%>%
  group_by(totwealth_fc)%>%
  summarise(totwealth_values = sum(totwealth_values))

# Calculate proportions and format as percentages
propdata$proportion <- proportions(propdata$totwealth_values)

# multiply by 100 to get percentages
propdata$percentage <- round(propdata$proportion*100,2)

#create plot
propPlot <-ggplot(propdata, aes(x = "", y = totwealth_values, fill = totwealth_fc)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  
  #transform geom bar to pie chart
  coord_polar("y", start = 0) +
  
  #add title
  labs(title = "Total Wealth") +
  theme_void() +
  scale_fill_brewer(palette = "Dark2")

# Hc =63.6, pk =31.2,nk =5.6,nfa =-0.35


#save data to excel file
write.xlsx(propdata,'propdata.xlsx')

##---EDA.2 wealth by continent groups 2018
#1. Filter for 2018 data

boxdata<- cwon%>%
  filter(year==2018)#%>%

# Create a box plot using ggplot
ggplot(boxdata, aes(x = continent, y = wealthPerCapita, fill = continent)) +
  geom_boxplot() +
  #Labels
  xlab("Continent") +
  ylab("Wealth  Per Capita ") +
  scale_y_continuous(labels = scales::comma)

####################################################
#scatter plot of wealth per capita versus life expectancy 2018
scatter_eda <- cwon_merge%>%
  filter(year == 2018)%>%
  ggplot(aes(x=SP.DYN.LE00.IN,y=wealthPerCapita), )+
  geom_point(color = 'orangered')+
  xlab('Life Expectancy')+
  ylab('Wealth Per Capita')
scatter_eda  


#------------------------------------------------------------------------------
#BAR RACE CHART


barRace_data <- cwon %>%  
  select(wb_name, totwealth, year, continent, w_rank,codes) %>%
  group_by(year) %>%  
  filter(w_rank <= 15) %>%
  ggplot()+
  aes(xmin = 0 ,  
      xmax = totwealth) +  
  aes(ymin = w_rank - .45,  
      ymax = w_rank + .45,  
      y = w_rank) +  
  geom_rect(aes(fill = continent))+
  
  ##Including country names
  geom_text(x=-30,hjust='right', aes(label=wb_name))+
  scale_y_reverse()+
  
  #specify plot scale
  scale_x_continuous(limits = c(-100,400),breaks = as.integer(c(0, 100,200, 300,400) ))+
  
  #specify labels
  labs(title='Top 15 wealthiest countries', caption = 'Source: World Bank Group 2021',
       x='Total Wealth (US$ Million)', y = 'World Rank') +
  #including country flags
  geom_flag(aes(x = -15, country = codes), size = 10)+
  
  # specify position and color for the year text
  geom_text(x=300,y = -13,
            aes(label = as.character(year)),  
            size = 15, col = "black") +  
  aes(group = wb_name)+
  
  # removing background
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line.x = element_line(colour = "black"))+
  
  #specifiy the variable for transition in the animation 
  transition_time(year)

#animate plot
#animate(barRace_data, nframes = 100, fps = 20, width = 800, height = 500)

# save animation to www folder
#anim_save('www/barRace.gif',animation = last_animation())





# Source: https://evamaerey.github.io/little_flipbooks_library/racing_bars/racing_barcharts.html#32
#https://stackoverflow.com/questions/68150898/animated-bar-plot-not-working-displaying-on-r-shiny-web-app


# Additions i made versus what's in the source code:

# 1.added the continent from which the top countries come from and looked at the top 20 rather then 25
# 2.looking and wealth and not GDP
# 3.added country flags for better visualization

#SHINY APP

# shiny UI server
ui <- navbarPage("The Changing Wealth of Nations",
                 theme = shinytheme('united'),
                 tabPanel("Description",
                          fluidRow(
                            # paragraph explaining he Changing wealth of nations
                            h4(p("Wealth is a complementary indicator to Gross Domestic Product (GDP) for monitoring sustainable development in acountry.
                             It illustrates that development entails managing a portfolio of assets such as: natural, human, and produced capital.
                             Comprehensive national wealth therefore signals whether GDP growth can be sustained over the long run (World Bank 2021). ")),
                            
                          ),
                          fluidRow(
                            column(6, p(strong('Total Wealth Composition')),#heading
                                   
                                   # visual of the hanging wealth of nations concept
                                   img(src="Wealth.PNG", height=500, width=600),
                                   
                                   
                                   #caption
                                   h5('Source: World Bank 2021')
                            ),
                            
                            column(6, strong('Data Description'),
                                   
                                   # description of the dataset
                                   tableOutput('Definitions'),
                                   
                                   # caption
                                   h5('Source: World Bank 2021')
                                   
                            )
                          )
                          
                          
                 ),
                 navbarMenu("World Graphs", 
                            tabPanel("Maps ",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput('map_input',
                                                     label = 'Select Variable',
                                                     choices = names(cwon[c(8,9,10,26,33)]),
                                                     selected = names(cwon[8])
                                         )
                                         
                                       ),
                                       mainPanel(
                                         plotlyOutput('map')
                                       )
                                       
                                     ),
                            ),
                            tabPanel("Graphs",
                                     
                                     fluidRow(
                                       column(6,
                                              #Plot 1: Rank of top 15 wealthiest countries over time
                                              
                                              img(src="barchart.gif", align = "left",height='600px',width='600px')
                                              
                                              
                                       ),
                                       
                                       
                                       column(6,
                                              
                                              #Trend of World Wealth over time 
                                              plotOutput('trend_output')
                                              
                                       )
                                     )
                                     
                            )),
                 # income level graphs
                 tabPanel("Income Level",
                          fluidRow(
                            #column area 1 
                            column(6,
                                   
                                   #scatter plot 'GDPPer Capita, Wealth Per capita', Life Expectancy, INcome level
                                   plotlyOutput('ILplot1')
                            ),
                            # Column area 2
                            column(6,
                                   
                                   plotOutput('nkPlot'),
                                   
                                   selectInput('nkselect',
                                               label = 'Select Variable',
                                               choices = names(cwon[c(3,39)]),
                                               selected = names(cwon[3])
                                   )
                                   
                            )
                          )
                          
                 ),
                 
                 
                 # Continent/Regional Level Graphs
                 tabPanel("Continent/Region", 
                          fluidRow(
                            column(6,
                                   
                                   # Density Plot of distribution of wealth over time by continent() 
                                   plotOutput('r_wdist'),
                                   
                                   # Slider Input for density plot selecting year
                                   sliderInput('danimation',
                                               label = 'Year',
                                               min = 1995, max = 2018, 
                                               value = 1995, step= 1,
                                               animate = animationOptions(interval = 1000,loop = FALSE)#animate = TRUE,
                                   ),
                                   
                                   # Select Input: selection between continent and region
                                   selectInput('Density_data',
                                               label = 'Select Continent/Region',
                                               choices = names(cwon[c(4,39)]),
                                               selected = names(cwon[4]))
                                   
                            ),
                            # second column area
                            column(6,
                                   #scatter Plot of life expectancy, wealth and population
                                   plotlyOutput('ILscatterPlot'),
                                   
                                   
                                   
                                   # Select Input: selection between continent and region
                                   selectInput('Rscatter_data',
                                               label = 'Select Continent/Region',
                                               choices = names(cwon[c(4,39)]),
                                               selected = names(cwon[4]))
                                   
                            )
                            
                          )
                          
                 ),
                 
                 tabPanel("Country",
                          fluidRow(
                            #column showing country summary of country name,share, rank and growth in wealth 
                            column(2,
                                   fluidRow( strong(h4('Country Name:')), 
                                     
                                     div(h2(textOutput('country_name')),style = "color:orangered")
                                     
                                     
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                     selectInput('c_country',
                                                 label = 'Select Country',
                                                 choices = unique(cwon[1]),
                                                 selected = 'Ireland'
                                     ),
                                     
                                     )
                            ),
                            column(10,
                                   fluidRow(
                                     column(6,
                                            # Plot of wealth decomposition over time
                                            plotOutput('c_decomp')
                                            
                                     ),
                                     column(6,
                                            #' scatter plot of wealth share vs population share by country '
                                            plotlyOutput('c_share')
                                     )
                                     
                                     
                                     
                                   ),
                                   
                            ),
                            
                          ),
                          
                 )
                 # map of with color showing the wealth component with highest share 
                 #Show trend of wealth for countries by highest wealth component
                 # ahow the share,rank, growth, 
                 #scatter plot of wealth vs population over time(show country flags)
                 
                 
)

#server function
server <- function(input, output, session){
  
  #--------------------------------------------------------------------
  #DESCRIPTION OUTPUT
  
  output$Definitions <- renderTable({
    def
    
  })
  
  #-----------------------------------------------------------------------
  #WORLD OUTPUT:
  #Plot1: Maps
  
  output$map <- renderPlotly({
    
    #plot map
    plot_geo(data = cwon,
             locationmode = 'country names',
             frame = ~year,
             width = 1000, height = 600
    )%>%
      #add the wealth data
      add_trace(locations = ~wb_name,
                z = ~get(input$map_input),
                
                #specify the legend range 
                zmin = 0,
                zmax = max(cwon[input$map_input]),
                
                #specify variable to be mapped by color
                color = ~get(input$map_input))%>%
      colorbar(title = " Total Wealth (USD Trillions") %>%
      layout(title = "Wealth Distribution Across Countries in the World")
  })
  
  #Source: https://www.youtube.com/watch?v=RrtqBYLf404
  
  # ------------------------------------------------
  #World Graphs
  # Plot1 : Bar race plot of top wealthiest countries in the world
  #gif file rendered through UI
   
  #-------------------------------------------------------------
  #PLOT2: Trend of world wealth
  output$trend_output <-renderPlot({
    
    # Plotting trend of wealth over time
    
    # specify the data 
    world_wealth <- cwon%>%
      select(wb_name,year,totwealth,nk,pk,hc,nfa)%>%# select the variables for wealth and componente
      group_by(year)%>%
      summarise(total_wealth=sum(totwealth),natural_capital = sum(nk),produced_capital =sum(pk),
                human_capital = sum(hc), net_foreign_assets = sum(nfa) # sum total wealth for each year
      )
    
    options(repr.plot.width =9, repr.plot.height =15)
    
    # define colors for each line
    colors <- c('brown', 'green', 'blue', 'darkorange', 'red')
    
    # plot the data
    trendPlot <- ggplot(world_wealth, aes(x = year)) +
      geom_line(aes(y = total_wealth, color = 'Total Wealth'), size = 1.0) +
      geom_line(aes(y = natural_capital, color = 'Natural Capital'), size = 1.0) +
      geom_line(aes(y = produced_capital, color = 'Produced Capital'), size = 1.0) +
      geom_line(aes(y = human_capital, color = 'Human Capital'), size = 1.0) +
      geom_line(aes(y = net_foreign_assets, color = 'Net Foreign Assets'), size = 1.0) +
      labs(title = 'Decomposition of World Wealth over Time', y = 'World Wealth', color = 'Wealth Components') +
      scale_color_manual(values = colors)+
      theme(legend.position = 'bottom',plot.title = element_text(face = "bold"))
    
    # print the plot
    trendPlot
     
    #NB: Natural Capital id declining over time.
    #Wealth is being created at the expense of depletion of natural capital which has implications on sustainable development.
    
  })

  #-----------------------------------------------------------------------
  # Income level plots
  #PLOT1: #scatter plot
  #GDPPer Capita, Wealth Per capita', Life Expectancy, INcome level
  
  output$ILplot1 <- renderPlotly({
    
    plot1 <- ggplot(cwon_merge, aes(NY.GDP.PCAP.KD, wealthPerCapita,size = SP.DYN.LE00.IN, fill = wb_income)) +
      geom_point(aes(frame = year, ids = wb_name),color = 'darkgrey', position = 'jitter') +
      scale_x_log10()+ 
      labs(title = 'Wealth Per Capita Versus, GDP Percapita and Life Expectancy',
           caption = 'Source: World Bank 2021',
           size = 'Life Expectancy', fill = 'WB Income Level')+ 
      xlab('GDP Per Capita')+ ylab('Wealth Per Capita')
    
    
    ggplotly(plot1)
    
    # Countries with high wealth per capita and high GDP per capita have sustainable economic growth
    #Countries with High GDP per Capita but low wealth, are gaining high GDP at the expense of their assets
    # we can see dome countries with high GDP but declining wealth because they are depleting some resources
    
    
  })
  
#-------------------------------------------------------------------------------------  
  ##PLOT 2: Natural Capital Decomposition
  #stacked bar plot of wealth component natural capital which is affected by climate and other issues
  
  output$nkPlot <- renderPlot({
    
    #stacked bar plot of wealth component natural capital which is affected by climate and other issues
    
    cwon_nk <- cwon[,c(1:10, 12:19,21:25,39)]
    
    naturalCapital_factors<- pivot_longer(cwon_nk, cols = c(for_tim,for_notim,mangroves,fisheries,pa,land, cropland, pasture,	ene,oil,	gas,coal,min), 
                                          names_to = 'nk_fc', values_to = 'nk_values')
    
    
    ggplot(naturalCapital_factors,aes(x=year, y = nk_values,fill = nk_fc))+
      geom_col()+
      facet_wrap(~get(input$nkselect))+
      labs(title = 'Decomposition of Natural Capital over Time',fill = ' Components')+
      ylab('Natural Capital')
    
    
    # Growth has been at the expense of depletion of natural capital
    
  })
  
  0
  
  #-----------------------------------------------------------------------
  # CONTINENT/REGION OUTPUT
  
  #Plot1: Density Plot
  
  #Density plot
  #reactive function for data. It determines the year to view
  dataInput <- reactive({
    
    data_select <- cwon[cwon$year == input$danimation,] 
    
  })
  
  # rendering the plot
  output$r_wdist <- renderPlot({
    
    #Plotting density plot 
    ggplot(dataInput(), aes(x = wealthPerCapita, fill = factor(get(input$Density_data)))) +
      geom_density(alpha = 0.6) + scale_x_log10()+ # change the scale to logarithmic
      
      # input the titles and labels
      labs(x = "WealthPer Capita", y = "Density",
           title = paste("Distribution of Wealth Per Capita by", input$Density_data, "(", input$danimation, ")"),fill=paste(input$Density_data)) +
      #scale_fill_discrete(name = "")+
      theme(plot.title = element_text(face="bold"))# make the title bold
    
    
  })
  
  #Source: https://www.youtube.com/watch?v=hVimVzgtD6w
  
  
  ############################################
  #Plot2: Scatter plot of Wealth, Life expectancy and Popylation by income group
  
  #'Rscatter_data
  output$ILscatterPlot <- renderPlotly({
    
    # cresting plot
    gg <- ggplot(cwon_merge, aes(totwealth/1000000000, SP.DYN.LE00.IN, color = factor(get(input$Rscatter_data)))) +
      geom_point(aes(size = pop, frame = year, ids = wb_name)) +
      
      # customizing labels
      scale_x_log10()+labs(title = ' Trends in Wealth, Life Expectancy and Population ',
                           caption = ' Source: World Bank 2021',color='WB Region / Continent')+
      xlab(' Total Wealth (USD Billion)')+
      ylab('Life Expectancy')+
      theme(plot.title = element_text(face="bold"))# making plot title bold
    
    #gg$labels$color <-'Income level'#fill aesthetic label
    gg$labels$size <- 'Population'# size aesthetic label
    
    
    ggplotly(gg)
    
  })
  
 
  #----------------------------------------------------------------------
  #COUNTRY OUTPUT
  #-----------------------------------------------------------------------
  # Country name
  output$country_name<- renderText({
    
    input$c_country
    
    
  })
  
  #----------------------------------------------------------------- 
  # Output 1: share of wealth
  
  output$country_share <- renderText({
    #filter for year
    data <- cwon[cwon$year == input$c_year,]
    
    # filter for country
    data <- data[data$wb_name == input$c_country,]
    
    
    c_share <- round(data[37],2)
    c_share <- paste(c_share)
    
    
  })
  
 
  #--------------------------------------------------------- 
  #####PLOT OUTPUTS######
  ## wealth by country
  
  ctry_data <- reactive({
    # selection of required variables
    cwon_select <- cwon[,c(1:10,26,33,39)]
    # Transform wealth into a factor variable of its components
    totwealth_factors<- pivot_longer(cwon_select, cols = c(pk, nk,hc,nfa), names_to = 'totwealth_fc', values_to = 'totwealth_values')
    
    totwealth_factors%>%
      filter(wb_name== input$c_country )
    
  })
  
  #decomp_ctry
  output$c_decomp <- renderPlot({
    
    
    ggplot(ctry_data(),aes(x=year, y = totwealth_values,fill = totwealth_fc ))+
      geom_col()+
      geom_line(aes(y = totwealth), size = 1.0)+
      labs(title = 'Wealth Trend over Time',fill = 'Wealth composition')+
      ylab(' Wealth')
    
  })
  
  #--------------------------------------------------------------------------
  #Wealth share  by population share scatterplot with geom smooth
  
  # WEALTH SHARE VERSUS POP SHARE
  
  
  
  cs_data <-reactive({
    
    
    
    data <- cwon[cwon$wb_name == input$c_country,]
  })
  
  
  output$c_share <- renderPlotly({
    
    #create scatter plot 
    plot1 <- ggplot(cs_data(), aes(pop_share, w_share)) +
      geom_point(color ='orangered', size = 1.5) +
      geom_smooth()+
      scale_x_log10()+ 
      labs(title = 'World share of Wealth versus population share')+ 
      xlab('Share of world Population (%)')+ ylab('Share of World Wealth(%)')
    
    # create plotly plot 
    ggplotly(plot1)
    
    
  })
  
  
}


# Shiny app function
shinyApp(ui, server)

#Source: https://shiny.rstudio.com/tutorial/written-tutorial
