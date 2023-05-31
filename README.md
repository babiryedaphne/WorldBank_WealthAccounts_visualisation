# WorldBank_WealthAccounts_visualisation
## The Changing Wealth of Nations

This repository contains code and data for a data visualization project focused on analyzing the changing wealth of nations over time. The project utilizes R and various R packages for data analysis and visualization.
## Installation
To run the code in this repository, you will need to have R installed on your system. You will also need to install the following R packages:

- shinythemes
- tidyverse
- maps
- mapproj
- maptools
- sf
- lubridate
- gganimate
- tweenr
- magick
- gifski
- magrittr
- countrycode
- grid
- plotly
- ggflags
- readxl
- VIM
- xlsx
- summarytools
- psych
- WDI

#### You can install these packages by running the following code in your R console:

install.packages(c("shinythemes", "tidyverse", "maps", "mapproj", "maptools", "sf", "lubridate", "gganimate", "tweenr", "magick", "gifski", "magrittr", "countrycode", "grid", "plotly", "ggflags", "readxl", "VIM", "xlsx", "summarytools", "psych", "WDI"))

## Data Source

The main data source used in this project is the "CWON2021 Country Tool - Full Dataset" from the World Bank. This dataset provides information on the changing wealth of nations from 1995 to 2018. The dataset is included in the repository as an Excel file named "CWON2021 Country Tool - Full Dataset.xlsx".
## Data Cleaning
Before performing any analysis or visualization, the data is cleaned and processed. Missing values are dealt with using the VIM package, and the countries without total wealth data are dropped from the dataset. Summary statistics and descriptive statistics are generated for the cleaned dataset.

## Calculations
Several calculations are performed on the cleaned dataset, including calculating wealth per capita, computing the proportion of total world wealth per country, and calculating the annual country ranks in terms of total wealth. Country codes and continent information are added to the dataset for visualization purposes. The dataset is then merged with additional world development indicators data from the World Bank.

## Exploratory Data Analysis
Various exploratory data analysis techniques are applied to the merged dataset. Visualizations are created to analyze the wealth composition, wealth by continent groups, and the relationship between wealth per capita and life expectancy. Additionally, a bar race chart is created to visualize the changing ranks of the top 15 wealthiest countries over time.

## Shiny App
The repository includes code for a Shiny app that provides an interactive interface for exploring the data visualizations. The Shiny app allows users to select variables for mapping and view various graphs related to wealth and income levels.

To run the Shiny app, open the R project file and execute the shiny::runApp() command in the R console.

