# Project2  

The purpose of this repository is to analyze a [news popularity dataset](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity), producing multiple files reporting analyses for each of the six different data channels: lifestyle, entertainment, social media, business, tech, and world. Each file goes through reading in the dataset, data manipulation and variable creation, summary table creation, data visualization, and model fitting using linear regression, random forest, and boosted tree methods. At the end of each document, a "best model" is declared.  

## Packages Used:  
* `tidyverse`  
* `caret`  
* `ggplot2`  

## Code for Rendering Documents:   
The code used to render the documents is as follows:  
`for(i in c("Lifestyle","Entertainment","Business","Social Media","Tech","World")){
rmarkdown::render("Project2.Rmd",output_file=i,params = list("channel"= i))
}`  

## Links:  
* The analysis for **lifestyle** articles is available [here](https://atbiggie.github.io/Project2/Lifestyle)  
* The analysis for **entertainment** articles is available [here](https://atbiggie.github.io/Project2/Entertainment)  
* The analysis for **business** articles is available [here](https://atbiggie.github.io/Project2/Business)  
* The analysis for **social media** articles is available [here](https://atbiggie.github.io/Project2/Social%20Media)  
* The analysis for **tech** articles is available [here](https://atbiggie.github.io/Project2/Tech)  
* The analysis for **world** articles is available [here](https://atbiggie.github.io/Project2/World)  

