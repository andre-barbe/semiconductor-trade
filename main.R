#This file currently does everything for the project: downloading data, manipulating it, creating graphs (eventually)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Program options
  download_data <- 1

#Download Comtrade data
  source("download_comtrade.R")
  
#Download non comtrade data
  #CEPII Data
    #already download
    #Source: http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=35



#load data
    #How to save and load a file: https://stackoverflow.com/questions/8345759/how-to-save-a-data-frame-in-r
    data_comtrade <- readRDS(file="data/data_comtrade.Rda")
  
#Manipulate data sets
    #convert Trade values from strings to numerics
      data_comtrade$TradeValue <- as.numeric(data_comtrade$TradeValue)
    #convert periods to dates
      #see example in https://stackoverflow.com/questions/41327852/convert-a-yyyymm-format-to-date-in-r
      data_comtrade$period <- as.Date(paste0(as.character(data_comtrade$period),01), format = "%Y%m%d")
  #Define HS code aggregates
    data_comtrade$hs_group=""
    data_comtrade$hs_group[data_comtrade$cmdCode=="8541"]="semiconductors"
    data_comtrade$hs_group[data_comtrade$cmdCode=="8542"]="semiconductors"
  
#graphs
library(ggplot2)

  #subset data
    #only look at certain variables (imports) and for certain countries and HS codes
      #example of how to both do bar chart and how to subset a dataframe is from
      #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      subsets1 <- subset(data_comtrade, (rgDesc %in% c("Imports") & rtTitle %in% ("United States of America") & hs_group %in% c("semiconductors")))
    #aggregate certain HS codes to form product groups
      #aggregate TradeValue by group
        #https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        aggregate1 <- aggregate(x = subsets1$TradeValue #I can't keep the variable name so I have to rename x to TradeValue later
                                #aggregattion grouping
                                  #aggregate all entries with the same time period and partner country title
                                  #the name before the "=" tells what the new variable names are
                                  ,by=list(period=subsets1$period 
                                           ,ptTitle=subsets1$ptTitle) 
                                , FUN=sum #how to aggregate (summation)
                                )
      #rename x back to TradeValue
        #https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
        names(aggregate1)[names(aggregate1) == "x"] <- "TradeValue"
        
  #graph subset
      # Makes graphs look nicer.
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
    ggplot(aggregate1, mapping = aes(x = period, y = TradeValue, group=ptTitle)) + #group specifies which data should be drawn as a single line
      #adds lines and legend
        geom_line(aes(linetype=ptTitle))+
        #geom_lineadds the lines to the graph.
        #linetype specification adds teh legend
      #add big points (scatterplot)
        #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
        geom_point()+ 
      #Label title and axis
        #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
        labs(title="US imports"
             ,x="time"
             ,y="Trade Value (USD)"
               ) 
    
    #Do I need to deseasonalize monthly data? Probably not worth the trouble

