#This file currently does everything for the project: downloading data, manipulating it, creating graphs (eventually)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Program options
  download_data <- 0

#Download Comtrade data
  #hs code list
  #28111111000 is Hydrogen Flouride which apparently is a chemical used in semiconductors
  #8541 and 8542 are semiconductors
  hs_codes_r=c("8541","8542","848071","8486","854370","854390","903082","903141")
  hs_codes_r4=substr(hs_codes_r,1,4)
    #https://statisticsglobe.com/r-extract-first-or-last-n-characters-from-string
  
  hs_codes=paste(hs_codes_r,sep=",")
  
  #Reference: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
  source("download_comtrade.R")
  
#Download non comtrade data
  #CEPII Data
    #already download
    #Source: http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=35
    #"The trade elasticity is the reaction of bilateral import flows (in value) to a change in the applied import tariff for a given product (as defined by the WCO’s six-digit Harmonized System classification in revision 2007 - hereafter HS6)."
    #Trade elasticities for semiconductors are very large in magnitude
    data_trade_elasticity <- read.csv(file="data/ProTEE_0_1.csv", header=TRUE, colClasses = c("character","numeric","numeric","numeric"))
      #colClasses tells the readCSV that the HS6 is a character, not numeric. Otherwise it thinks it is numeric and deletes leading zeroes


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
    
    data_trade_elasticity$HS6
    data_trade_elasticity_subset <- subset(data_trade_elasticity, startsWith(data_trade_elasticity$HS6,hs_codes_r4))
      #based on answer here but changed grepl to startswith
        #https://stackoverflow.com/questions/5823503/pattern-matching-using-a-wildcard
      #Startswith documentation: https://www.rdocumentation.org/packages/gdata/versions/2.18.0/topics/startsWith
    data_trade_elasticity_subset
    mean_data <- data.frame("mean of all (not just semi related) HS in CEPII database",0,0,mean(data_trade_elasticity$sigma, na.rm=TRUE))
      #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/mean
      #na.rum =TRUE means it deletes NAs (otherwise it just gives NA for the mean)
    
    names(mean_data) <- names(data_trade_elasticity_subset)
    data_trade_elasticity_subset <- rbind(data_trade_elasticity_subset,mean_data)

    
    

