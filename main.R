#This file does everything for the project: downloading data, manipulating it, creating graphs (or calls the files that do)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Program options
  download_data <- 1

#Define semiconductor related HS codes
  #Reference 1: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    #28111111000 is Hydrogen Flouride which apparently is a chemical used in semiconductors
  #semiconductors themselves
    #8541 and 8542 are semiconductors
    list_hs_semi=c("8541","8542")
  #SME
    #Source: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    list_hs_SME=c("848071","8486","854370","854390","903082","903141")
  #SM Chemicals
    #https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    list_hs_chemicals=c("281111","370790","391190","392099")
  #define an r readable version of the hs codes
    hs_codes_r=c(list_hs_semi,list_hs_SME,list_hs_chemicals)
    #how to combine lists: https://stackoverflow.com/questions/36665492/how-to-combine-two-lists-in-r
  hs_codes_r4=substr(hs_codes_r,1,4)
    #define a HS4 equivalent, for use in the CEPII data
    #https://statisticsglobe.com/r-extract-first-or-last-n-characters-from-string
    #I considered going to the HS2 level but that seems to be way too broad. The sigmas within each HS2 were all over the place
  hs_codes=paste(hs_codes_r,collapse=',')
    #define a version readable by the UNCOMTRADE API
    #https://stackoverflow.com/questions/2098368/concatenate-a-vector-of-strings-character
    #You want to use only collapse, not also sep, otherwise you get weird results

#define countries of interest (all?)
  #reporters
  #partners
  #https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm
  
  #Define what to download for COMTRADE
    #I think this can only run with 5 countries max. When I did 6 countries, I got an error
    country_list1 = "842,156,392,410,528" #USA China Japan South Korea Netherlands
    country_list2 = "490,702, 276" #Taiwan Singapore Germany
    #Country id numbers from: https://comtrade.un.org/db/mr/rfreporterslist.aspx
      #484 Mexico
      #124 Canada
      #156 China
      #842 USA
      #410 South Korea
      #392 Japan
      #528 Netherlands
      #490 Other Asia (Taiwan)
      #276 Germany
      #Taiwan is not listed separately, it is part of "other asia NES"
      #https://unstats.un.org/unsd/tradekb/Knowledgebase/50104/Taiwan-Province-of-China-Trade-data
      #702 Singapore  
    #Accoridng to wikipedia, this covers more or less all the major semiconductor countries
      #Source https://en.wikipedia.org/wiki/Semiconductor_industry#Regions
  
#Define years of interest list
  #For UNC COMTrade
    #I don't think a single download can can have more than 12 months of data
    #website: https://comtrade.un.org/data/
    period_list=c("2017","2018","2019")
  #for some reason, 2018 data won't download.
  
#Comtrade data
  #Define scripts to download data
    source("download_comtrade.R")
  #Download Comtrade Data
    if(download_data==1){
      get.Comtrade.loop(list_reporter = country_list1
                        ,list_partner = "0"
                        ,ps = ps
                        ,hs_codes = hs_codes
                        ,save_location="data/data_comtrade1.Rda"
      )
                        
      get.Comtrade.loop(list_reporter = country_list2
                        ,list_partner = "0"
                        , ps = ps
                        ,hs_codes = hs_codes
                        ,save_location="data/data_comtrade2.Rda"
      )
      
    }    
  #load Data
    #How to save and load a file: https://stackoverflow.com/questions/8345759/how-to-save-a-data-frame-in-r
    data_comtrade1 <- readRDS(file="data/data_comtrade1.Rda")
    data_comtrade2 <- readRDS(file="data/data_comtrade2.Rda")
    data_comtrade = rbind(data_comtrade1,data_comtrade2)
    
  #Clean data
    #convert Trade values from strings to numerics
      data_comtrade$TradeValue <- as.numeric(data_comtrade$TradeValue)
    #convert periods to dates
      #see example in https://stackoverflow.com/questions/41327852/convert-a-yyyymm-format-to-date-in-r
      data_comtrade$period <- as.Date(paste0(as.character(data_comtrade$period),01), format = "%Y%m%d")
  #Define semiconductor related subgroups
      #for example, SME vs SM inputs vs SM (themselves)
      data_comtrade$hs_group=""
      data_comtrade$hs_group[data_comtrade$cmdCode %in% list_hs_semi]="semiconductors"
      data_comtrade$hs_group[data_comtrade$cmdCode %in% list_hs_SME]="SME"
      data_comtrade$hs_group[data_comtrade$cmdCode %in% list_hs_chemicals]="semi chemicals"
  
#CEPII Data
  #Description
    #"The trade elasticity is the reaction of bilateral import flows (in value) to a change in the applied import tariff for a given product (as defined by the WCO’s six-digit Harmonized System classification in revision 2007 - hereafter HS6)."
    #Trade elasticities for semiconductors are very large in magnitude
  #Download Data
    #already download
    #Source: http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=35
  #Load Data
    data_trade_elasticity <- read.csv(file="data/ProTEE_0_1.csv", header=TRUE, colClasses = c("character","numeric","numeric","numeric"))
    #colClasses tells the readCSV that the HS6 is a character, not numeric. Otherwise it thinks it is numeric and deletes leading zeroes
  
#graphs
library(ggplot2)

  #subset data
    #only look at certain variables (imports) and for certain countries and HS codes
      #example of how to both do bar chart and how to subset a dataframe is from
      #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      subsets1 <- subset(data_comtrade, (rgDesc %in% c("Exports") & hs_group %in% c("SME")))
    #aggregate certain HS codes to form product groups
      #aggregate TradeValue by group
        #https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        aggregate1 <- aggregate(x = subsets1$TradeValue #I can't keep the variable name so I have to rename x to TradeValue later
                                #aggregattion grouping
                                  #aggregate all entries with the same time period and partner country title
                                  #the name before the "=" tells what the new variable names are
                                  ,by=list(period=subsets1$period 
                                           ,rtTitle=subsets1$rtTitle) 
                                , FUN=sum #how to aggregate (summation)
                                )
      #rename x back to TradeValue
        #https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
        names(aggregate1)[names(aggregate1) == "x"] <- "TradeValue"
        
  #graph subset
      # Makes graphs look nicer.
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
    ggplot(aggregate1, mapping = aes(x = period, y = TradeValue, group=rtTitle)) + #group specifies which data should be drawn as a single line
      #adds lines and legend
        geom_line(aes(linetype=rtTitle))+
        #geom_lineadds the lines to the graph.
        #linetype specification adds teh legend
      #add big points (scatterplot)
        #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
        geom_point()+ 
      #Label title and axis
        #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
        labs(title="Graph"
             ,x="time"
             ,y="Trade Value (USD)"
               ) 
  #Create table on trade data elascitiies
    #subset CEPII data to only look at those related to semiconductors
    data_trade_elasticity_subset <- subset(data_trade_elasticity
                                           , startsWith(data_trade_elasticity$HS6,
                                              #based on answer here but changed grepl to startswith
                                              #https://stackoverflow.com/questions/5823503/pattern-matching-using-a-wildcard
                                              #Startswith documentation: https://www.rdocumentation.org/packages/gdata/versions/2.18.0/topics/startsWith
                                              hs_codes_r4))
                                                #hs_codes_r4 is the first 4 digit of the semi-related HS codes.
                                                #I do this because some of the semi codes are 6 digit, but only their 4 digit parent (or sibling) is in the CEPII data

    #Create row with average trade elasticity of all( not just trade related) CEPII HSes
      mean_data <- data.frame("mean of all (not just semi related) HS in CEPII database",0,0,mean(data_trade_elasticity$sigma, na.rm=TRUE))
      #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/mean
      #na.rum =TRUE means it deletes NAs (otherwise it just gives NA for the mean)
      names(mean_data) <- names(data_trade_elasticity_subset)
        #this is necessary so that the new row binds with the existing datframe
    #Merge the mean row with the existing dataframe
      data_trade_elasticity_subset <- rbind(data_trade_elasticity_subset,mean_data)
      data_trade_elasticity_subset
    

