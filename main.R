#This file does everything for the project: downloading data, manipulating it, creating graphs (or calls the files that do)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())


#Define semiconductor related HS codes
  #Reference 1: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    #28111111000 is Hydrogen Flouride which apparently is a chemical used in semiconductors
  #semiconductors themselves
    #8541 and 8542 are semiconductors
    #list_hs_semi=c("8541","8542")
  #SME
    #Source: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    list_hs_SME=c("848071","8486","854370","854390","903082","903141")
  #SM Chemicals
    #https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit
    list_hs_chemicals=c("281111","370790","391190","392099")
  #define an r readable version of the hs codes
    hs_codes_r=c(list_hs_SME,list_hs_chemicals)
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
  #https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm
    #Taiwan is not listed separately, it is part of "other asia NES"
    #https://unstats.un.org/unsd/tradekb/Knowledgebase/50104/Taiwan-Province-of-China-Trade-data
 
#Define years of interest list
  #For UNC COMTrade
    #I don't think a single download can can have more than 12 months of data
    #website: https://comtrade.un.org/data/
    period_list=c("2016","2017","2018","2019","2020")

#define frequency
    UN_COMTRADE_freq="A"
  
#Comtrade data
  #Define scripts to download data
    source("download_comtrade.R")
  #Download Comtrade Data
      #Do two calls as there is a max download limit per call
      data_comtrade_raw_1 <- get.Comtrade.single(r = "all"
                                              ,p = "0"
                                              ,freq = UN_COMTRADE_freq
                                              ,ps = "2018,2019,2020"
                                              ,cc=hs_codes
                                              ,rg="2"
      )
      
      Sys.sleep(3) 
        #wait between downloads as otherwise the UN system cancels the second download
      
      data_comtrade_raw_2 <- get.Comtrade.single(r = "all"
                                                ,p = "0"
                                                ,freq = UN_COMTRADE_freq
                                                ,ps = "2016,2017"
                                                ,cc=hs_codes
                                                ,rg="2"
  )
    #Combine the two downloads into a single data set
      data_comtrade=rbind(data_comtrade_raw_1$data,data_comtrade_raw_2$data)
      
  #Save Data
    #How to save and load a file: https://stackoverflow.com/questions/8345759/how-to-save-a-data-frame-in-r
    saveRDS(data_comtrade,file="data/data_comtrade.Rda") 
    
    
  #Clean data
    #convert Trade values from strings to numerics
      data_comtrade$TradeValue <- as.numeric(data_comtrade$TradeValue)
    #convert periods to dates
      #see example in https://stackoverflow.com/questions/41327852/convert-a-yyyymm-format-to-date-in-r
      if(UN_COMTRADE_freq=="M"){
        data_comtrade$period <- as.Date(paste0(as.character(data_comtrade$period),"01"), format = "%Y%m%d")
          #the paste0 adds a day value
          #need the quotes around 01 otherwise leading zeroes are deleted
      }
      if(UN_COMTRADE_freq=="A"){
        data_comtrade$period <- as.Date(paste0(as.character(data_comtrade$period),"0101"), format = "%Y%m%d")
          #the paste0 adds a month and a day value
          #need the quotes around 0101 otherwise leading zeroes are deleted
      }
      
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
library("ggrepel")    
    #For adding labels next to the line
      #reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r

#create loop that creates graph for each HS code
    for (i in 1:length(hs_codes_r)) {
      filterto_cmdCode <- hs_codes_r[[i]]
      #filter data to that HS code
        #example of how to both do bar chart and how to subset a dataframe is from
          #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      #delete data not in filter
        subsets1 <- subset(data_comtrade, (rgDesc %in% c("Export","Exports") & cmdCode %in% c(filterto_cmdCode)))
          #NTS: annual data calls it export (no -S)
          #NTS: monthly data calls it exports (yes -S)
        #Determine top exporting coutnries
          #sort data
            top_countries <- subsets1[subsets1$period == max(subsets1$period),]
              #filter to most recent year only
            top_countries <- top_countries[order(-top_countries$TradeValue),]
              #sort by trade value, descending
            top_X_countries <- top_countries$rtTitle[1:4]
      #delete data not from top countries
          #subsets1 <- subset(subsets1, (rtTitle %in% top_X_countries))
      #collapse data not from top 4 countries
        #if not in top 4, change name to "rest of world"
          subsets1$rtTitle[!(subsets1$rtTitle %in% top_X_countries)] <- "Rest of world"
        #Delete all columns except for the ones I am using
          subsets2 <- subsets1[,names(subsets1) %in% c("cmdCode","period","rtTitle","TradeValue")]
        #aggregate the data together, summing over trade values, for groups of period/title pairs
          subsets2 <- aggregate(subsets2$TradeValue, by = list(subsets2$period,subsets2$rtTitle), FUN = sum, na.rm=TRUE)
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        names(subsets2)=c("period","rtTitle","TradeValue")
          #For some reason, aggregating destroys all the column names so I have to put them back
          
      #add labels next to line
        #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
      data_label <- subsets2
      data_label$label <-NA
      data_label$label[which(data_label$period == max(data_label$period))] <- data_label$rtTitle[which(data_label$period == max(data_label$period))]
      
        
      #graph subset
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      print(
        #ggplot won't show up if inside loop without this option
          #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
        ggplot(data_label, mapping = aes(x = period, y = TradeValue, group=rtTitle)) + #group specifies which data should be drawn as a single line
        #adds lines and legend
          geom_line(aes(linetype=rtTitle))+
        #geom_lineadds the lines to the graph.
        #linetype specification adds teh legend
        #add big points (scatterplot)
          #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
          geom_point()+ 
        #Label title and axis
          #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
          labs(title=filterto_cmdCode
             ,x="time"
             ,y="Trade Value (USD)"
        )+
        geom_label_repel(aes(label = label),
                         nudge_x = 1,
                         na.rm = TRUE)
          #adds the line labels next to the lines
        +theme(legend.position = "none")
            #removes the legend
        +ylim(0, max(data_label$TradeValue))
          #https://ggplot2.tidyverse.org/reference/lims.html
          #sets y axis values to range from 0 to whatever the max is
          #without this option, the min y value is set at whatever the min of the data is, not 0
      )
    }

#Create table on trade data elascitiies
  #subset CEPII data to only look at those related to semiconductors
      data_trade_elasticity$filter <- (substr(data_trade_elasticity$HS6,1,4) %in% hs_codes_r) | (substr(data_trade_elasticity$HS6,1,6) %in% hs_codes_r)
        #hs_codes_r contain some codes that are 4 digit and some that are 6 digit
        #an HS6 in the trade elascitiy data passes the filter if it its HS6 is an exact match for the HS6 in the code list, or its HS4 is an exact match for an HS4 in the code list
      #keep trade data elasticities that match th filter
        data_trade_elasticity_subset <- subset(data_trade_elasticity, data_trade_elasticity$filter)

  #Create row with average trade elasticity of all( not just trade related) CEPII HSes
    mean_data <- data.frame("mean of all (not just semi related) HS in CEPII database",0,0,mean(data_trade_elasticity$sigma, na.rm=TRUE),TRUE)
    #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/mean
    #na.rum =TRUE means it deletes NAs (otherwise it just gives NA for the mean)
    names(mean_data) <- names(data_trade_elasticity_subset)
      #this is necessary so that the new row binds with the existing datframe
  #Merge the mean row with the existing dataframe
    data_trade_elasticity_subset <- rbind(data_trade_elasticity_subset,mean_data)
  #Round to nearest whole number for ease of reading
    data_trade_elasticity_subset$sigma = round(data_trade_elasticity_subset$sigma,0)
    data_trade_elasticity_subset
  

