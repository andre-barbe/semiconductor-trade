#This file does everything for the project: downloading data, manipulating it, creating graphs (or calls the files that do)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Manually download data
  #CEPII Data comes from http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=35
  #UN COmtrade data comes from
    #/api/get?max=502&type=C&freq=A&px=HS&ps=2020%2C2019%2C2018%2C2017%2C2016&r=all&p=0&rg=all&cc=848620%2C903082%2C903141
    #website: https://comtrade.un.org/data/

#load comtrade data    
    data_comtrade=read.csv2(sep=",",file="data/Manual Download/comtrade 2021-08-16.csv")
    #How to save CSV files https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual5.html
      
  #Clean data
        data_comtrade$date <- as.Date(paste0(as.character(data_comtrade$Period),"0101"), format = "%Y%m%d")
          #the paste0 adds a month and a day value
          #need the quotes around 0101 otherwise leading zeroes are deleted

    #Rename variables
        names(data_comtrade)[names(data_comtrade)=="Trade.Value..US.."]="TradeValue"
      
#Load CEPII Data
  #Description
    #"The trade elasticity is the reaction of bilateral import flows (in value) to a change in the applied import tariff for a given product (as defined by the WCO’s six-digit Harmonized System classification in revision 2007 - hereafter HS6)."
    #Trade elasticities for semiconductors are very large in magnitude
  #Download Data
    #already download
    #Source: http://www.cepii.fr/CEPII/en/bdd_modele/presentation.asp?id=35
  #Load Data
    data_trade_elasticity <- read.csv(file="data/Manual Download/ProTEE_0_1.csv", header=TRUE, colClasses = c("character","numeric","numeric","numeric"))
    #colClasses tells the readCSV that the HS6 is a character, not numeric. Otherwise it thinks it is numeric and deletes leading zeroes
  
#graph Comtrade Data
library(ggplot2)
library("ggrepel")    
    #For adding labels next to the line
      #reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r

#create loop that creates graph for each HS code
    hs_codes_r=c("848620","903082","903141")
    for (i in 1:length(hs_codes_r)) {
      filterto_Commodity.Code <- hs_codes_r[[i]]
      #filter data to that HS code
        #example of how to both do bar chart and how to subset a dataframe is from
          #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      #delete data not in filter
        subsets1 <- subset(data_comtrade, (Trade.Flow %in% c("Export","Exports") & Commodity.Code %in% c(filterto_Commodity.Code)))
          #NTS: annual data calls it export (no -S)
          #NTS: monthly data calls it exports (yes -S)
        #Determine top exporting coutnries
          #sort data
            top_countries <- subsets1[subsets1$date == max(subsets1$date),]
              #filter to most recent year only
            top_countries <- top_countries[order(-top_countries$TradeValue),]
              #sort by trade value, descending
            top_X_countries <- top_countries$Reporter[1:4]
      #delete data not from top countries
          #subsets1 <- subset(subsets1, (Reporter %in% top_X_countries))
      #collapse data not from top 4 countries
        #if not in top 4, change name to "rest of world"
          subsets1$Reporter[!(subsets1$Reporter %in% top_X_countries)] <- "Rest of world"
        #Delete all columns except for the ones I am using
          subsets2 <- subsets1[,names(subsets1) %in% c("Commodity.Code","Period","Reporter","TradeValue")]
        #aggregate the data together, summing over trade values, for groups of period/title pairs
          subsets2 <- aggregate(subsets2$TradeValue, by = list(subsets2$Period,subsets2$Reporter), FUN = sum, na.rm=TRUE)
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        names(subsets2)=c("Period","Reporter","TradeValue")
          #For some reason, aggregating destroys all the column names so I have to put them back
          
      #add labels next to line
        #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
      data_label <- subsets2
      data_label$label <-NA
      data_label$label[which(data_label$Period == max(data_label$Period))] <- data_label$Reporter[which(data_label$Period == max(data_label$Period))]
      
        
      #graph subset
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      print(
        #ggplot won't show up if inside loop without this option
          #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
        ggplot(data_label, mapping = aes(x = Period, y = TradeValue, group=Reporter)) + #group specifies which data should be drawn as a single line
        #adds lines and legend
          geom_line(aes(linetype=Reporter))+
        #geom_lineadds the lines to the graph.
        #linetype specification adds teh legend
        #add big points (scatterplot)
          #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
          geom_point()+ 
        #Label title and axis
          #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
          labs(title=filterto_Commodity.Code
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

#Create table on CEPII trade data elascitiies
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
  
#load VLSI Data
    
#Combine VLSI Data with Comtrade Data
    
#Graph VLSI Data

