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
    
    #Delete non-export data
        data_comtrade <- subset(data_comtrade, (Trade.Flow %in% c("Export","Exports")))
        
      
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
        data_graph_comtrade <- subset(data_comtrade, (Commodity.Code %in% c(filterto_Commodity.Code)))
          #NTS: annual data calls it export (no -S)
          #NTS: monthly data calls it exports (yes -S)
      #Determine top exporting coutnries
        #sort data
          top_countries <- data_graph_comtrade[data_graph_comtrade$date == max(data_graph_comtrade$date),]
            #filter to most recent year only
          top_countries <- top_countries[order(-top_countries$TradeValue),]
            #sort by trade value, descending
          top_X_countries <- top_countries$Reporter[1:4]
      #delete data not from top countries
          #data_graph_comtrade <- subset(data_graph_comtrade, (Reporter %in% top_X_countries))
      #collapse data not from top 4 countries
        #if not in top 4, change name to "rest of world"
          data_graph_comtrade$Reporter[!(data_graph_comtrade$Reporter %in% top_X_countries)] <- "Rest of world"
        #Delete all columns except for the ones I am using
          data_graph_comtrade <- data_graph_comtrade[,names(data_graph_comtrade) %in% c("Commodity.Code","Period","Reporter","TradeValue")]
        #aggregate the data together, summing over trade values, for groups of period/title pairs
          data_graph_comtrade <- aggregate(data_graph_comtrade$TradeValue, by = list(data_graph_comtrade$Period,data_graph_comtrade$Reporter), FUN = sum, na.rm=TRUE)
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        names(data_graph_comtrade)=c("Period","Reporter","TradeValue")
          #For some reason, aggregating destroys all the column names so I have to put them back
          
      #add labels next to line
        #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
      data_graph_comtrade <- data_graph_comtrade
      data_graph_comtrade$label[which(data_graph_comtrade$Period == max(data_graph_comtrade$Period))] <- data_graph_comtrade$Reporter[which(data_graph_comtrade$Period == max(data_graph_comtrade$Period))]
      
      #Change trade values from dollars to million USD
        data_graph_comtrade$TradeValue=data_graph_comtrade$TradeValue/1000/1000/1000
      
      #Save plot as png
        png(file=paste("data/Results/Exports of ",hs_codes_r[i],".png",sep=""))
        
      #graph subset
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      print(
        #ggplot won't show up if inside loop without this option
          #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
        ggplot(data_graph_comtrade, mapping = aes(x = Period, y = TradeValue, group=Reporter)) + #group specifies which data should be drawn as a single line
        #adds lines and legend
          geom_line(aes(linetype=Reporter))+
        #geom_lineadds the lines to the graph.
        #linetype specification adds teh legend
        #add big points (scatterplot)
          #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
          geom_point()+ 
        #Label title and axis
          #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
          labs(title=paste("Exports of",filterto_Commodity.Code,"by Top 4 Exporters and ROW")
             ,x="time"
             ,y="Exports (Billion USD)"
        )+
        geom_label_repel(aes(label = label),
                         nudge_x = 1,
                         na.rm = TRUE)
          #adds the line labels next to the lines
        +theme(legend.position = "none")
            #removes the legend
        +ylim(0, max(data_graph_comtrade$TradeValue))
          #https://ggplot2.tidyverse.org/reference/lims.html
          #sets y axis values to range from 0 to whatever the max is
          #without this option, the min y value is set at whatever the min of the data is, not 0
      )
      
      dev.off()
        #close graph file being saved to, so can open a new one
    }

#Create table on CEPII trade data elascitiies
  #subset CEPII data to only look at those related to semiconductors
      data_trade_elasticity$filter <- (substr(data_trade_elasticity$HS6,1,4) %in% hs_codes_r) | (substr(data_trade_elasticity$HS6,1,6) %in% hs_codes_r)
        #hs_codes_r contain some codes that are 4 digit and some that are 6 digit
        #an HS6 in the trade elascitiy data passes the filter if it its HS6 is an exact match for the HS6 in the code list, or its HS4 is an exact match for an HS4 in the code list
      #keep trade data elasticities that match th filter
        table_trade_elasticity <- subset(data_trade_elasticity, data_trade_elasticity$filter)

  #Create row with average trade elasticity of all( not just trade related) CEPII HSes
    mean_TE_of_all_HS <- data.frame("mean of all (not just semi related) HS in CEPII database",NA,NA,mean(data_trade_elasticity$sigma, na.rm=TRUE),TRUE)
    #https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/mean
    #na.rum =TRUE means it deletes NAs (otherwise it just gives NA for the mean)
    names(mean_TE_of_all_HS) <- names(table_trade_elasticity)
      #this is necessary so that the new row binds with the existing datframe
  #Merge the mean row with the existing dataframe
    table_trade_elasticity <- rbind(table_trade_elasticity,mean_TE_of_all_HS)
  #Clean Table
    #Round to nearest whole number for ease of reading
      table_trade_elasticity$sigma = round(table_trade_elasticity$sigma,0)
    #Drop filter variable as no longer needed
      table_trade_elasticity=table_trade_elasticity[names(table_trade_elasticity)!="filter"]
    #Rename variable names to be more clear
      names(table_trade_elasticity)[names(table_trade_elasticity)=="zero"]="Sig Dif from Zero?"
      names(table_trade_elasticity)[names(table_trade_elasticity)=="positive"]="Any positive Elasticities?"
      names(table_trade_elasticity)[names(table_trade_elasticity)=="sigma"]="Trade Elasticity"
  #Export Table
    table_trade_elasticity
    write.csv(table_trade_elasticity, file="data/Results/Table Trade Elasticity.csv",row.names = F)
  
#load VLSI Data
    data_production=read.csv2(file="data/Manual Download/VLSI Production Data.csv",sep=",",header=T,
                              skip=3
                                #skip=3 to skip irrelevant rows as described in here https://stackoverflow.com/questions/23902421/designating-other-than-first-row-as-headers-in-r
                              #,colClasses = c("character","character","character","numeric","numeric","numeric","numeric","numeric")
                              ,stringsAsFactors = FALSE
                              )
    #Convert VLSI Data to Numeric
    data_production$AOW[is.na(data_production$AOW)]="NA"
      #it imports NA (North America) as <NA> so I turn it back into "NA"
    #Label Netherlands firms as from NE, not EU
      data_production$AOW[data_production$COMPANY %in% c("ASML","ASMI")]="NE"
        #Per Will H's instructions, these are the companies in the Netherlands
      #see https://cset-collab.atlassian.net/browse/SEMI-41
    data_production=data_production[data_production$AOW %in% c("NA","JA","NE"),]
    for (year in 2016:2020){
      var_name=paste("X",year,sep="")
        #define corresponding variable name for each year
      data_production[,var_name][is.na(data_production[,var_name])]="0"
        #replace NAs with "0"s
      data_production[,var_name][data_production[,var_name]=="EX"]="0"
        #replace EXs with "0"s
      data_production[,var_name]=as.numeric(data_production[,var_name])
        #convert to numeric
    }

      
    #Collapse VLSI Data by HQ Region
      #delete unused columns
        data_production <- data_production[,names(data_production) %in% c("AOW","X2016","X2017","X2018","X2019","X2020")]
      #aggregate the data together, summing over trade values, for each AOW
        data_production_wide <- aggregate(cbind(data_production$X2016,data_production$X2017,data_production$X2018,data_production$X2019,data_production$X2020),
                                           by = list(data_production$AOW), FUN = sum, na.rm=TRUE)
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
      #For some reason, aggregating destroys all the column names so I have to put them back
        names(data_production_wide)=c("Region","X2016","X2017","X2018","X2019","X2020")
    
      #Reshape data to long
        library(tidyr)
        #convert ID variable to factor
          #specified here http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
          data_production_wide$Region=as.factor(data_production_wide$Region)
        #Do reshaping
          #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
          data_production_long <- gather(data_production_wide, Year, Production, X2016:X2020, factor_key=TRUE)
        
        #Convert years to numeric  
          library(stringr)
          data_production_long$Year=as.numeric(str_sub(data_production_long$Year,-4))
        
        #Convert report names to "country" to prep for merge with produciton data
          names(data_production_long)[names(data_production_long)=="Region"]="Country"
        #Convert country names to long names to prep for merge
          levels(data_production_long$Country)[levels(data_production_long$Country)=="JA"]="Japan"
          levels(data_production_long$Country)[levels(data_production_long$Country)=="NE"]="Netherlands"
          levels(data_production_long$Country)[levels(data_production_long$Country)=="NA"]="USA"
          
          
      #Create COMTRADE Dataset by country
        #delete all data not from 3 important countries
            data_graph_comtrade_country=data_comtrade[(data_comtrade$Reporter %in% c("Japan","USA","Netherlands")),]
          #Delete all columns except for the ones I am using
            data_graph_comtrade_country <- data_graph_comtrade_country[,names(data_graph_comtrade_country) %in% c("Period","Reporter","TradeValue")]
          #aggregate the data together, summing over trade values, for groups of period/title pairs
            data_graph_comtrade_country <- aggregate(data_graph_comtrade_country$TradeValue, by = list(data_graph_comtrade_country$Period,data_graph_comtrade_country$Reporter), FUN = sum, na.rm=TRUE)
            #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
          names(data_graph_comtrade_country)=c("Year","Country","Exports")
            #For some reason, aggregating destroys all the column names so I have to put them back
          #Convert Country to factor
            data_graph_comtrade_country$Country=as.factor(data_graph_comtrade_country$Country)
      
      #Merge production and comtrade data
          data_pe_wide=merge(data_graph_comtrade_country,data_production_long,by=c("Country","Year"))
          #Convert data to all be billions
            data_pe_wide$Exports=data_pe_wide$Exports/1000/1000/1000 #Exports are originally in dollars
            data_pe_wide$Production=data_pe_wide$Production/1000  #Production originally in million dollars
          #Reshape dataset to long
            #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
            data_pe_long <- gather(data_pe_wide, Flow, Value, Production:Exports, factor_key=TRUE)
            
            
      #Graph Production and Export Data by Country
            
            #Convert Flow from factor to character
              #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
              library(dplyr)
              data_pe_long %>% mutate(across(where(is.factor), as.character)) -> data_pe_long
            
            
              #add labels next to line, at the last year, of each production type
                #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
                data_pe_long$label[which(data_pe_long$Year == max(data_pe_long$Year))] <-
                  data_pe_long$Flow[which(data_pe_long$Year == max(data_pe_long$Year))]
              
                #graph each country
                list_regions=c("USA","Netherlands","Japan")
                for (i in 1:length(list_regions)) {
                  # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
                  
                  #Save plot as png
                    png(file=paste("data/Results/Prod and Exports by ",list_regions[i],".png",sep=""))
                    #How to save a plot: https://www.datamentor.io/r-programming/saving-plot/
                  
                  print(
                    #ggplot won't show up if inside loop without this option
                    #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
                    
                        
                    ggplot(data_pe_long[data_pe_long$Country==list_regions[i],], mapping = aes(x = Year, y = Value, group=Flow)) + #group specifies which data should be drawn as a single line
                      #adds lines and legend
                      geom_line(aes(linetype=Flow))+
                      #geom_lineadds the lines to the graph.
                      #linetype specification adds teh legend
                      #add big points (scatterplot)
                      #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
                      geom_point()+ 
                      #Label title and axis
                      #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
                      labs(title=paste("Comparison of SME Exports (COMTRADE) and Firm Revenue (VLSI) for",list_regions[i])
                           ,x="time"
                           ,y="Billion USD"
                      )+
                      geom_label_repel(aes(label = label),
                                       nudge_x = 1,
                                       na.rm = TRUE)
                    #adds the line labels next to the lines
                    +theme(legend.position = "none")
                    #removes the legend
                    +ylim(0, max(data_pe_long$Value))
                    #https://ggplot2.tidyverse.org/reference/lims.html
                    #sets y axis values to range from 0 to whatever the max is
                    #without this option, the min y value is set at whatever the min of the data is, not 0
                  )
                  
                  dev.off()
                    #Stops plotting this file, so can begin next file
                  
                }

                