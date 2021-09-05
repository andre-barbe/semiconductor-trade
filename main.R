#This file does everything for the project: downloading data, manipulating it, creating graphs (or calls the files that do)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Load Data--------------------------------------------------
  
#Manually download data
  #CEPII Data comes from http://www.cepii.fr/cepii/en/bdd_modele/presentation.asp?id=35
  #UN Comtrade data comes from
    #/api/get?max=502&type=C&freq=A&px=HS&ps=2020%2C2019%2C2018%2C2017%2C2016&r=all&p=0&rg=all&cc=848620%2C903082%2C903141
    #website: https://comtrade.un.org/data/

#load comtrade data    
    data_comtrade=read.csv2(sep=",",file="Inputs Manual Download/comtrade 2021-08-16.csv")
    #How to save CSV files https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R-Manual/R-Manual5.html
        
#Clean data comtrade data
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
    data_trade_elasticity <- read.csv(file="Inputs Manual Download/ProTEE_0_1.csv", header=TRUE, colClasses = c("character","numeric","numeric","numeric"))
    #colClasses tells the readCSV that the HS6 is a character, not numeric. Otherwise it thinks it is numeric and deletes leading zeroes

    
    
    
#Graph Comtrade Data (Comtrade only, not combined with VLSI yet)-------------------------------------------- 
    
#Load libraries for graphing
  library(ggplot2)
  library("ggrepel")    
      #For adding labels next to the line
        #reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r

#Create graph that shows SME exports aggregated over all commodities, for each top country
    #Prepares data to be graphed
      data_agg_SME=data_comtrade #Creates data set for this graph
      #collapse data not from top 4 countries
        data_agg_SME$Reporter[!(data_agg_SME$Reporter %in% c("USA","Netherlands","Japan","Singapore"))] <- "Rest of world" #Turns all countries that aren't US, Neth, or J into "Rest of WOrld"
        data_agg_SME <- data_agg_SME[,names(data_agg_SME) %in% c("Commodity.Code","Period","Reporter","TradeValue")]     #Delete all columns except for the ones I am using
        data_agg_SME <- aggregate(data_agg_SME$TradeValue, by = list(data_agg_SME$Period,data_agg_SME$Reporter), FUN = sum, na.rm=TRUE) #aggregate the data together, summing over trade values, for groups of period/title pairs
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
        names(data_agg_SME)=c("Period","Reporter","TradeValue")
          #For some reason, aggregating destroys all the column names so I have to put them back
        data_agg_SME$TradeValue=data_agg_SME$TradeValue/1000/1000/1000  #Change trade values from dollars to million USD
        
    #Creates actual graph
      #add labels next to line
      #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
      data_agg_SME <- data_agg_SME
      data_agg_SME$label[which(data_agg_SME$Period == max(data_agg_SME$Period))] <- data_agg_SME$Reporter[which(data_agg_SME$Period == max(data_agg_SME$Period))]
      
      #Save plot as png
      png(file=paste("Results/Exports of ALL SME Combined.png",sep=""))
      
      #graph subset
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      print(
        #ggplot won't show up if inside loop without this option
        #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
        ggplot(data_agg_SME, mapping = aes(x = Period, y = TradeValue, group=Reporter, color=Reporter)) + #group specifies which data should be drawn as a single line
          #adds lines and legend
          geom_line(aes(linetype=Reporter))+
            #geom_lineadds the lines to the graph.
            #linetype specification adds thh legend
          #add big points (scatterplot)
          #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
          geom_point()+ 
          #Label title and axis
          #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
          labs(title=paste("Exports of all 3 SMEs combined, by Country")
               ,x="time"
               ,y="Exports (Billion USD)"
          )+
          geom_label_repel(aes(label = label),
                           nudge_x = 1,
                           na.rm = TRUE)
        #adds the line labels next to the lines
        +theme(legend.position = "none")
        #removes the legend
        +ylim(0, max(data_agg_SME$TradeValue))
        #https://ggplot2.tidyverse.org/reference/lims.html
        #sets y axis values to range from 0 to whatever the max is
        #without this option, the min y value is set at whatever the min of the data is, not 0
      ) #closes print
    dev.off() #close png for graph file being saved to, so can open a new one
    #Add total row to data frame
    sum_allied_SME_exports_2020 =  
      sum(as.numeric(data_agg_SME$TradeValue[
            data_agg_SME$Period==2020
            &(data_agg_SME$Reporter %in% c("USA","Japan","Netherlands"))
          ]
          ))
      data_agg_SME=rbind(data_agg_SME,
                         c("2020",
                           "Total Allies",
                           sum_allied_SME_exports_2020,
                           round(100*480/1000/sum_allied_SME_exports_2020,1)
                            #Percentage of 2021 China <=10nm equipment spendinng.
                            #The number 480 million is from here: https://docs.google.com/spreadsheets/d/1SiiEytk54fNhxXEMLaBCUXEDAA6cL7XsA3xPxOBl5mM/edit#gid=1406310715&range=E43
                           )
      )
      
    write.csv(data_agg_SME, file=paste("Results/Table of Exports of All SME Combined.csv",sep=""),row.names = F) #Exports results to make fact checking easier
    
# #create loop that creates graph for each HS code
#     hs_codes_COM=c("848620","903082","903141")
#     for (i in 1:length(hs_codes_COM)) {
#       filterto_Commodity.Code <- hs_codes_COM[[i]]
#       #filter data to that HS code
#         #example of how to both do bar chart and how to subset a dataframe is from
#           #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
#       #delete data not in filter
#         data_graph_comtrade_temp <- subset(data_comtrade, (Commodity.Code %in% c(filterto_Commodity.Code)))
#           #NTS: annual data calls it export (no -S)
#           #NTS: monthly data calls it exports (yes -S)
#       #Determine top exporting coutnries
#         #sort data
#           top_countries <- data_graph_comtrade_temp[data_graph_comtrade_temp$date == max(data_graph_comtrade_temp$date),]
#             #filter to most recent year only
#           top_countries <- top_countries[order(-top_countries$TradeValue),]
#             #sort by trade value, descending
#           top_X_countries <- top_countries$Reporter[1:4]
#       #delete data not from top countries
#           #data_graph_comtrade_temp <- subset(data_graph_comtrade_temp, (Reporter %in% top_X_countries))
#       #collapse data not from top 4 countries
#         #if not in top 4, change name to "rest of world"
#           data_graph_comtrade_temp$Reporter[!(data_graph_comtrade_temp$Reporter %in% top_X_countries)] <- "Rest of world"
#         #Delete all columns except for the ones I am using
#           data_graph_comtrade_temp <- data_graph_comtrade_temp[,names(data_graph_comtrade_temp) %in% c("Commodity.Code","Period","Reporter","TradeValue")]
#         #aggregate the data together, summing over trade values, for groups of period/title pairs
#           data_graph_comtrade_temp <- aggregate(data_graph_comtrade_temp$TradeValue, by = list(data_graph_comtrade_temp$Period,data_graph_comtrade_temp$Reporter), FUN = sum, na.rm=TRUE)
#           #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
#         names(data_graph_comtrade_temp)=c("Period","Reporter","TradeValue")
#           #For some reason, aggregating destroys all the column names so I have to put them back
#           
#       #add labels next to line
#         #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
#       data_graph_comtrade_temp$label[which(data_graph_comtrade_temp$Period == max(data_graph_comtrade_temp$Period))] <- data_graph_comtrade_temp$Reporter[which(data_graph_comtrade_temp$Period == max(data_graph_comtrade_temp$Period))]
#       
#       #Change trade values from dollars to million USD
#         data_graph_comtrade_temp$TradeValue=data_graph_comtrade_temp$TradeValue/1000/1000/1000
#       
#       #Save plot as png
#         png(file=paste("Results/Exports of ",hs_codes_COM[i],".png",sep=""))
#         
#       #graph subset
#       # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
#       print(
#         #ggplot won't show up if inside loop without this option
#           #https://stackoverflow.com/questions/15678261/ggplot-does-not-work-if-it-is-inside-a-for-loop-although-it-works-outside-of-it
#         ggplot(data_graph_comtrade_temp, mapping = aes(x = Period, y = TradeValue, group=Reporter)) + #group specifies which data should be drawn as a single line
#         #adds lines and legend
#           geom_line(aes(linetype=Reporter))+
#         #geom_lineadds the lines to the graph.
#         #linetype specification adds teh legend
#         #add big points (scatterplot)
#           #reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization  
#           geom_point()+ 
#         #Label title and axis
#           #Reference: http://www.sthda.com/english/wiki/ggplot2-line-plot-quick-start-guide-r-software-and-data-visualization#customized-line-graphs
#           labs(title=paste("Exports of",filterto_Commodity.Code,"by Top 4 Exporters and ROW")
#              ,x="time"
#              ,y="Exports (Billion USD)"
#         )+
#         geom_label_repel(aes(label = label),
#                          nudge_x = 1,
#                          na.rm = TRUE)
#           #adds the line labels next to the lines
#         +theme(legend.position = "none")
#             #removes the legend
#         +ylim(0, max(data_graph_comtrade_temp$TradeValue))
#           #https://ggplot2.tidyverse.org/reference/lims.html
#           #sets y axis values to range from 0 to whatever the max is
#           #without this option, the min y value is set at whatever the min of the data is, not 0
#       )
#       dev.off()
#         #close graph file being saved to, so can open a new one
#       write.csv(data_graph_comtrade_temp, file=paste("Results/Table of Exports of ",hs_codes_COM[i],".csv",sep=""),row.names = F) #Exports results to make fact checking easier
#     }
#     

    
    
#Create table on CEPII trade data elascitiies---------------------------------------------
  #subset CEPII data to only look at those related to SME or semiconductors
    hs_codes_CEPII=append(c("848620","903082","903141"),c("8541","8542","270900"))
      #Semiconductors are split between 8541 and 8542
        #Source: http://www.wcoomd.org/-/media/wco/public/global/pdf/events/2019/hs-conference/semiconductors-and-the-future-of-the-hs_sia-white-paper_april-2019.pdf?la=fr
      #Crude oil is 270900
        # Source:https://hts.usitc.gov/?query=crude%20petroleum
    data_trade_elasticity$filter <- (substr(data_trade_elasticity$HS6,1,4) %in% hs_codes_CEPII) | (substr(data_trade_elasticity$HS6,1,6) %in% hs_codes_CEPII)
      #hs_codes_CEPII contain some codes that are 4 digit and some that are 6 digit
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
    write.csv(table_trade_elasticity, file="Results/Table Trade Elasticity.csv",row.names = F)

    
    
    
    
    
    
    
    
#Load Data and Create Graph for VLSI Revenue / Comtrade Exports---------------------------------
  #load VLSI Data
    data_revenue=read.csv2(file="Inputs Manual Download/VLSI Revenue Data.csv",sep=",",header=T,
                              skip=3
                                #skip=3 to skip irrelevant rows as described in here https://stackoverflow.com/questions/23902421/designating-other-than-first-row-as-headers-in-r
                              #,colClasses = c("character","character","character","numeric","numeric","numeric","numeric","numeric")
                              ,stringsAsFactors = FALSE
                              )
    #Convert VLSI Data to Numeric
    data_revenue$AOW[is.na(data_revenue$AOW)]="NA"
      #it imports NA (North America) as <NA> so I turn it back into "NA"
    #Label Netherlands firms as from NE, not EU
      data_revenue$AOW[data_revenue$COMPANY %in% c("ASML","ASMI")]="NE"
        #Per Will H's instructions, these are the companies in the Netherlands
      #see https://cset-collab.atlassian.net/browse/SEMI-41
    data_revenue=data_revenue[data_revenue$AOW %in% c("NA","JA","NE"),]
    for (year in 2016:2020){
      var_name=paste("X",year,sep="")
        #define corresponding variable name for each year
      data_revenue[,var_name][is.na(data_revenue[,var_name])]="0"
        #replace NAs with "0"s
      data_revenue[,var_name][data_revenue[,var_name]=="EX"]="0"
        #replace EXs with "0"s
      data_revenue[,var_name]=as.numeric(data_revenue[,var_name])
        #convert to numeric
    }

  #Clean VLSI Revenue Data
    #Collapse VLSI Data by HQ Region
      #delete unused columns
        data_revenue <- data_revenue[,names(data_revenue) %in% c("AOW","X2016","X2017","X2018","X2019","X2020")]
      #aggregate the data together, summing over trade values, for each AOW
        data_revenue_wide <- aggregate(cbind(data_revenue$X2016,data_revenue$X2017,data_revenue$X2018,data_revenue$X2019,data_revenue$X2020),
                                           by = list(data_revenue$AOW), FUN = sum, na.rm=TRUE)
          #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
      #For some reason, aggregating destroys all the column names so I have to put them back
        names(data_revenue_wide)=c("Region","X2016","X2017","X2018","X2019","X2020")
    
    #Reshape data to long
      library(tidyr)
      #convert ID variable to factor
        #specified here http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
        data_revenue_wide$Region=as.factor(data_revenue_wide$Region)
      #Do reshaping
        #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
        data_revenue_long <- gather(data_revenue_wide, Year, Revenue, X2016:X2020, factor_key=TRUE)
      #Convert years to numeric  
        library(stringr)
        data_revenue_long$Year=as.numeric(str_sub(data_revenue_long$Year,-4))
      #Convert report names to "country" to prep for merge with revenue data
        names(data_revenue_long)[names(data_revenue_long)=="Region"]="Country"
      #Convert country names to long names to prep for merge
        levels(data_revenue_long$Country)[levels(data_revenue_long$Country)=="JA"]="Japan"
        levels(data_revenue_long$Country)[levels(data_revenue_long$Country)=="NE"]="Netherlands"
        levels(data_revenue_long$Country)[levels(data_revenue_long$Country)=="NA"]="USA"
    
  #Create COMTRADE Dataset by country
    #delete all data not from 3 important countries
        data_graph_comtrade_temp_country=data_comtrade[(data_comtrade$Reporter %in% c("Japan","USA","Netherlands")),]
      #Delete all columns except for the ones I am using
        data_graph_comtrade_temp_country <- data_graph_comtrade_temp_country[,names(data_graph_comtrade_temp_country) %in% c("Period","Reporter","TradeValue")]
      #aggregate the data together, summing over trade values, for groups of period/title pairs
        data_graph_comtrade_temp_country <- aggregate(data_graph_comtrade_temp_country$TradeValue, by = list(data_graph_comtrade_temp_country$Period,data_graph_comtrade_temp_country$Reporter), FUN = sum, na.rm=TRUE)
        #Reference: https://stackoverflow.com/questions/1660124/how-to-sum-a-variable-by-group
      names(data_graph_comtrade_temp_country)=c("Year","Country","Exports")
        #For some reason, aggregating destroys all the column names so I have to put them back
      #Convert Country to factor
        data_graph_comtrade_temp_country$Country=as.factor(data_graph_comtrade_temp_country$Country)
    
  #Merge revenue and comtrade data into a wide data frame
    data_pe_wide=merge(data_graph_comtrade_temp_country,data_revenue_long,by=c("Country","Year"))
    #Convert data to all be billions
      data_pe_wide$Exports=data_pe_wide$Exports/1000/1000/1000 #Exports are originally in dollars
      data_pe_wide$Revenue=data_pe_wide$Revenue/1000  #revenue originally in million dollars
    #Reshape dataset to long
      #http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
      data_pe_long <- gather(data_pe_wide, Flow, Value, Revenue:Exports, factor_key=TRUE)
    #Convert Flow from factor to character
      #https://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters
      library(dplyr)
      data_pe_long %>% mutate(across(where(is.factor), as.character)) -> data_pe_long
            
  #Create Graphs of revenue and Export Data by Country
    #add labels next to line, at the last year, of each revenue type
      #Reference: https://statisticsglobe.com/add-labels-at-ends-of-lines-in-ggplot2-line-plot-r
      data_pe_long$label[which(data_pe_long$Year == max(data_pe_long$Year))] <-
        data_pe_long$Flow[which(data_pe_long$Year == max(data_pe_long$Year))]
    #Loop to create line for each country
      # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
      list_regions=c("USA","Netherlands","Japan")
      for (i in 1:length(list_regions)) {
        #Save plot as png
          png(file=paste("Results/Prod and Exports by ",list_regions[i],".png",sep=""))
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
            labs(title=paste(list_regions[i],": Comparison of Domestic Exports (COMTRADE) and Worldwide Revenue of Firms HQed here (VLSI)")
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
        ) #close print
      dev.off() #Stops plotting this graph file, so can begin next file for next country
      } #Close loop over countries
      write.csv(data_pe_long, file=paste("Results/Table of Exports vs WW Revenue.csv",sep=""),row.names = F) #Exports results to make fact checking easier

    print("Job Completed Successfully")
                