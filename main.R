#This file currently does everything for the project: downloading data, manipulating it, creating graphs (eventually)

#Clear variables
  #Source: https://www.geeksforgeeks.org/clear-the-console-and-the-environment-in-r-studio/
  rm(list=ls())

#Program options
  download_data <- 1

# Define script to download comtrade data
  # from https://comtrade.un.org/Data/Doc/api/ex/r
  get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                           ,maxrec=50000
                           ,type="C"
                           ,freq="A"
                           ,px="HS"
                           ,ps="now"
                           ,r
                           ,p
                           ,rg="all"
                           ,cc="TOTAL"
                           ,fmt="json"
  )
  {
    string<- paste(url
                   ,"max=",maxrec,"&" #maximum no. of records returned
                   ,"type=",type,"&" #type of trade (c=commodities)
                   ,"freq=",freq,"&" #frequency
                   ,"px=",px,"&" #classification
                   ,"ps=",ps,"&" #time period.
                   #Dont' need to list each time month. For example, according to https://comtrade.un.org/data/doc/api/#DataRequests if you are using monthly data, you can just type the years in
                   ,"r=",r,"&" #reporting area
                   ,"p=",p,"&" #partner country
                   ,"rg=",rg,"&" #trade flow
                   ,"cc=",cc,"&" #classification code
                   ,"fmt=",fmt        #Format
                   ,sep = ""
    )
    
    if(fmt == "csv") {
      raw.data<- read.csv(string,header=TRUE)
      return(list(validation=NULL, data=raw.data))
    } else {
      if(fmt == "json" ) {
        raw.data<- fromJSON(file=string)
        data<- raw.data$dataset
        validation<- unlist(raw.data$validation, recursive=TRUE)
        ndata<- NULL
        if(length(data)> 0) {
          var.names<- names(data[[1]])
          data<- as.data.frame(t( sapply(data,rbind)))
          ndata<- NULL
          for(i in 1:ncol(data)){
            data[sapply(data[,i],is.null),i]<- NA
            ndata<- cbind(ndata, unlist(data[,i]))
          }
          ndata<- as.data.frame(ndata)
          colnames(ndata)<- var.names
        }
        return(list(validation=validation,data =ndata))
      }
    }
  }

#define hscodes of interest

#define countries of interest (all?)
  #reporters
  #partners
  #https://wits.worldbank.org/wits/wits/witshelp/content/codes/country_codes.htm
  
  
#Download comtrade data
  library("rjson")
  #Examples from https://comtrade.un.org/Data/Doc/api/ex/r which runs the get.Comtrade script
 
  #Define what to download for COMTRADE
    country_list = "124,484,842,410,702"
      #Country id numbers from: https://comtrade.un.org/db/mr/rfreporterslist.aspx
      #124 Canada
      #842 USA
      #410 South Korea
      #490 Other Asia (Taiwan)
      #Taiwan is not listed separately
        #https://unstats.un.org/unsd/tradekb/Knowledgebase/50104/Taiwan-Province-of-China-Trade-data
      #702 Singapore  
    
    #Define period list
      #I don't think a single download can can have more than 12 months of data
      #website: https://comtrade.un.org/data/
      period_list=c("2017","2019")
    #hs code list
      #28111111000 is Hydrogen Flouride which apparently is a chemical used in semiconductors
      #8541 and 8542 are semiconductors
      hs_codes="8541,8542,848071,8486,854370,854390,903082,903141"
    #Reference: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit

      #Run download loop
      if(download_data==1){
        #Create list that will house each data download
        list_data_comtrade = list() #https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
        #for loop that will download each year. I believe the API will only let me download 12 periods at most
        for (ps in period_list){
          #download data call
          data_comtrade_ps <- get.Comtrade(r = country_list
                                           ,p = country_list
                                           ,freq ="M"
                                           ,ps = ps
                                           ,cc=hs_codes
          )
          #save downloaded data to the list
          list_data_comtrade[[ps]] <- data_comtrade_ps$data
          
          #Add a pause before downloading again
          #I think this is necessary to prevent the WB system from rejecting the requests as too frequent
          #How to pause: https://stackoverflow.com/questions/34859558/set-a-delay-between-two-instructions-in-r
          Sys.sleep(3) 
        }
        
        #Combine all the downloaded data into a single data frame
        data_comtrade = do.call(rbind, list_data_comtrade)
        
        #save COMTRADE data to file
          #How to save and load a file: https://stackoverflow.com/questions/8345759/how-to-save-a-data-frame-in-r
          #using "saveRDS" instead of "save" because "save" saves *all* objects and with their current names.
          #so when they are loaded, it overwrites everything and makes a huge mess
          saveRDS(data_comtrade,file="data/data_comtrade.Rda") 
      }

#Download non comtrade data
  #statistica



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

