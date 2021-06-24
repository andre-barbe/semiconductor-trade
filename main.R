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
  #Example data download
    #Example 1 from https://comtrade.un.org/Data/Doc/api/ex/r which runs the get.Comtrade script
    library("rjson")
    #s1 <- get.Comtrade(r="842", p="124,484")
    #s1
    
  #example 3
    #s3 <- get.Comtrade(r="842", p="0", ps="201201,201202,201203", freq="M")
  
  #export data to file so don't have to re-download
    #https://mail.rfaqs.com/reading-and-writing-json-files-in-r/
    #jsons1 <- toJSON(s1)
    #write(jsons1,"data/s1.json")
    
  #actual data download goes here
    country_list = "124,484,842"
      #124 
      #842 is USA
    #Define period list
      period_list=c("2018","2019")
    #hs code list
    #Reference: https://docs.google.com/document/d/1pbYg6z0LPQEcC5yolcURZpsSPQ5AkxFQ1Mdh-0C09Q8/edit

    #Actual data
      #Create the big overall data frame so that it can be used in the loop
        list_data_comtrade = list() #https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
      for (ps in period_list){
        data_comtrade_ps <- get.Comtrade(r = country_list
                                    ,p = country_list
                                    ,freq ="M"
                                    ,ps = ps
                                    )
        list_data_comtrade[[ps]] <- data_comtrade_ps$data
        #Add a pause
          #I think this is necessary to prevent the WB system from rejecting the requests as too frequent
          #How to pause: https://stackoverflow.com/questions/34859558/set-a-delay-between-two-instructions-in-r
          Sys.sleep(3) 
      }
      data_comtrade = do.call(rbind, list_data_comtrade)
  

#Download non comtrade data
  #statistica


#I don't think a single download can can have more than 12 months of data
  #website: https://comtrade.un.org/data/
  
#save data to file

#load data if not redownloading it
  
#combine data sets
#manipulate data
  
#graphs
library(ggplot2)

#The data is downloaded as strings, so I need to convert it to numeric if I want to graph it.
#s1$data$TradeValue <- as.numeric(s1$data$TradeValue)
#s1$data$yr <- as.numeric(as.character(s1$data$yr))

#subset data
  #example of how to both do bar chart and how to subset a dataframe is from
  #https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
  subsets1 <- subset(data_comtrade, (rgDesc %in% c("Imports") & rtTitle %in% ("United States of America")))
  

#graph subset
    # Makes graphs look nicer.
    # From https://www.datanovia.com/en/blog/how-to-subset-a-dataset-when-plotting-with-ggplot2/
  ggplot(subsets1, mapping = aes(x = period, y = TradeValue, group=ptTitle)) + #group specifies which data should be drawn as a single line
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

