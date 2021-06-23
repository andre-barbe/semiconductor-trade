#This file currently does everything for the project: downloading data, manipulating it, creating graphs (eventually)

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
                   ,"ps=",ps,"&" #time period
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
  
#Download comtrade data
  {
  #Example data download
    #Example 1 from https://comtrade.un.org/Data/Doc/api/ex/r which runs the get.Comtrade script
    library("rjson")
    s1 <- get.Comtrade(r="842", p="124,484")
    s1
  
  #export data to file so don't have to re-download
    #https://mail.rfaqs.com/reading-and-writing-json-files-in-r/
    jsons1 <- toJSON(s1)
    write(jsons1,"data/s1.json")
  
  #actual data download goes here
  }

#Download non comtrade data
  #statistica
  
#combine data sets
#manipulate data
  
#graphs





