# Define script to download comtrade data
# from https://comtrade.un.org/Data/Doc/api/ex/r
get.Comtrade.single <- function(url="http://comtrade.un.org/api/get?"
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
country_list = "124,842,410,702,490"
#Country id numbers from: https://comtrade.un.org/db/mr/rfreporterslist.aspx
#I think this can only run with 5 countries max. When I did 6 countries, I got an error
#484 Mexico
#124 Canada
#156 China
#842 USA
#410 South Korea
#490 Other Asia (Taiwan)
#Taiwan is not listed separately, it is part of "other asia NES"
#https://unstats.un.org/unsd/tradekb/Knowledgebase/50104/Taiwan-Province-of-China-Trade-data
#702 Singapore  

#Define period list
#I don't think a single download can can have more than 12 months of data
#website: https://comtrade.un.org/data/
period_list=c("2017","2018","2019")
#for some reason, 2018 data won't download.

save_location="data/data_comtrade.Rda"

#Run download loop
  get.Comtrade.loop <- function( country_list,
                                 freq = "M",
                                 ps = ps,
                                 hs_codes=hs_codes,
                                 save_location
  )
  {
    #Create list that will house each data download
    list_data_comtrade = list() #https://stackoverflow.com/questions/29402528/append-data-frames-together-in-a-for-loop/29419402
    #for loop that will download each year. I believe the API will only let me download 12 periods at most
    for (ps in period_list){
      #download data call
      data_comtrade_ps <- get.Comtrade.single(r = country_list
                                       ,p = country_list
                                       ,freq = freq
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
    saveRDS(data_comtrade,file=save_location) 
  }
  
if(download_data==1){
  get.Comtrade.loop(country_list = country_list
                    , ps = ps
                    ,hs_codes = hs_codes
                    ,save_location=save_location
                    )
}    
  
