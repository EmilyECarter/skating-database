#we need to account for two things- firstly programs that are done two years in a row-
#address this by creating a table and filling it down

#secondly muliple programs in a year which is not picked up as a seprate table entry
#account for this by getting the ul blocks

link <- "https://en.wikipedia.org/wiki/Max_Aaron"


# get the lines of the table
lines <- link %>%
  read_html() %>%
  html_nodes(xpath = '//th[contains(.,"Season")]/ancestor::table') %>%
  html_nodes(xpath = 'tbody/tr')

#create a table to fill with values
ncol <-  lines %>%
  .[[1]] %>%
  html_children()%>%
  length()
nrow <- length(lines)
table <- as.data.frame(matrix(nrow = nrow,ncol = ncol))
  for(i in 1:nrow){
    
    
    raw <- lines[[i]]%>% 
      html_children()
    content <- raw %>%
      html_text()%>%
      gsub("\n","",.)

    colselect <- is.na(table[i,])
    table[i,colselect] <- content
    
    # get the line repetition of each columns
    repetition <- lines[[i]]%>%
      html_children()%>%
      html_attr("rowspan")%>%
      ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
      as.numeric
    
    # repeat the cells of the multiple rows down
    for(j in 1:length(repetition)){
      span <- repetition[j]
      if(span > 1){
        table[(i+1):(i+span-1),colselect][,j] <- rep(content[j],span-1)
      }
    }
  }
    
    #now we deal with the issue of two programs per season
    for(k in 1:length(raw)){
      x <- raw[[k]]
      #this first method works if they are formatted in ul boxes ie have dot points
      mini.box <- length(x%>%html_nodes(xpath='ul'))
      if(mini.box >1){
        #duplicate the row of the dataframe by the number of programs in the season
        for(m in 1:(mini.box-1)){
          #i is the column number
          table[seq(i+m,nrow(table)+1),] <- table[seq(i,nrow(table)),]
        }
        #now select the entries to be replaced
        double.text <- x%>%html_text%>%
          gsub("\n","",.)
        replace <-x%>%html_nodes(xpath='ul')%>%html_text()
        subset1 <- table==double.text
        table[subset1] <-replace
        
      }
      #however some wikipeadia pages are not formatted as such, instead just have an"hr" tag
      
      #skip the first entry which is always the year and thus never has a ul tag
    
      if(mini.box==0 & k>1){
        print("YES")
        tags <- raw[[k]]%>%html_children()
        for l in 1:length(tags){
          if(html_name(tags[l])=="hr"){
            #this gets complicated-we have to find where the partial line is (the hr tag)
            #then find the text directly before the partial line and use this to split the text
            split_text <- tags[l-1]%>%html_text
            full_text <- raw[[k]]%>%html_text()
            replace_txt <- strsplit(full_text,split=split_text, fixed=TRUE)
            #now we have to paste the split back onto the first part
            replace_txt[[1]][1]<- paste(replace_txt[[1]][1],split_text, sep = "")
            #then as above duplicate the dataframe row for the multiple entries.
            
            #we don't have to do another loop because the loop will be repeated at each hr tag
            #I HOPE LETS SEE
            table[seq(i+1,nrow(table)+1),] <- table[seq(i,nrow(table)),]

            #now select the entries to be replaced
            double.text <-full_text>%
              gsub("\n","",.)
            subset1 <- table==double.text
            table[subset1] <-replace_txt
            
            
          }
        }
        #check to see if the box has an hr tag in it
      }
      
      
      #need to do yet another loop through to get the indivuial tags
      #I miss lists and apply
      
    }
  }
    
    

  #Now we have a list with every seasons programs 
  #the first entry on the list is the catogory names 
  #use these to add a name catogry column to each season
  labels <- season.list[[1]]
  #now that we have the names remove the first entry from the list
  season.list[[1]] <- NULL 
  all.programs <- lapply(season.list, function(music){
    print(music)
    music<- gsub("\\[.*?\\]","",music)
    rep_length <- length(music)/length(labels)
    program <- rep(labels, rep_length )
    df <- data.frame(program, music)
    #make a new column for the season
    #it should always be the first cell- sometimes it is called different things in wikipedia
    #if this becomes a problems later fix
    df$season <- df[1,2]
    df<- df[-1,]
    df <- with(df, df[!(music == "" | is.na(music)), ])
    return(df)
    #remove entries with NAs
  })
  
  all.programs <- bind_rows(all.programs)
  #Now we just need a column for the name of the skater
  #get this from the title of the wikipage?
  name <- url %>%
    read_html() %>%
    html_node(xpath = '//*[@id="firstHeading"]')%>%html_text()
  all.programs$name <- name
  return(all.programs)
}





lines[[11]]%>%
  html_children()%>%
  html_attr("rowspan")%>%
  ifelse(is.na(.),1,.) %>% # if no rowspan, then it is a normal row, not a multiple one
  as.numeric
