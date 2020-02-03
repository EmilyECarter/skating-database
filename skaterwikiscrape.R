#sometimes wikipedia 
#need to fix for skaters with multiple tables?-the headings come up again, there is a possibility the
#heading might change
#to fix this I may need to get more precise with choosing the correct table.


#need to now fix for a table with entry that goes across multiple rows.

link <- "https://en.wikipedia.org/wiki/Zhang_Hao_(figure_skater)"

result.list <- sapply(full.links, function(link){
  print(link)
  
  # get the lines of the table
  lines <- link %>%
    read_html() %>%
    html_nodes(xpath = '//th[contains(.,"Season")]/ancestor::table') %>%
    html_nodes(xpath = 'tbody/tr')
  if(length(lines) != 0){

    season.list <-lapply(lines, function(line){
      
      content <- line%>% 
        html_children()
      #make a vector witht the text.
      vec<- content%>%
        html_text%>%
        gsub("\n","",.)
      #now go through by each ul box instead of each td cell to capture things with a half line
      #because wikipeadia puts mulitple entries on a single cell.
      #if a content box has more than one ul box we want to duplicate the dataframe and make a new
      #row with the different content
      for(i in 1:length(content)){
        x <- content[[i]]
        mini.box <- length(x%>%html_nodes(xpath='ul'))
        if(mini.box >1){
          #duplicate the df and so each song can have its own row
          vec2 <- cbind(vec, vec)
          #create a vector to match the duplicated row to
          double.text <- x%>%html_text%>%
            gsub("\n","",.)
          #text to replace with
          replace <-x%>%html_nodes(xpath='ul')%>%html_text()
          #now replace the row 
          subset1 <- vec2==double.text & !is.na(vec2)
          vec[subset1] <-replace
        }
      }
      return(vec)
    })
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
})
