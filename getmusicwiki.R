library(rvest)
library(dplyr)
library(reshape2)

page <- read_html("https://en.wikipedia.org/wiki/List_of_figure_skaters")



#//*[@id="mw-content-text"]/div/div[28]
page <- read_html("https://en.wikipedia.org/wiki/List_of_figure_skaters")
pages <- page %>%
  html_nodes(xpath = '//*[@id=\"mw-content-text\"]/div/div[28]') %>%
  html_nodes("li") %>%
  html_nodes("a")
half.links<- html_attr(pages, "href")
full.links <- paste("https://en.wikipedia.org", half.links, sep="")
#remove dead links
dead.links <- grepl('action=edit&redlink=1', full.links)
full.links <- full.links[!dead.links]
link.list <- append(link.list, full.links)




#do a loop going from 2 to 28 to get every letter 
#create the links
i<- 2:28
xpaths <- sapply(i,function(i){
  paste("\'//*[@id=\"mw-content-text\"]/div/div[", i,"]\'", sep="" )
})

#for each letter of the alpbet get all the links to skaters pages
links<- sapply(xpaths, function(link){
  page <- read_html("https://en.wikipedia.org/wiki/List_of_figure_skaters")
  pages <- page %>% html_nodes(xpath = link) %>%
  html_nodes("li") %>%
  html_nodes("a")
  half.links<- html_attr(pages, "href")
  full.links <- paste("https://en.wikipedia.org", half.links, sep="")
  #remove dead links
  dead.links <- grepl('action=edit&redlink=1', full.links)
  full.links <- full.links[!dead.links]
  return(full.links)
})

#now that we have all the links, scrape each webpage for the music/program table.
result.list <- sapply(link.list, function(link){
  print(link)
  x <-link %>% 
    read_html() %>%
    html_nodes(xpath = '//th[contains(.,"Season")]/ancestor::table') %>% 
    html_table(fill = TRUE)
  #test if there are any programs listed on the page
  if(length(x) != 0){
    if(is.list(x[[1]]) ==T){
      result <- lapply(x, function(x){
        programs <- colnames(x)
        programs <- programs[!programs == "Season"]
        result<- melt(x, measure.vars=programs)
        return(result)
      })
      result <- bind_rows(result)
      
    } else{
      programs <- colnames(x)
      programs <- programs[!programs == "Season"]
      result<- melt(x, measure.vars=programs)
      result <-bind_rows(result)
    }
    name <-
    
    
    
    return(result)
  }
})


##############problems found
#I didn't get the names of each skater-rewrite the function to get the names 
#sometime it picks up different tables- might not be too much of a problem if I just remove it
#years where the skater skates to multiple different pieces of musics-it has a line through it
link <- "https://en.wikipedia.org/wiki/Max_Aaron"
x <-link %>% 
  read_html() %>%
  html_nodes(xpath = '//th[contains(.,"Season")]/ancestor::table') %>% 
  html_table(fill = TRUE)

{
  data.frame(bgcolor = html_nodes(., 'hr') %>% html_attr('bgcolor'), 
             html_table(.))
  }

fulldf <- bind_rows(result.list)
#get rid of the citation marks in the season
#remove empty ones-where the skater didn't do an exhibition?


#can do a manual search of the data
#break the music up, get the name of the music pice
#usually the music is formated by piece name follwed by the word "by"
#not sure yet how to account for pairs/ice dance

  



format.data.function(x, link){
  #melt the program type, but keep what year
  programs <- colnames(x)
  #now add a column for the name, get this from the link
  name <- link
  program$name <- name
}
#this creates a list of tables when there are multiple tables. so lappy through the list and rbind