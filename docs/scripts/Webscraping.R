#Web scraping + Bayes + Floorball
#
library(tidyverse)
library(rvest)
library(htmlTable)
library(RSelenium)
library(httr)

#####
# just for testing
url <- "https://tulospalvelu.fliiga.com/category/402!sb2024/statistics/points"

#Let us create a function to get raw data
get_data <- function(num_of_seasons){
  all_data = data.frame(matrix(ncol = 13, nrow=0))
  colnames(all_data) <- c("PELAAJA","JOUKKUE","O","M","S","P","R","+","-","+/-","L","L%", "KAUSI")
  url <- "https://tulospalvelu.fliiga.com/category/402!sb2024/statistics/points"
  #
  rD <- rsDriver(browser = "firefox",
                 chromever = NULL)
  Sys.sleep(3)
  remDr <- rD$client
  #x <- 1
  for(x in 0:num_of_seasons){
    vuosi = as.character(2024-x)
    url_new <- str_replace(url, "2024", vuosi)
    #
    remDr$navigate(url_new)
    #
    Sys.sleep(2)
    #
    pop <- tryCatch({rD$client$findElement(using = "xpath", value = "//div[@id='qc-cmp2-ui']/div[2]/div/button[2]/span")
    }, error = function(e1){
      message("No popups")
      NULL
    })
    if(!is.null(pop)){
      print("Founded popoup!")
      pop$clickElement()
    } else{
      print("Not founded popup")
    }
    #
    Sys.sleep(2)
    #
    if(vuosi=="2024"){
      raw <- remDr$findElement(using = "xpath",
                               value = '//*[@id="categorystatistics"]/div[3]')
      #
      raw <- raw$getElementText()[[1]]
      #
      lines <- unlist(strsplit(raw, "\n"))
      #
      column_names <- c(lines[1:12])
      #
      lines <- lines[13:length(lines)]
      #
      raw2 <- do.call(rbind, strsplit(lines, "\\s+"))
      #
      df <- as.data.frame(raw2, stringsAsFactors = FALSE)
      df <- df %>% unite("PELAAJA", V1, V2, sep = " ", remove = TRUE)
      colnames(df) <- column_names
      
      df <- df %>% mutate(kausi = paste0(vuosi,"-",as.character(as.integer(vuosi)+1)))
      all_data <- rbind(all_data, df)
      
    } else{
      dropdown_trigger <- rD$client$findElement(using = "css selector", value = ".v-input:nth-child(1) .v-select__slot .v-icon")
      Sys.sleep(2)
      dropdown_trigger$clickElement()
      Sys.sleep(2) 
      
      dropdown_trigger2 <-tryCatch({rD$client$findElement(using = "xpath", value = "//div[contains(text(), 'Runkosarja')]")
      }, error = function(e3){
        tryCatch({rD$client$findElement(using = "xpath", value = "//div[contains(text(), 'F-liiga miehet')]")
        }, error = function(e3){
          tryCatch({rD$client$findElement(using = "xpath", value = "//div[contains(text(), '1099')]")
          }, error = function(e3){
            message("No F-liiga")
            NULL
          })
        })
      })
      
      if(!is.null(dropdown_trigger2)){
        Sys.sleep(2) 
        print("Founded element!")
        dropdown_trigger2$clickElement()
        Sys.sleep(2) 
        raw <- remDr$findElement(using = "xpath",
                                 value = '//*[@id="categorystatistics"]/div[3]')
        raw <- raw$getElementText()[[1]]
        lines <- unlist(strsplit(raw, "\n"))
        #
        column_names <- c(lines[1:12])
        #
        lines <- lines[13:length(lines)]
        #
        raw2 <- do.call(rbind, strsplit(lines, "\\s+"))
        #
        df <- as.data.frame(raw2, stringsAsFactors = FALSE)
        df <- df %>% unite("PELAAJA", V1, V2, sep = " ", remove = TRUE)
        colnames(df) <- column_names
        #
        df <- df %>% mutate(kausi = paste0(vuosi,"-",as.character(as.integer(vuosi)+1)))
        all_data <- rbind(all_data, df)
        
      } else{
        print("Not founded element!")
      }
    } 
    
  }
  remDr$close()
  rD$server$stop()
  
  #Return data
  return(all_data)
  
}

get_rest_games <- function(){
  rD <- rsDriver(browser = "firefox",
                 chromever = NULL)
  Sys.sleep(3)
  remDr <- rD$client
  #
  url_games <- "https://tulospalvelu.fliiga.com/category/402!sb2024/group/1"
  remDr$navigate(url_games)
  #
  pop <- tryCatch({rD$client$findElement(using = "xpath", value = "//div[@id='qc-cmp2-ui']/div[2]/div/button[2]/span")
  }, error = function(e1){
    message("No popups")
    NULL
  })
  if(!is.null(pop)){
    print("Founded popoup!")
    pop$clickElement()
  } else{
    print("Not founded popup")
  }
  #
  
  raw3 <- remDr$findElement(using = "xpath",
                            value = '//*[@id="categorygrouptables"]/div/div/div/div[1]')
  #
  raw3 <- raw3$getElementText()[[1]]
  #
  lines <- unlist(strsplit(raw3, "\n"))
  #
  column_names <- c(lines[1:13])
  #
  lines2 <- lines[14:length(lines)]
  #
  raw4 <- do.call(rbind, strsplit(lines, "\\s+"))
  #
  rows <- list()
  #
  index <- 1
  #For parsing messy data
  while (index <= length(lines2)) {
    hash <- lines2[index]
    joukkue <- lines2[index + 1]
    main_stats <- unlist(strsplit(lines2[index + 2], " "))
    five_viim <- lines2[(index + 3):(index + 7)]
    #
    row <- c(
      hash,
      joukkue,
      main_stats, 
      paste(five_viim, collapse = " ")
    )
    
    rows <- append(rows, list(row))
    index <- index + 8
  }
  
  df <- do.call(rbind, rows)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  colnames(df) <- column_names
  df <- df %>% select(JOUKKUE, O)
  df$O <- as.integer(df$O)
  df <- df %>% mutate("Games Left" = 33-O)
  
  #Finally close Selenium
  remDr$close()
  rD$server$stop()
  
  #Return data
  return(df)
}


#Download stats for last x seasons. 4 means this plus the last 4
#testi <- get_data(4)
#games_left <- get_rest_games()
#

