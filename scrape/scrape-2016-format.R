###
### Program for parsing 2016 season format PDFs
###
library(tabulizer)
library(pdftools)
library(magick)
library(tidyverse)
library(here)


### 
### Functions to obtain header information
###
get_compo_info <- function(thefile) {
    
    competitionName_xy <- c(top=23,left=140,bottom=45,right=510)
    competitionDate_xy <- c(top=48,left=400,bottom=68,right=500)
    competitionPlace_xy <- c(top=63,left=140,bottom=80,right=384)
    competitionType_xy <- c(top=79, left=143,bottom=95,right=305)
    
    componame <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(competitionName_xy))
    compodate <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(competitionDate_xy))
    compoplace <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(competitionPlace_xy))
    compotype <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(competitionType_xy))
    
    compoinfo <- data.frame(series = componame[[1]], place = compoplace[[1]], date = compodate[[1]], ctype = compotype[[1]])
    return(compoinfo)
}

###
### Judge information
###
get_judge_info <- function(thefile) {
    
    judges_xy <- c(top = 130, left = 236,bottom = 190,right = 375)
    blurb <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(judges_xy))
    
    judges <- blurb %>%
        as.data.frame %>% 
        setNames(nm=c("judgeNo","namenat")) %>% 
        mutate(judgeName = str_sub(namenat, start=1, end=-7)) %>% 
        mutate(judgeNation = str_sub(namenat, start=-4,end=-2)) %>%
        select(-namenat)
    judges_wide <- judges %>% 
        gather(key=var, value=val, -judgeNo) %>% 
        mutate(var = str_c(var,judgeNo)) %>% 
        select(-judgeNo) %>% 
        spread(key=var, value=val)
    
    return(judges_wide)
}

###
### MAIN CODE SEGMENT:
### Which files are we scraping? PDFs from a folder below:
###
meta_pdfs <- data.table::fread(input=here("scrape/scraped-url-data.csv")) %>% 
    mutate(fileid = str_c(season,"-",eventid,"-",raceid,"-",codex,"-",filetype,".pdf"))

pdfs <- list.files(here("scrape/pdfs/"), pattern="201[56789].*.pdf")

competitions <- as.list(pdfs) %>% 
    map(get_compo_info) %>%
    setNames(nm=pdfs) %>% 
    bind_rows(.id="fileid") %>% 
    left_join(meta_pdfs, by="fileid")
head(competitions)

judgesets <- as.list(pdfs) %>% 
    map(get_judge_info) %>% 
    setNames(nm=pdfs) %>% 
    bind_rows(.id="fileid")
head(judgesets)

double_record <- c("2016-37454-4425-3773-RL1.pdf",
                   "2016-37467-4466-3782-RL1.pdf",
                   "2017-39342-4738-3817-RL1.pdf")
obscurities <- c("2017-39333-4713-3833-RL1.pdf",   # 4H weird format at the beginning of 2017
                 "2017-39331-4709-3831-RL1.pdf")

                 


competition_relevant <- left_join(competitions, judgesets, by="fileid")  %>% 
    # Remove some competitions we are currently unable to scrape
    # Team competitions
    filter(!str_detect(ctype, "Team")) %>% 
    # These have multiple record listings, and that functionality: added later.
    filter(!(fileid %in% double_record)) %>% 
    # Some formats are just deviant
    filter(!(fileid %in% obscurities)) %>% 
    # Recode some variables
    mutate(country = str_sub(place, start=-4,end=-2)) %>% 
    mutate(gender = if_else(str_detect(series, "LADIES"), "Ladies","Gents")) %>% 
    # Generate competition serial number
    mutate(componr = as.character(row_number()))


### MAIN PAGE FORMAT DEFINITIONS
###
### Variables deciding on the format:
###
### Type of competition : currently recognizes Individual and KO formats
### # of record listings: will be added later, these aren ow removed.
###
formats_KO <- list(
        points_xy = c(top = 223, left = 219,bottom = 679,right = 566),
        nationalities_xy = c(top = 223, left = 184,bottom = 676,right = 219),
        names_xy = c(top = 223, left = 70,bottom = 676,right = 181))
formats_Individual <- list(
        points_xy = c(top = 223, left = 234,bottom = 679,right = 570),
        nationalities_xy = c(top = 223, left = 196,bottom = 679,right = 231),
        names_xy = c(top = 223, left = 72,bottom = 679,right = 196))

###
### This function scrapes page 1 for results
###
get_results_page1 <- function(thetest) {

    thefile <- thetest$fileid
    theformat <- formats_Individual
    if (str_detect(thetest$ctype, " KO")) theformat <- formats_KO
    

    # Main points area
    pointsNames <- c("speed","length","lengthPoints","judgeA","judgeB","judgeC","judgeD","judgeE","stylePoints","startGate","gatePoints","windSpeed","windPoints","totalPoints")
    blurb_points <- tabulizer::extract_tables(file = here("scrape/pdfs",thefile), pages = 1, guess=F, area = list(theformat$points_xy)) %>% 
        .[[1]] %>% 
        as.data.frame
    if (ncol(blurb_points)==length(pointsNames)) {
        colnames(blurb_points) <-pointsNames
    } else {
        colnames(blurb_points) <- pointsNames[pointsNames!="gatePoints"]
        blurb_points <- mutate(blurb_points, gatePoints = NA)
    }
    blurb_points
    
    # Nationalities and dates of birth
    blurb_nationalities <- tabulizer::extract_tables(file = here("scrape/pdfs",thefile), pages = 1, guess=F, area = list(theformat$nationalities_xy)) %>% 
        .[[1]] %>% 
        as.data.frame %>% 
        mutate(r = row_number()) %>%
        mutate(sr = ((r-1) %% 2)) %>%
        mutate(mr = r-sr) %>%
        mutate(f = if_else(sr!=1, "competitorNation","competitorDoB")) %>% 
        group_by(mr) %>% 
        select(-r,-sr) %>% 
        spread(key=f, value=V1) %>% 
        ungroup %>% 
        select(-mr)
    
    # Names and clubs
    blurb_names <- tabulizer::extract_tables(file = here("scrape/pdfs",thefile), pages = 1, guess=F, area = list(theformat$names_xy)) %>% 
                      .[[1]] %>% 
                      as.data.frame %>% 
                      mutate(r = row_number()) %>%
                      mutate(sr = ((r-1) %% 2)) %>%
                      mutate(mr = r-sr) %>%
                      mutate(f = if_else(sr!=1, "competitorName","competitorClub")) %>% 
                      group_by(mr) %>% 
                      select(-r,-sr) %>% 
                      spread(key=f, value=V1) %>% 
                      ungroup %>% 
                      select(-mr)
    
    results_data <- bind_cols(blurb_names, blurb_nationalities, blurb_points)
    return(results_data)
}

###
### MAIN CODE SEGMENT: Pull out PDFs with competitions to be scraped
### Scrape them one by one, and collate results in df

### Scrape season 2016
###
season2016 <- competition_relevant %>% 
    filter(str_detect(date,"2016")) %>% 
    split(.,.$componr) %>% 
    map(get_results_page1) %>% 
    bind_rows(.id="componr") %>% 
    left_join(competition_relevant, by="componr")
data.table::fwrite(x = scrapes, file = here("scrape/scraped-data-season-2016.csv"))

