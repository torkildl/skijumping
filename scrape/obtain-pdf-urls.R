##
## Download all the results files from FIS
##
library(tidyverse)
library(xml2)
library(httr)
library(rvest)
library(here)

###
### Function to generate list of a season's WC competitions
###
getSeason <- function(yr) {
    return(str_c("https://www.fis-ski.com/DB/general/event-details.html?eventselection=&place=&sectorcode=JP&seasoncode=", as.character(yr), "&categorycode=WC&disciplinecode=&gendercode=&racedate=&racecodex=&nationcode=&seasonmonth=X-", as.character(yr), "&saveselection=-1&seasonselection="))
}

###
### Function to scrape URLs for different events for one season, based on season list URL
###
scraplinks <- function(url){
    # Create an html document from the url
    webpage <- xml2::read_html(url)
    # Extract the URLs
    url_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")
    # Extract the link text
    link_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    return(tibble(link = link_, url = url_))
}

###
### Function to filter links scraped by scraplinks to get urls to season events
###
filterUrls <- function(links, ssn) {
    theUrls <- links %>% 
        filter(str_detect(url, "eventid=")) %>% 
        filter(str_detect(link, c(as.character(as.numeric(ssn-1)),as.character(ssn)))) %>% 
        select(url)
    return(theUrls)
}

###
### Function that produces a list of urls of season events from a season year argument
###
seasonEvents <- function(season) {
    getSeason(season) %>% 
        scraplinks %>% 
        filterUrls(.,ssn = season)
}

###
### Function that filters all URLs for one event into unique links for the races in that event
###
filterRaces <- function(links) {
    theUrls <- links %>% 
        filter(str_detect(url, "raceid=")) %>%
        filter(str_detect(url, "results.html")) %>% 
        select(url)
    return(theUrls)
}

###
### MAIN FUNCTION FOR GETTING ONE SEASON OF DATA
###
getURLdata <- function(season) {

    ###
    ### List of events generated from season summary calendar page
    ###
    events <- seasonEvents(season) %>% 
        mutate(eventid = str_sub(str_extract(url, "eventid=\\d+"),start=9))
        
    ###
    ### Segment to get races from events
    ###
    ###
    races <- events %>% 
        split(., .$eventid) %>% 
        map(function(x) scraplinks(x$url)) %>% 
        map(filterRaces) %>% 
        bind_rows(.id = "eventid") %>% 
        distinct %>% 
        mutate(raceid = str_sub(url, start=-4,end=-1)) %>% 
        filter(!is.na(as.numeric(raceid)))
    
    ###
    ### Segment to take urls for races and obtain links to PDFs with 
    ### results, start lists etc. Returns tibble of such urls with raceid included. 
    ### Codex is included in URL
    ###
    pdfLinks <- races %>% 
        mutate(url = str_c(url,"#down")) %>% 
        split(., .$raceid) %>% 
        map(function(x) pull(x,url)) %>% 
        map(as.character) %>% 
        map(scraplinks) %>% 
        bind_rows(.id="raceid") %>% 
        filter(str_detect(string = url, pattern = ".pdf")) %>% 
        select(-link)
    
    ###
    ### Collate a data set of PDF url with associated race and event IDs and filetypes.
    ###
    thedata <- events %>% 
        rename(event_url = url) %>% 
        left_join(races, by="eventid") %>% 
        rename(race_url = url) %>% 
        left_join(pdfLinks, by="raceid") %>% 
        rename(pdf_url = url) %>% 
        filter(!is.na(pdf_url)) %>% 
        mutate(codex = str_sub(pdf_url, start=38,end=41)) %>% 
        mutate(filetype = str_sub(str_extract(pdf_url, pattern = str_c("JP",codex,"\\S+\\.pdf")), start=7, end=-5))

    return(thedata)
}

###
### MAIN CODE SEGMENT: 
### Scrapes all URLs for all PDF files for all WC events for 2015-2019 seasons
###
seasons <- seq(2015,2019) 
allseasons <- seasons %>% 
    setNames(nm = as.character(seasons)) %>% 
    map(getURLdata) %>% 
    bind_rows(.id="season")
data.table::fwrite(allseasons, here("scrape/scraped-url-data.csv"))

