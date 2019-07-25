##
## Download all the results files from FIS
## URLs and other info is read from file.
##
library(tidyverse)
library(xml2)
library(httr)
library(rvest)
library(here)

### Read in scraped URLs 
allseasons <- data.table::fread(here("scrape/scraped-url-data.csv")) %>% 
    mutate(fileid = str_c(season,"-",eventid,"-",raceid,"-",codex,"-",filetype,".pdf"))
summary(allseasons)

targets <- allseasons %>% 
    filter(filetype=="RL1")

### Create directory structure
###
if (!dir.exists(here("scrape/pdfs"))) dir.create(here("scrape/pdfs"))

downloadPDFsafe <- function(desc) {
    destpath <- here(str_c("scrape/pdfs/",desc$fileid))
    if (file.exists(destpath) && file.size(destpath)>0) {
        warning(str_c("File ", desc$fileid, " already exists. Skipping!"))
    } else {
        download.file(url = desc$pdf_url, destfile = destpath, timeout=5)
        Sys.sleep(5)
    }
}

targets %>% 
    select(pdf_url, fileid) %>% 
    mutate(n = row_number()) %>% 
    split(.,.$n) %>% 
    map(downloadPDFsafe)
