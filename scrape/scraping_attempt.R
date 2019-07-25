library(tabulizer)
library(pdftools)
library(magick)
library(tidyverse)
library(here)



### Read list of links scraped and downloaded
pdfs <- list.files(here("scrape/pdfs"), pattern="*.pdf") 
thefile <- sample(grep(pdfs, pattern="2016", value=T),1)

# # Convert first page to png
# dir.create(here("temp"))
# pdf_convert(here("newer-data/",thefile), pages=1, dpi=300, filenames = here("temp/firstpage.png"))
# # Cut out judge names
# firstpage <- image_read(here("temp/firstpage.png"))
#     judge_table <- image_crop(firstpage, "400x200+1000+560")
# image_write(judge_table, path = "temp/judgetable.png")
# system("convert temp/judgetable.png temp/judgetable.pdf")


### Pull out information about the judges from the PDFs
# This area is for the newest format
judges_xy <- c(top = 130, left = 236,bottom = 190,right = 375)
blurb <- tabulizer::extract_tables(file = here("scrape/pdfs/",thefile), pages = 1, guess=F, area = list(judges_xy))
blurb

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
judges
judges_wide

### Get first page results
first_results_xy <- c(top = 231, left = 28,bottom = 677,right = 567)
blurb <- tabulizer::extract_tables(file = here("scrape/pdfs",thefile), pages = 1, guess=F, area = list(first_results_xy))

thenames<-c("placement","number","competitor","competitorNation","speed","length","lengthPoints","judgeA","judgeB","judgeC","judgeD","judgeE","stylePoints","startGate","gatePoints","windSpeed","windPoints","totalPoints")
competitors <- blurb %>% 
    as.data.frame %>% 
    mutate(r = row_number()) %>% 
    mutate(sr = r %% 2) %>% 
    mutate(mr = r-sr) %>% 
    filter(sr==1) %>% 
    setNames(nm=thenames)

clubs_dates <- blurb %>% 
    as.data.frame %>% 
    select(X3,X4) %>% 
    mutate(r = row_number()) %>% 
    mutate(sr = r %% 2) %>% 
    mutate(mr = r-sr) %>% 
    filter(sr==0) %>% 
    setNames(nm=c("competitorClub","competitorDOB"))

competitorData <- bind_cols(competitors, clubs_dates)

### Get second page results
second_results_xy <- c(top = 134, left = 28,bottom = 687,right = 567)
blurb2 <- tabulizer::extract_tables(file = here("scrape/pdfs",thefile), pages = 2, guess=F, area = list(second_results_xy))
str(blurb2)

competitors2 <- blurb2 %>% 
    as.data.frame %>% 
    slice(-1) %>% 
    setNames(nm=thenames) %>% 
    mutate(r = row_number()) %>% 
    mutate(sr = r %% 2) %>% 
    mutate(mr = r-sr) %>% 
    filter(sr==1)

clubs_dates2 <- blurb2 %>% 
    as.data.frame %>% 
    slice(-1) %>% 
    select(X3,X4) %>% 
    setNames(nm=c("competitorClub","competitorDOB")) %>% 
    mutate(r = row_number()) %>% 
    mutate(sr = r %% 2) %>% 
    mutate(mr = r-sr) %>% 
    filter(sr==0)

competitorData2 <- bind_cols(competitors2, clubs_dates2) %>% 
    filter(!placement=="Did Not Start")

all_competitors <- bind_rows(competitorData, competitorData2)





