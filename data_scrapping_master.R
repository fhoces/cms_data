#############################
#### Script to download ALL Data from: Medicare Advantage/Part D Contract and Enrollment Data
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/index.html
#### Date: 04072017
#### Author: Fernando Hoces de la Guardia
start.time <- Sys.time()

## Write here the folder where you want to download the data
MY.PATH <- "/Users/fhocesde/Documents/VBID_data/"

## read in the CMS website that cotains links to all the pages where we want to download data
raw_lines <- readLines("https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/index.html")

#Here is where the list of webpages begins
patern1 <- "<h2><a href=\"/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/MCRAdvPartDEnrolData/index.html\">Medicare Advantage/Part D Contract and Enrollment Data</a></h2>"

#Statrting from the line above (in the html raw file) identify where the list of urls ends (first time "</ul>" appears)
extract_lines_1 <- grep(patern1, raw_lines)
extract_lines_2 <- min( grep("</ul>", 
                             raw_lines[extract_lines_1:length(raw_lines)], fixed = TRUE) ) 

#Reduce the raw file to the section that only contains the urls of interest
raw_lines1 <- raw_lines[extract_lines_1:(extract_lines_2 + extract_lines_1 - 1)]
#Keep only links, and delete the first (main) link
raw_lines2 <- grep("*html", raw_lines1, value = TRUE)[-1]


## clean the extracted lines and turn each line in to a proper link and title
clean_lines <- sub('(.*href=\")(.*.html)(.*)', "\\2", raw_lines2)

#links
data.web.pages.main <- paste("https://www.cms.gov", clean_lines, sep="")
#titles (to be used as folder names)
data.titles <- sub('(.*title=\")(.*?)(\" href.*)', "\\2", raw_lines2)


## The "scrape.zipfiles" function takes a url as input and download the desired .zip file in that url
## it will be used within each sub page 
scrape.zipfiles <- function(x) {
  ## read in the url
  raw_lines1 <- readLines(x)
  ## identify the pattern and extract the lines that contain the pattern (of the zip file)
  pattern1_1 <- "Downloads.*Full.*[0-9][0-9][0-9][0-9].*.zip"
  pattern1_2 <- "Downloads.*[0-9][0-9][0-9][0-9].*.zip.*Full"
  pattern1_3 <- "Downloads.*Alt.*[0-9][0-9][0-9][0-9].*.zip"
  pattern1_4 <- "Downloads.*.zip"
  extract_lines1_1 <- grepl(pattern1_1, raw_lines1)
  extract_lines1_2 <- grepl(pattern1_2, raw_lines1)
  extract_lines1_3 <- grepl(pattern1_3, raw_lines1)  
  extract_lines1_4 <- grepl(pattern1_4, raw_lines1)  
  matched.lines1 <- raw_lines1[(extract_lines1_1 | extract_lines1_2 | extract_lines1_4) & !extract_lines1_3]
  ## clean the extracted lines
  clean_lines1 <- sub('(.*href=\")(.*.zip)(.*)', "\\2", matched.lines1)
  clean_lines1 <- paste("https://www.cms.gov", clean_lines1, sep="")
  
  ## define the filename and path where you want to download the zip file
  file.name <- sub("(.*Downloads/)(.*.zip)(.*)","\\2",clean_lines1)
  ## some fileames contain subdirectories but you just want all in the same folder
  file.name <- gsub("[ / ]", "_", file.name)
  
  file.name <- paste(MY.PATH1, file.name, sep="")
  
  ## download the zip files
  download.file(clean_lines1,file.name)
}

#Now for each of the main pages (24) we need to run a funciton that calls each set of sub pages
i <- 0
for (link.to.scrape in data.web.pages.main) 
{
  i <- i + 1
  #Here I get the name of the folder where I will save all the zip files
  aux.title <- data.titles[i]
  clean.title <- gsub(" ",replacement = "_" , aux.title)
  clean.title <- gsub("/",replacement = "_" , clean.title)
  
  #Call the main url, and find the list sub urls where I scrape the zip files
  raw_lines1 <- readLines(link.to.scrape)
  extract_lines_1 <- min( grep("<tr>", 
                               raw_lines1, fixed = TRUE) ) 
  extract_lines_2 <- max( grep("</tr>", 
                               raw_lines1, fixed = TRUE) ) 
  #Keep only the lines with links in it and clean them
  raw_lines1 <- raw_lines1[extract_lines_1:extract_lines_2]
  raw_lines2 <- grep("*html", raw_lines1, value = TRUE)
  clean_lines <- sub('(.*href=\")(.*.html)(.*)', "\\2", raw_lines2)
  # For each main web page (24), the list below has the all "sub-websites" to scrape (~100 each) 
  data.web.pages.sub <- paste("https://www.cms.gov", clean_lines, sep="")
  
  # Create a folder and redefine path to that folder
  dir.create(file.path(MY.PATH, clean.title), showWarnings = FALSE)
  MY.PATH1 <-paste(MY.PATH,clean.title,"/",  sep="" )
  
  #Scrape each "sub-website" and get the only zipfile in each (containing the data)
  sapply(data.web.pages.sub, scrape.zipfiles)
}





# Tracking time of execution
print(Sys.time() - start.time)
