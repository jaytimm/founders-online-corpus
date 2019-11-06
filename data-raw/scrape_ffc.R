

library(tidyverse)
local <- '/home/jtimm/jt_work/GitHub/git_projects/founding-fathers-corpus/data-raw'
url <- 'founders-online-metadata.xml'

setwd(local)
xml2 <- xml2::read_xml(url) %>% xml2::xml_children()

x1 <- lapply(1:length(xml2), function(i) {
  #nb <- for (i in 1: length(xml2)) {
  zz <- xml2[[i]] %>% xml2::as_list()
  data.frame('title' = paste0(zz$title, ''),
             'permalink' = paste(zz$permalink),
             'project' = paste(zz$project),
             'authors' = paste0(unlist(zz$authors), collapse = ' | '),
             'recipients' = paste0(unlist(zz$recipients), collapse = ' | '),
             'date_from' = paste0(zz$`date-from`, ''),
             'date_to' = paste0(zz$`date-to`, ''),
             stringsAsFactors = FALSE)
})

x2 <- x1 %>% bind_rows()
x2$date_from <- as.Date(x2$date_from)
x2$date_to <- as.Date(x2$date_to)


base <- 'https://founders.archives.gov/API/docdata/'
x2$api <- paste0(base,
                 gsub(' .*$', '', x2$project), '/',
                 gsub('^.*/', '', x2$permalink))


clean_text <- function(x) {
  a1 <- strsplit(x, '\n')[[1]]
  a2 <- trimws(a1)
  a3 <- subset(a2, a2 != '')
  paste(a3, collapse = ' ')}


texts <- lapply(1:nrow(x2), function (y) {
  return (tryCatch(jsonlite::fromJSON(x2$api[y])$content,
                   error =function(e) NA,
                   finally =  print(paste0(y, ' / ', nrow(x2)))))  })

x2$og_text <- unlist(texts)
x2$text <-unlist(lapply(x2$og_text, clean_text))
#x3 <- x2 %>% filter(!is.na(og_text))

x3 <- x2 %>%
  mutate(period = case_when(
    date_from < as.Date('1775-04-19') ~ 'Colonial',
    date_from >= as.Date('1775-04-19') & date_from <= as.Date('1783-09-03') ~ 'Revolutionary War',
    date_from >= as.Date('1783-09-04') & date_from <=  as.Date('1789-04-29') ~ 'Confederation Period',
    date_from >= as.Date('1789-04-30') & date_from <=  as.Date('1797-03-03') ~ 'Washington Presidency',
    date_from >= as.Date('1797-03-04') & date_from <=  as.Date('1801-03-03') ~ 'Adams Presidency',
    date_from >= as.Date('1801-03-04') & date_from <=  as.Date('1809-03-03') ~ 'Jefferson Presidency',
    date_from >= as.Date('1809-03-04') & date_from <=  as.Date('1817-03-03') ~ 'Madison Presidency',
    date_from >= as.Date('1817-03-04') ~ 'post-Madison Presidency' ))


#setwd("/home/jtimm/jt_work/GitHub/packages/FoundersArchiveCorpus")
#devtools::use_data(founders_corpus, overwrite=TRUE)

x4 <- split(x3, x3$period)
names(x4) <- gsub(' ',  '_', names(x4))

lapply(1:length(x4), function(x) {

  write.csv(paste0(names(x4[[x]]), '.csv', row.names = FALSE))})
