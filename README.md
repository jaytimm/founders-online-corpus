Founders Archives Corpus
------------------------

[Founders Online](https://founders.archives.gov/) is a [National
Archives](https://www.archives.gov/) resource that makes available
\~180K writings/letters of the Founders of the United States of America.
A treasure trove. Here, we simply extract (via
[API](https://founders.archives.gov/API/docdata/)) all
documents/metadata included in Founders Online, and collate them as a
collection of R-based `RDS` files.
[R-Script](https://github.com/jaytimm/founders_archive_corpus/blob/master/scrape_founders_archive.R).
[RDS
Files](https://github.com/jaytimm/founders_archive_corpus/tree/master/data).

------------------------------------------------------------------------

### § Load corpus

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(magrittr, dplyr, tidyr, ggplot2, data.table)
```

To get started, we aggregate the eight `RDS` files ([here]()) as a
single data frame.

``` r
setwd(local)
gfiles <- list.files(path = local, 
                     pattern = "rds", 
                     recursive = TRUE) 

ffc <- lapply(gfiles, readRDS) %>% data.table::rbindlist()
```

**Columns included in dataset are presented below.** The
`title`:`date_to` columns have been extracted directly from [document
metadata](https://founders.archives.gov/Metadata/) made available by
Founders Online as XML. The `api` column was derived from the
`permalink` and `project` columns, and used to access/scrape document
text. The `og_text` column contains document text as presented on
Founders Online, which includes quite a bit of line breaks and odd
spacing. The `text` column is a stripped version of `og_test`, with
whitespace and line breaks removed. These data are not perfect.

    ##  [1] "title"      "permalink"  "project"    "authors"    "recipients"
    ##  [6] "date_from"  "date_to"    "api"        "og_text"    "text"      
    ## [11] "period"

The `period` column categorizes each document in corpus historically.
Categories/dates are based on Founders Online. `RDS` files are split
according to these eight categories.

|    n| period                  | general   | start      | end        |
|----:|:------------------------|:----------|:-----------|:-----------|
|    1| Colonial                | 1706-1775 | 1706-01-01 | 1775-04-18 |
|    2| Revolutionary War       | 1775-1783 | 1775-04-19 | 1783-09-03 |
|    3| Confederation Period    | 1783-1789 | 1783-09-04 | 1789-04-29 |
|    4| Washington Presidency   | 1789-1797 | 1789-04-30 | 1797-03-03 |
|    5| Adams Presidency        | 1797-1801 | 1797-03-04 | 1801-03-03 |
|    6| Jefferson Presidency    | 1801-1809 | 1801-03-04 | 1809-03-03 |
|    7| Madison Presidency      | 1809-1817 | 1809-03-04 | 1817-03-03 |
|    8| post-Madison Presidency | 1817+     | 1817-03-04 | 1837-01-01 |

### § Some descriptives

The table below summarizes the number of documents and number of words
per corpus sub-period. Note that word counts are very rough, and that a
single document may be double counted. (see Founders Online for an
explanation). So, \~180k document/66 million word corpus.

``` r
data.table::setDT(ffc)

ffc[, doc_length := lengths(gregexpr("\\W+", text))]

by_period <-ffc[, list(doc_n = .N, 
                       word_count = sum(doc_length)),
                by = list(period)]

by_period %>% 
  left_join(Period_table %>% select(n, period)) %>% 
  arrange(n) %>% 
  janitor::adorn_totals(c('row')) %>%
  mutate_if(is.numeric, formatC, big.mark=",") %>%
  select(-n) %>%
  knitr::kable()
```

| period                  | doc\_n  | word\_count |
|:------------------------|:--------|:------------|
| Colonial                | 16,111  | 5,569,488   |
| Revolutionary War       | 48,502  | 16,255,296  |
| Confederation Period    | 17,024  | 7,184,958   |
| Washington Presidency   | 27,531  | 10,435,172  |
| Adams Presidency        | 13,743  | 4,351,329   |
| Jefferson Presidency    | 28,854  | 10,883,697  |
| Madison Presidency      | 15,094  | 6,468,738   |
| post-Madison Presidency | 14,618  | 5,026,013   |
| Total                   | 181,477 | 66,174,691  |

The plot below summarizes document generation by month for the six major
Founders over a \~eighty-year time period.

``` r
founders <- c('Washington, George', 'Adams, John', 'Jefferson, Thomas', 
              'Madison, James', 'Hamilton, Alexander', 'Franklin, Benjamin')
```

``` r
ffc$Month_Yr <- sub('-[0-9]*$', '', ffc$date_from)
ffc$Month_Yr <- as.Date(paste0(ffc$Month_Yr, '-01'), format = '%Y-%m-%d')
  
ffc[, list(doc_n = .N), 
           by = list(Month_Yr,authors)] %>%
  filter(authors %in% founders) %>%
  filter(Month_Yr < as.Date('1830-01-01'),
         Month_Yr > as.Date('1750-01-01')) %>%
  
  ggplot(aes(x = Month_Yr, 
             y = doc_n, 
             color = authors,
             group = authors)) +
  geom_line(size=.5) +
  geom_vline(xintercept = Period_table$start,
             linetype =2, 
             color = 'black', 
             size = .25) +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab ("") + xlab("") +
  scale_x_date(labels = scales::date_format("%Y"),
               breaks = scales::date_breaks('10 year')) +
  facet_wrap(~authors, 
             scales = "free_y", 
             ncol=2) + 
  labs(title = "Founders' writings historically by month",
       subtitle = 'From 1750 to 1830')
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)

### § Exploring text

A simple function for displaying Founder documents & meta inline:

``` r
display_letter <- function(x) {
  a1 <- strsplit(x$og_text, '\n')[[1]]
  a2 <- trimws(a1)
  a3 <- subset(a2, a2 != '')
  a4 <- paste(a3, collapse = '\n')
  a4a <-paste0('Author: ', x$author, '\n>', 
               'Recipient: ', x$recipient, '\n>', 
               'Date: ', x$date_to, '\n>', 
               'Period: ', x$period, '\n>\n', 
               a4)
  a5 <- gsub(' *\n', '  \n', a4a)
  paste('>', gsub(' *(\n*) *$', '\\1', a5))
}
#`r display_letter(ffc[150681,])`
```

A letter from **Thomas Jefferson to Benjamin Franklin** with an
“inclosed paper” – presumably a draft of the *Declaration of
Independence*.

> Author: Jefferson, Thomas  
> Recipient: Franklin, Benjamin  
> Date: 1776-06-21  
> Period: Revolutionary War
>
> Th: J. to Doctr. Franklyn  
> Friday morn. \[21 June 1776?\]  
> The inclosed paper has been read and with some small alterations
> approved of by the committee. Will Doctr. Franklyn be so good as to
> peruse it and suggest such alterations as his more enlarged view of
> the subject will dictate? The paper having been returned to me to
> change a particular sentiment or two, I propose laying it again before
> the committee tomorrow morning, if Doctr. Franklyn can think of it
> before that time. <br>

A corrrespondance from **George Washington to Benjamin Tallmadge**
discussing a couple of Washington’s spies, the Culpers.

> Author: Washington, George  
> Recipient: Tallmadge, Benjamin  
> Date: 1780-07-11  
> Period: Revolutionary War
>
> Dear Sir  
> Head Quarters Bergen County 11th July 1780  
> As we may every moment expect the arrival of the French Fleet a
> revival of the correspondence with the  
> Culpers will be of very great importance. If the younger cannot be
> engaged again, you will endeavour to prevail upon the  
> elder to give you information of the movements and position of the
> enemy upon Long Island—as whether they are all  
> confined to the post at Brooklyn or whether they have any detached
> posts and where, and what is their strength at those  
> posts—in short desire him to inform you of whatever comes under his
> notice and which seems worthy of communication. You  
> will transmit your letters to Genl Howe, who will forward them to me.
> I am, &c.  
> P.S. desire him to attend particularly to the provision which they are
> making of Wood and Forage—and  
> whether they drive in any stock.

### § Caveats
