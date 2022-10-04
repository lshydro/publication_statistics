###########################################################-
# Objective: Visualise publication and references statistics 
# of Hydrologic Journals
# Author: Lina Stein, University of Potsdam
# note: 06.04.2022
###########################################################-


library(fulltext)
library(rcrossref)
library(tidyverse)
library(patchwork)


#Rcrossref
#Added email to the polite pool 
#file.edit("~/Renviron")
#crossref_email= "name@example.com"



AWR_issn = "03091708"
HESS_issn_online = "16077938"
HESS_issn_print = "10275606" 
HP_issn_online = "10991085"
HP_issn_print = "08856087"
HSJ_issn = "02626667"
JoH_issn = "00221694"
Water_issn = "20734441"
WIREsW_issn <- "20491948"
WRR_issn_online = "19447973"
WRR_issn_print = "00431397"

list_issn = list(AWR_issn, HESS_issn_online, HESS_issn_print, HP_issn_online, HP_issn_print, HSJ_issn, JoH_issn, Water_issn, WIREsW_issn, WRR_issn_online, WRR_issn_print)


#load publications data from crossref

s_time = Sys.time()
hydro_publications_list = lapply(list_issn, function(temp_issn){
  #https://ciakovx.github.io/rcrossref.html#Getting_publications_from_journals_with_cr_journals
  #Get journal information
  journal_details = rcrossref::cr_journals(issn = temp_issn, works = FALSE) %>%  purrr::pluck("data")
  totaldoi = journal_details$total_dois
  
  #Get articles associated with that journal
  total_publications <- cr_journals(issn = temp_issn, works = TRUE, cursor = "*", cursor_max = totaldoi, limit = 1000) %>%
    purrr::pluck("data")
  return(total_publications)
  Sys.sleep(5)
})
e_time = Sys.time()
e_time-s_time
save(hydro_publications_list, file = "hydro_publications_list.Rdata")

load(file = "hydro_publications_list.Rdata")


filter_type = c("journal-article", "proceedings-article")
patterns_excl <- c("editor", "correction", "foreword", "preface", "issue information", "peer reviewers", "reply", "Reviews and Abstracts", "Cover Image", "Calendar")

clean_hydro_pub_list = lapply(hydro_publications_list[!unlist(lapply(hydro_publications_list, is.null))], function(in_list){
  out = in_list %>%
    filter(type %in% filter_type) %>% #filter for entry type 
    distinct(doi, .keep_all = TRUE) %>% #filter duplicates
    filter(!grepl(paste(patterns_excl, collapse="|"), title, ignore.case = T)) %>% #remove exclude patterns
    filter(!is.na(title)) %>%
    mutate(pub_year = as.integer(substr(issued, 1, 4))) %>% #integer publication year
    mutate(years_since_pub = 2020-pub_year) %>% #years since publications
    mutate(cited_by_count = as.numeric(is.referenced.by.count)) %>%
    mutate(cites_count = as.numeric(reference.count)) %>%
    mutate(citation_stats = ifelse(cited_by_count>=years_since_pub, "high", "low")) %>%
    select(container.title, title, doi, pub_year, cited_by_count, cites_count)
  
  return(out)
})

hydro_pub_df = do.call(rbind, clean_hydro_pub_list)

#shorten journal name
hydro_pub_df$container.title[hydro_pub_df$container.title == "Wiley Interdisciplinary Reviews: Water"] = "WIREs Water"

#exclude incomplete year 2022
hydro_pub_df_summary = hydro_pub_df %>%
  count(pub_year, container.title) %>%
  filter(pub_year !=2022) %>%
  filter(container.title !="Deleted DOIs")

#add missing years as zero values to avoid gaps in the stack area plot. Added data does not show in the plot.
hydro_pub_df_summary = complete(hydro_pub_df_summary, container.title, pub_year, fill = list(n = 0))


#remove all articles that cite zero others, since they are likely missing citation information and would distort the mean.  
hydro_cites = hydro_pub_df %>%
  filter(cites_count !=0) %>%
  filter(pub_year !=2022) %>%
  filter(container.title !="Deleted DOIs")%>%
  mutate(pub_year_fact = as.factor(pub_year))%>% #factor level for stats plotting
  mutate(pub_year_fact = fct_expand(pub_year_fact, "1960", "1961", "1962"))%>% #extend x axis to 1960
  mutate(pub_year_fact = factor(pub_year_fact, levels(pub_year_fact)[c(60:62,1:59)]))#reorder factor 


#Visualisation of the data
outlier_func = function(x) {
  subset(x, x < quantile(x,0.1) | quantile(x,0.9) < x)
}

#Colour vision deficiency safe colour palette by Paul Tol
tol8qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")


#Plot publication numbers
p1 = ggplot(hydro_pub_df_summary, aes(x=pub_year, y=n, fill=container.title)) + geom_area() + theme_minimal()+xlab("Publication year")+ylab("Articles published per year")+scale_fill_manual(values=tol8qualitative, name = "Journal")+xlim(1960,2021)+theme(text = element_text(size = 14),legend.position = c(0.4, 0.6), panel.grid = element_blank())


#plot reference statistics
p2 = ggplot(hydro_cites, aes(pub_year_fact, cites_count)) + geom_boxplot(outlier.color = NA, width = 0.3)+ylim(0,125)+xlab("Publication year")+ylab("References per article")+theme_bw()+theme(text = element_text(size = 16), panel.grid = element_blank())+scale_x_discrete(breaks = c("1960", "1980", "2000", "2020"), drop=F)+ scale_y_continuous(expand = c(0, 0), limits = c(0,130))

p1 + p2 + plot_layout(nrow = 1)+ plot_annotation(tag_levels = 'a')
ggsave(file = "Articles_referenced_over_time_hydro_comb.pdf",dpi = "retina", width = 12, height = 5 )










