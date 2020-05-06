################################
# Load libraries used
################################

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(quanteda)) install.packages("quanteda", repos = "http://cran.us.r-project.org")
if(!require(readtext)) install.packages("readtext", repos = "http://cran.us.r-project.org")
if(!require(quanteda.textmodels)) install.packages("quanteda.textmodels", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(topicmodels)) install.packages("topicmodels", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")


################################
# Download file
################################



# Read in the the txt file with the data for analysis. This data set will 
# be transformed into a corpus of texts for each debate. Training and test sets 
# for analysis will be created from the the resulting corpus later in the script

    debates <- read_delim("CYO.d42pc.txt", delim = "|")

# Assess the number of people speaking by Party Affiliation.
    debates %>%
      group_by(personSpeakingAffiliation) %>%
      count() 

# Anomolies were noted where proper names are recorded as Party Affiliation.  The text reveals these
# are records of nominating persons for election of Speaker of the House.          
    inspctAnmlys <- debates %>%
      filter(str_detect(personSpeakingAffiliation, c("Charles"))) 
    inspctAnmlys$speakingContent # take a look at the record and we see it is election of speaker, not debate

# Removing records for nomination of Speakers. 
    debates <- debates %>%
      filter(!(str_detect(personSpeakingAffiliation, c("Charles")))) # remove the entry 

# A little more detailed summary of the records by looking at the counts of persons by Party Affiliation
    psa.counts <- debates %>%
      group_by(personSpeaking, personSpeakingAffiliation) %>%
      tally(sort = T)
    psa.counts
      
# Add in a column to identify the Parliament based on the date of the person speaking
# If the date falls in the range assign corresponding parl else check the next.  Default is the last Parlaiment in data 
        debates <- debates %>%
      mutate(parl = ifelse(between(personSpeakingDate,"2001-01-29", "2004-05-23"),"37", 
        ifelse(between(personSpeakingDate,"2004-10-04", "2005-11-29"),"38",
        ifelse(between(personSpeakingDate,"2006-04-03", "2008-09-07"),"39",
        ifelse(between(personSpeakingDate,"2008-11-18", "2011-03-26"),"40",
        ifelse(between(personSpeakingDate,"2011-06-02", "2015-08-02"),"41",
        ifelse(between(personSpeakingDate,"2015-12-03", "2019-09-11"),"42",
        "43")))))))
  
# how many debates are avaiale in the data for each parliment
    debates %>%
      group_by(parl) %>%
      count()

# Only a few of the debates from the 41st and 43rd Parliament  are in the data.
# We will focus on Parliament 42 for the text analysis

    debates <- debates %>%
      filter(parl == 42) # filter for 42nd Parlaiment

# Who speaks the  most?
    psa.counts <- debates %>%
      group_by(personSpeaking, personSpeakingAffiliation) %>%
      tally(sort = T)
    psa.counts

# Geoff Regan served as Speaker of the House in the 42nd Parliament.  
# The role of Speaker is to preside over the house of
# commons and not to be engaged in debate.  Filter to removing the records 
# where Geoff Regan is the person speaking. 
    debates <- debates %>%
      filter(personSpeaking != "Hon. Geoff Regan")

# Tally occurances of personSpeaking one more time and show the results in a plot
    psa.counts <- debates %>%
      group_by(personSpeaking, personSpeakingAffiliation) %>%
      tally()
  psa.samp <- head(psa.counts,7) # store 7 results to use in report
    

# plot the occurance of personSpeaking 
    p <- psa.counts %>%
      ggplot(aes(x=personSpeakingAffiliation, y=log(n), color = personSpeakingAffiliation, label = personSpeaking )) +
      geom_point(alpha = 0.25) +
      theme_few() +
      ggtitle("Occurances of Individuals Speaking by Party Affiliation \n42nd Parliament of Canada")
    p
    
# Mean of occurances of individuals speaking
    speakingstats <- summary(psa.counts$n)
    

# select the columns of interest for the analysis that will be performed.
# for this analysis the columns of party affiliation, speaking content, speaking date, personSpeaking and parl
# is of interest
    CYO.Debates <- debates %>%
      select(c("personSpeakingAffiliation","speakingContent", "personSpeakingDate", "personSpeaking", "parl"))

# It will be more effecient to use quanteda's readtext to read the data in for use in quanteda as docvars can be set at read time.
# rather than manipulating the df for quanteda. Using tab seperated values to avoid odd reslts from commas
# that are contained within the personSpeaking text.    

# export CYO.Debates to a tab seperated values file.
    write_tsv(CYO.Debates, "CYO.Debates.tsv")

# clean up the enviroment area
#    rm(list = ls(all.names = T))
  
###############################
# Quantitative analysis of text with Quanteda package
###############################  
  
  
# Read the data that will be used for analysis
    debates <- readtext("CYO.Debates.tsv", 
      text_field = "speakingContent",
      docvarnames = c("personSpeakingAffiliation",
      "personSpeakingDate",
      "personSpeaking",
      "parl"))

# Create a corpus of the texts using quanteda corpus
    corp_debates <- corpus(debates) 

    
###############################
# Create a training and test sets
###############################  
    
# generate test id numbers without replacement
    set.seed(1450)
    id_test <- sample(1:114101, 11410, replace = FALSE)
    
# create docvar with ID
    corp_debates$id_numeric <- 1:ndoc(corp_debates)

# 90% training set (documents not in id_test).  
    corp_training <- corpus_subset(corp_debates, !id_numeric %in% id_test)

# Training Data Frame Matrix      
    dfmat_training <- corp_training %>%
      dfm(remove = stopwords("english"), 
          stem = F,
          remove_punct = T,
          remove_symbols = T,
          remove_numbers = T,
          include_docvars = T)
    
# 10% test set (documents in id_test) # Using pipe we can create the corpus and data frame matrix in one pass
    corp_test <- corpus_subset(corp_debates, id_numeric %in% id_test)

# Test Data Frame Matrix              
    dfmat_test  <- corp_test %>%
      dfm(remove = stopwords("english"), 
          stem = F,
          remove_punct = T,
          remove_symbols = T,
          remove_numbers = T,
          include_docvars = T)      

# Explore training data - Wordclouds give a visual sense of word occurance
# Apply dfm_trim to issolate the top 20% of reoccuring words
    dfmat_debates_wrdcld <- dfm_trim(dfmat_training,
                            #min_termfreq = 0.8,
                            #max_docfreq = 5000,
                            termfreq_type = "rank")
                   
  
    textplot_wordcloud(dfmat_debates_wrdcld,
                    #min_size = 2,
                    max_words = 150,
                    random_color = T,
                    color = c("blue", "red", "orange", "darkblue", "gray45","olivedrab4" ))


        
## top terms frequency        

dfm_debates_features <- textstat_frequency(dfmat_training)
# Sort by reverse frequency order
dfm_debates_features$feature <- with(dfm_debates_features, reorder(feature, -frequency))

ggplot(dfm_debates_features[1:50,], aes(x = feature, y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Words by party affiliation
dfm_debates_pty_prop <- dfmat_training %>%
    dfm_weight(scheme = "prop")

# Calculate relative frequency by party
freq_weight <- textstat_frequency(dfm_debates_pty_prop, n = 15, groups = "personSpeakingAffiliation")

ggplot(data = freq_weight, aes(x = nrow(freq_weight):1, y = frequency)) +
  geom_point() +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_weight):1,
                     labels = freq_weight$feature) +
  labs(x = NULL, y = "Relative frequency")



## Similarity between people speaking
## Cluster persons speaking by occurence of terms 1K to 2k times
  dfmat_speaking_sim <- dfmat_training %>%
    dfm(remove = stopwords('en'))  

  dfmat_speaking_simlty <- dfm_group(dfmat_training, groups = c('personSpeakingAffiliation','personSpeaking'))

  dfmat_speaking_simlty <- dfmat_speaking_simlty %>% 
    dfm_select(min_nchar = 4) %>%  # minimum characters in term
    dfm_trim(min_termfreq = 1000) %>% #mimimum times the term occurs accross all documents
    dfm_trim(max_termfreq = 2000)  # maximum times the term occurs accross all documents

  dfmat_speaking_simlty <- dfmat_speaking_simlty[ntoken(dfmat_speaking_simlty) > 1499,] # minimum total tokens

  tstat_dist <- as.dist(textstat_dist(dfmat_speaking_simlty))
  speaking_clust <- hclust(tstat_dist)
  plot(speaking_clust)



# Calculate relative frequency by ranking members
#   House Leaders - Dominic Leblanc, Bardish Chagger
#   Opposition House Leaders - Andrew Scheer, Candice Bergen

# Subset corpus to texts of the house and house opposition leaders
    corp_debates_leaders <- corpus_subset(corp_training,
                                       personSpeaking == c("Hon. Dominic LeBlanc", "Hon. Andrew Scheer", "Hon. Bardish Chagger","Hon. Candice Bergen")) 

    dfm_debates_leaders <- corp_debates_leaders %>%
      dfm(remove = stopwords('en'), 
      remove_punct = T,
      remove_symbols = T,
      remove_numbers = T,
      include_docvars = T)

    dfm_debates_leaders_prop <- dfm_debates_leaders %>%
      dfm_weight(scheme = "propmax")

    freq_weight_leaders <- textstat_frequency(dfm_debates_leaders_prop, n = 25, groups = c("personSpeakingAffiliation", "personSpeaking"))

    ggplot(data = freq_weight_leaders, aes(x = nrow(freq_weight_leaders):1, y = frequency)) +
      geom_point() +
      facet_wrap(~ group, scales = "free") +
      coord_flip() +
      scale_x_continuous(breaks = nrow(freq_weight_leaders):1,
                   labels = freq_weight_leaders$feature) +
      labs(x = NULL, y = "Relative frequency")+
      theme_few()


## Key Words in Context

# update docnames to name of speaker for convenience
    docnames(corp_debates_leaders) <- str_sub(corp_debates_leaders$personSpeaking, start = 6)

    toks_debates_leaders <- tokens(corp_debates_leaders,
                                what = "word",
                                include_docvars = T)

# key words in context
    kwic_debates_leaders <- as.data.frame(kwic(toks_debates_leaders,
                              pattern = c("finance","working"),
                              window = 5))


###################
# Classification of Documents
##################


# Train N-Bayes classifier
    mod_niave <- textmodel_nb(dfmat_training, dfmat_training$personSpeakingAffiliation, 
                              prior = "uniform", 
                              distribution = "multinomial")
    summary(mod_niave)

    dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

    actual_class <- dfmat_matched$personSpeakingAffiliation
    predicted_class <- predict(mod_niave, newdata = dfmat_matched)
    tab_class <- table(actual_class, predicted_class)
    tab_class
    cnfmtrx <- confusionMatrix(tab_class, mode = "everything")
    cnfmtrx

# heatmap of confusion matrix

# Save confusion matrix as data frame
    confusion.data <- as.data.frame(cnfmtrx[["table"]])

# Reverse the order
    level_order_y <-
      factor(confusion.data$actual_class,
      level = c('BQ', 'CCF', 'CPC', 'GP', 'Ind.', 'Lib.', 'NDP', 'PPC'))

    ggplot(confusion.data,
      aes(x = predicted_class, y = level_order_y, fill = Freq)) +
      xlab("Predicted class") +
    ylab("Actual class") +
    geom_tile() + theme_bw() + coord_equal() +
    scale_fill_distiller(palette = "Paired", direction = 1) +
    scale_x_discrete(labels = c("BQ", "CCF", "CPC", "GP", "Ind.", "Lib.", "NDP", "PPC")) +
    scale_y_discrete(labels = c("BQ", "CCF", "CPC", "GP", "Ind.", "Lib.", "NDP", "PPC")) +
        ggtitle("Confusion Matrix Heatmap\nNaive Bayes Document Classification (training) \nDebates of the 42nd Parliament of Canada (training set)")


# Sentiment Analysis
# Tokens
    toks_training <- tokens(corp_training, 
                            remove_punct = T,
                            remove_symbols = T,
                            remove_numbers = T,
                            include_docvars = T)
    toks_training_lsd <- tokens_lookup(toks_training, 
                                       dictionary =  data_dictionary_LSD2015[1:2])
 dfmat_training_ptystmnt <-  dfm(toks_training_lsd, tolower = T, groups = "personSpeakingAffiliation")
 
 senment_prty <- dfmat_training_ptystmnt %>%
   convert(to = "data.frame") %>%
   group_by(document) %>%
   mutate(pos = sum(positive /(negative + positive)))
 
 dfmat_training_prsnstmnt <-  dfm(toks_training_lsd, tolower = T, groups = "personSpeaking")

 head(dfmat_training_prsnstmnt)
 