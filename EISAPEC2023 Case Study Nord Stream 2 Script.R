# FernUniversity in Hagen
# Institute of Political Science
# Chair: Political Science II - International Politics
# Author: Tim Paul Broszio



### Let's get started!----


getwd()

sessionInfo()




# cleaning workspace/ environment
rm(list = ls())

### Loading relevant packages----

pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("quanteda", "wordcloud2", "tidyverse", "tidyr", "readr", "RColorBrewer", "ggthemes",
       "readr", "tm", "extrafont", "readtext", "lubridate", "wordcloud", 
       "udpipe", "scales", "googlenlp", "spacyr", "quanteda.textstats",
       "quanteda.textplots", "topicmodels", "ldatuning", "stm", "spacyr", 
       "remotes", "LDAvis", "reshape2", "stmCorrViz", "servr")

set.seed(seed = 321)


#
#### Case Study Nord Stream 2----
# 

# loading the Corpus (saved in a R.da file)

file.choose()


load("D:\\R\\Scripte\\SZ und FAZ Artikel China Textmining!\\Zeitungsmining\\Corpusgesamt.Rda")

### subset the research periods

Artikel18 <- corpus_subset(Artikel, Year == 2018)

Artikel18shorttext <- substr(Artikel18, 1, 200)

save(Artikel18, file = "CorpusNordStream18.Rda")


Artikel21 <- corpus_subset(Artikel, Year == 2021)

Artikel21shorttext <- substr(Artikel21, 1, 200)

save(Artikel21, file = "CorpusNordStream21.Rda")


Artikel22 <- corpus_subset(Artikel, Year == 2022)

Artikel22shorttext <- substr(Artikel22, 1, 200)

save(Artikel22, file = "CorpusNordStream22.Rda")




### Data Preprocessing-----

# Stopwords

ToBeRemoved <- c("reuters", "dpa", "afp", "china", "chin*", 
                 "siehe", "th", "dass", "sei", "mehr", "ab",
                 "sagte", "worden", "jahr", "seit", "schon", "p.k", 
                 "bloomberg", "montag", "dienstag", "mittwoch", "donnerstag", 
                 "freitag", "samstag", "sonntag", "januar", "februar", 
                 "märz", "april", "mai", "juni", "juli", "august", "september", 
                 "november", "dezember", "w", "boe", "dm", "wegen", "amerika*", 
                 "peking", "dafür", "ende", "berlin", "steht", "macht", 
                 "jedoch", "müssen", "sagt", "land", "wurde", "usa",
                 "ap", "peking*", "taz", "sz", "faz", "etwa", "seien",
                 "deutsch*", "washington", "berlin", "heute", "gestern",
                 "seite", "neu*", "immer", "prozent", "of", "deshalb", 
                 "künftig", "woche", "könn*", "künft*", "offenbar", "hätt*", 
                 "zeit", "vergang*", "große*", "angaben", "jahre*", "teil", 
                 "asien", "insel", "welt", "mark", "kp", "million*", "muß", "c", 
                 "süddeutschen zeitung", "f.a.z_frankfurt", "f.a.z", "f", "a", "z",
                 "frankfurt", "darüber", "hinaus", "de", "hob", "hervor", 
                 "treffen", "solle", "ging", "gebe", "alten", "hieß", "wäre", 
                 "weiß", "wochen", "fest", "alte", "höhe", "trotz", "new", "york",
                 "fünf", "groß", "rede", "zahl", "hält", "lesedauer", "veröffentlicht",
                 "minuten", "quelle", "anzeige", "lesen", "getty", "image", 
                 "images", "beziehungen", "dollar",
                 "euro", "schanghai", "kilometer", "lange", "größten", "gegenüber", 
                 "stehen", "frage", "zurück", "deutlich", "heißt", 
                 "vereinigt*", "staaten", "amerika", "minister", "us*", 
                 "schließlich", "tatsächlich", "tag", "kurz", "lä*", "la*", "zuvor", 
                 "hamburg", "außenminist*", "zeitung", "münchen", "regie*", "russ*", 
                 "bundesregierung", "präsident", "spd", "cdu", "fdp", "wenige", 
                 "pipeline", "gaspipeline", "anmerkungen", "redaktion", "ukrain*", 
                 "europ*", "moskau", "berlin", "brüssel", "frankreich", "präsid*", "staat", 
                 "sankt", "petersburg", "st", "st_petersburg", "sankt_petersburg")

##

Namen <- c("felix", "lee", "johnny", "erling",
                "nikolaus", "doll", "nikolaus", "doll", "friedericke", "böge",
                "christo", "hardebusch", "nora", "sausmikat", "nora", "sausmikat", 
                "sven", "hansen", "sven", "hansen", "jones", "bos", "jones", "bos",
                "kai", "strittmatter", "kai", "strittmatter", "petra", "kolonko","petra",
                "georg", "blume", "eric bonse", "britta", "petersen", "britta", 
                "petersen", "peter", "weissenburger", "frank", "stocker", "sophie", "mühlmann", 
                "pavel", "lokshin", "johnny_erling", "johnny", "erling", "julia", "boxler",
                "josep", "borrel", "horst", "seehof", "stefani", "schiffer", "christoph", "nil", "schmid", 
                "eric bons", "olaf", "scholz", "kanzler", "scholz", "bundeskanzler", "scholz", "angela", "merkel", "scholz",
                "merkel", "bundeskanzlerin", "merkel", "kanzlerin", "merkel", "wladimir", "putin",  
                "jens", "jens", "stoltenberg", "peter", "altmeier", "robert", "habeck", "heiko", "maa",
                "jean-yv", "klaus", "geiger", "silke", "bigalke", "paul-anton", "krüger", "donald", "trump", 
                "nord_stream", "schiltz", "europäisch", "union", "wolodymyr", "selenskyj", 
                "wolodimir", "selenski", "charl", "michel", "rio", "janeiro",
                "friedrich", "merz", "daniel", "wetzler", "liz", "truss", "eric", "bons", "manuela", 
                "schwesig", "michael", "roth", "weißen", "haus", "dimitrij", "dimitri", "peskow", "inna", 
                "hartwich", "ursula", "leyen", "eduard", "steiner", "antoni", "blinken", 
                "wendi", "sherman", "annalena", "baerbock", "erwin", "seller", "annalena_baerbock_grüne",
                "christian", "lindner", "christian_lindner", "thorsten", "jungholt", "boris", "johnson", 
                "dmytro", "kuleba", "barack", "obama", "clemens", "wergin", "le", "drian", "sigmar_gabriel", 
                "sigmar", "gabriel", "joe", "biden", "schröder", "gerhard", "nord", "stream", 
                "stefan", "kornelius", "emmanuel", "macron", "altmaier", "peter")




### additional stopwords (german) 

stopde <- readLines("D:\\R Scripte\\Scripte\\SZ und FAZ Artikel China Textmining!\\Zeitungsmining\\lexika\\german_stopwords_full.txt")


### Tokenisation including: removing stopwords, numbers, punctuation, symbols, urls and language stemming

### Creating Uni and Bigrams

ArtikelTokens.clean <- tokens(Artikel, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

### Tokens as bigrams

ArtikelTokens.clean.bi <- tokens_ngrams(ArtikelTokens.clean, n = 2)


### Important to separate the Tokens per research period
### Tokens per 2018

ArtikelTokens2018.clean <- tokens(Artikel18, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))
#

ArtikelTokens2018.clean.bi <- tokens_ngrams(ArtikelTokens2018.clean, n = 2)


#### Tokens per 2021

ArtikelTokens2021.clean <- tokens(Artikel21, remove_numbers = T, remove_symbols = T, remove_url = T, remove_punct = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))


#

ArtikelTokens2021.clean.bi <- tokens_ngrams(ArtikelTokens2021.clean, n = 2)

### Tokens per 2022

ArtikelTokens2022.clean <- tokens(Artikel22, remove_numbers = T, remove_symbols = T, remove_url = T, remove_punct = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

#

ArtikelTokens2022.clean.bi <- tokens_ngrams(ArtikelTokens2022.clean, n = 2)



# 

save(ArtikelTokens.clean, file = "AllTokensClean.Rda")

save(ArtikelTokens.clean.bi, file = "AllTokensCleanbi.Rda")

save(ArtikelTokens2018.clean, file = "Tokens2018.Rda")

save(ArtikelTokens2018.clean.bi, file = "Tokens2018bi.Rda")

save(ArtikelTokens2021.clean, file = "Tokens2021.Rda")

save(ArtikelTokens2021.clean.bi, file = "Tokens2021bi.Rda")

save(ArtikelTokens2022.clean, file = "Tokens2022.Rda")

save(ArtikelTokens2022.clean.bi, file = "Tokens2022bi.Rda")




### Construction of a DFM / Document Feature Matrix (based on the preprocessed tokens)
# dfm are created according to each research period. 


# clean dfms


dfm.clean <- dfm(ArtikelTokens.clean)

dfm.clean18 <- dfm(ArtikelTokens2018.clean)

dfm.clean21 <- dfm(ArtikelTokens2021.clean)

dfm.clean22 <- dfm(ArtikelTokens2022.clean)


### Construction of a DFM as BIGRAMS


dfm.clean.bi <- dfm(ArtikelTokens.clean.bi)

dfm.clean.bi18 <- dfm(ArtikelTokens2018.clean.bi)

dfm.clean.bi21 <- dfm(ArtikelTokens2021.clean.bi)

dfm.clean.bi22 <- dfm(ArtikelTokens2022.clean.bi)


### Trimmed dfm 
# keep only words occurring frequently (top 30%) and in <=50 documents


dfm.trim <- dfm_trim(dfm.clean, min_termfreq = 0.3, max_docfreq = 50,
                     termfreq_type = )


dfm.trim.group <- dfm.trim %>% 
  dfm_group(groups = Newspaper)


### Group DFM per Newspaper and Year


dfm.clean.grouped <-  dfm.clean %>% 
  dfm_group(groups = Newspaper)

dfm.clean.grouped18 <-  dfm.clean18 %>% 
  dfm_group(groups = Newspaper)

dfm.clean.grouped21 <-  dfm.clean21 %>% 
  dfm_group(groups = Newspaper)

dfm.clean.grouped22 <-  dfm.clean22 %>% 
  dfm_group(groups = Newspaper)

#

dfm.clean.group.year <- dfm.clean %>% 
  dfm_group(groups = Year)


### First Visualisation of the content-----

freqs_all <- textstat_frequency(dfm.clean)

freqs_all_bi <- textstat_frequency(dfm.clean.bi)

# per research period

freqs18 <- textstat_frequency(dfm.clean18)

freqs21 <- textstat_frequency(dfm.clean21)

freqs22 <- textstat_frequency(dfm.clean22)

freqs18bi <- textstat_frequency(dfm.clean.bi18)

freqs21bi <- textstat_frequency(dfm.clean.bi21)

freqs22bi <- textstat_frequency(dfm.clean.bi22)



### Wordcloud  1
# package wordcloud2


wordcloud2(freqs_all_bi,
           color = "random-dark",
           backgroundColor = "#bbb8b7",
           size = 0.75, minSize = 4,
           shuffle = FALSE, rotateRatio = 0.1,
           shape = "elipse",
           ellipticity = 0.5) 


### wordcloud 2
# package quanteda


### Comparison Cloud per Newspaper

colortest1 <- c("darkorchid3", "firebrick3",
                "mediumseagreen", "sienna2")

#

textplot_wordcloud(dfm.clean.group.year, color = colortest1,
                   min_size = 1, max_size = 3, 
                   min_count = 100, max_words = 30, 
                   rotation = 0.05, random_order = FALSE,
                   comparison = TRUE, labelsize = 1, 
                   labeloffset = 0.001)



### Topic Modelling (stm package)----
# Research Period 2018 
# without metadata

dfm2stm <- convert(dfm.clean18, to = "stm")

names(dfm2stm)


#

TM2018 <- stm(dfm2stm$documents, dfm2stm$vocab, 
            K = 3, max.em.its = 1000, 
            data = dfm2stm$meta, init.type = "Spectral")


# Visualisation
# allows to evalutate how close the topics are "related" to each other
# enables the researcher to further iterate and adjust the number of topics 

toLDAvis(mod = TM2018, docs = dfm2stm$documents)

# Time for the Iteration to set the "correct" number of topics
# Wordlists per Topics

Labels2018 <- labelTopics(TM2018, topics = 2, frexweight = 1)

Labels2018

# Plotting prevalence per topic 

plot(TM2018, main = "Expected Topic Proportion 2018", type = "summary")

# Show top documents associated with respective topic

RepresentDocs2018 <- findThoughts(TM2018, texts = Artikel18, n = 3, topics = c(2))
RepresentDocs2018

# Show quote representative for respective topic

firstdocs.2 <- findThoughts(TM2018,texts = Artikel18shorttext, n = 30, topics = c(2))$docs[[1]]
firstdocs.2

plotQuote(firstdocs.2, main="Top Documents, Topic 2 - Nord Stream 2?")



#
### Research Period 2021 
#


dfm2stm21 <- convert(dfm.clean21, to = "stm")

names(dfm2stm21)


#

TM2021 <- stm(dfm2stm21$documents, dfm2stm21$vocab, 
              K = 3, max.em.its = 1000, 
              data = dfm2stm21$meta, init.type = "Spectral")


# Visualisation
# allows to evalutate how close the topics are "related" to each other
# enables the researcher to further iterate and adjust the number of topics 

toLDAvis(mod = TM2021, docs = dfm2stm21$documents)

# Time for the Iteration to set the "correct" number of topics
# Wordlists per Topics

Labels2021 <- labelTopics(TM2021, topics = 2, frexweight = 1)

Labels2021

# Plotting prevalence per topic 

plot(TM2021, main = "Expected Topic Proportion 2021", type = "summary")


# Show top documents associated with respective topic

RepresentDocs2021 <- findThoughts(TM2021, texts = Artikel21, n = 3, topics = c(2))
RepresentDocs2021

# Show quote representative for respective topic

firstdocs.21 <- findThoughts(TM2021,texts = Artikel21shorttext, n = 15, topics = c(2))$docs[[1]]
firstdocs.21

plotQuote(firstdocs.21, main="Top Documents, Topic 2 - Nord Stream 2?")


#
### Research Period 2022
#


dfm2stm22 <- convert(dfm.clean22, to = "stm")

names(dfm2stm22)


#

TM2022 <- stm(dfm2stm22$documents, dfm2stm22$vocab, 
              K = 3, max.em.its = 1000, 
              data = dfm2stm22$meta, init.type = "Spectral")


# Visualisation
# allows to evalutate how close the topics are "related" to each other
# enables the researcher to further iterate and adjust the number of topics 

toLDAvis(mod = TM2022, docs = dfm2stm22$documents)

# Time for the Iteration to set the "correct" number of topics
# Wordlists per Topics

Labels2022 <- labelTopics(TM2022, topics = 2, frexweight = 1)

Labels2022

# Plotting prevalence per topic 

plot(TM2022, main = "Expected Topic Proportion 2022", type = "summary")


# Show top documents associated with respective topic

RepresentDocs2022 <- findThoughts(TM2022, texts = Artikel22, n = 3, topics = c(2))
RepresentDocs2022

# Show quote representative for respective topic

firstdocs.22 <- findThoughts(TM2022,texts = Artikel22shorttext, n = 30, topics = c(2))$docs[[1]]
firstdocs.22

plotQuote(firstdocs.2, main="Top Documents, Topic 1 - Nord Stream 2?")



#
#### Case Study Military Support for Ukraine----
#


# corpus creation

DATA_DIR <- system.file("extdata/", package = "readtext")


(Zeitungen <- readtext(paste0(DATA_DIR, "/pdf/Artikel Ukraine/*.pdf"), 
                       docvarsfrom = "filenames", 
                       docvarnames = c("Newspaper", "Date", "Title"),
                       sep = "_"))


Zeitungen$doc_id <- str_sub(Zeitungen$doc_id, start = 0, end = -5)


# preprocessing steps


Zeitungen$"Date" <- dmy(Zeitungen$"Date")


# Create new column, where month contains Date from each Document. 


Zeitungen <- Zeitungen %>%
  mutate(Month=month(Date))

# Delete URLS from all documents


Zeitungen$text <- gsub("http.*", "", Zeitungen$text)

Zeitungen$text <- gsub("https.*", "", Zeitungen$text)

#

save(Zeitungen, file = "UkrainePreCorpus.Rda")

#

Artikel <- corpus(Zeitungen, docid_field = "Title",
                  text_field = "text",
                  unique_docnames = FALSE)


docvars(Artikel, "Number") <- sprintf("%02d", 1:ndoc(Artikel))

#

ArtikelshorttextUKR <- substr(Artikel, 1, 200)

#

Artikel01 <- corpus_subset(Artikel, Month == 1)

Artikel02 <- corpus_subset(Artikel, Month == 2)

Artikel03 <- corpus_subset(Artikel, Month == 3)


#

save(Artikel, file = "UkraineCorpus.Rda")


### data preprocessing----
# use the stoppwords defined previously


# Stopwords

ToBeRemoved <- c("reuters", "dpa", "afp", "china", "chin*", 
                 "siehe", "th", "dass", "sei", "mehr", "ab",
                 "sagte", "worden", "jahr", "seit", "schon", "p.k", 
                 "bloomberg", "montag", "dienstag", "mittwoch", "donnerstag", 
                 "freitag", "samstag", "sonntag", "januar", "februar", 
                 "märz", "april", "mai", "juni", "juli", "august", "september", 
                 "november", "dezember", "w", "boe", "dm", "wegen", "amerika*", 
                 "peking", "dafür", "ende", "berlin", "steht", "macht", 
                 "jedoch", "müssen", "sagt", "land", "wurde", "usa",
                 "ap", "peking*", "taz", "sz", "faz", "etwa", "seien",
                 "deutsch*", "washington", "berlin", "heute", "gestern",
                 "seite", "neu*", "immer", "prozent", "of", "deshalb", 
                 "künftig", "woche", "könn*", "künft*", "offenbar", "hätt*", 
                 "zeit", "vergang*", "große*", "angaben", "jahre*", "teil", 
                 "asien", "insel", "welt", "mark", "kp", "million*", "muß", "c", 
                 "süddeutschen zeitung", "f.a.z_frankfurt", "f.a.z", "f", "a", "z",
                 "frankfurt", "darüber", "hinaus", "de", "hob", "hervor", 
                 "treffen", "solle", "ging", "gebe", "alten", "hieß", "wäre", 
                 "weiß", "wochen", "fest", "alte", "höhe", "trotz", "new", "york",
                 "fünf", "groß", "rede", "zahl", "hält", "lesedauer", "veröffentlicht",
                 "minuten", "quelle", "anzeige", "lesen", "getty", "image", 
                 "images", "beziehungen", "dollar",
                 "euro", "schanghai", "kilometer", "lange", "größten", "gegenüber", 
                 "stehen", "frage", "zurück", "deutlich", "heißt", 
                 "vereinigt*", "staaten", "amerika", "minister", "us*", 
                 "schließlich", "tatsächlich", "tag", "kurz", "lä*", "la*", "zuvor", 
                 "hamburg", "außenminist*", "zeitung", "münchen", "regie*", "russ*", 
                 "bundesregierung", "präsident", "spd", "cdu", "fdp", "wenige", 
                 "pipeline", "gaspipeline", "anmerkungen", "redaktion", "ukrain*", 
                 "europ*", "moskau", "berlin", "brüssel", "frankreich", "präsid*", "staat", 
                 "sankt", "petersburg", "st", "st_petersburg", "sankt_petersburg", "kiew", 
                 "poroschenko", "dodik", "madrid", "belarus", "eu", "türk*", "bozkir", "
                 bürgermeister", "toi", "kollegen")

##

Namen <- c("felix", "lee", "johnny", "erling",
           "nikolaus", "doll", "nikolaus", "doll", "friedericke", "böge",
           "christo", "hardebusch", "nora", "sausmikat", "nora", "sausmikat", 
           "sven", "hansen", "sven", "hansen", "jones", "bos", "jones", "bos",
           "kai", "strittmatter", "kai", "strittmatter", "petra", "kolonko","petra",
           "georg", "blume", "eric bonse", "britta", "petersen", "britta", 
           "petersen", "peter", "weissenburger", "frank", "stocker", "sophie", "mühlmann", 
           "pavel", "lokshin", "johnny_erling", "johnny", "erling", "julia", "boxler",
           "josep", "borrel", "horst", "seehof", "stefani", "schiffer", "christoph", "nil", "schmid", 
           "eric bons", "olaf", "scholz", "kanzler", "scholz", "bundeskanzler", "scholz", "angela", "merkel", "scholz",
           "merkel", "bundeskanzlerin", "merkel", "kanzlerin", "merkel", "wladimir", "putin",  
           "jens", "jens", "stoltenberg", "peter", "altmeier", "robert", "habeck", "heiko", "maa",
           "jean-yv", "klaus", "geiger", "silke", "bigalke", "paul-anton", "krüger", "donald", "trump", 
           "nord_stream", "schiltz", "europäisch", "union", "wolodymyr", "selenskyj", 
           "wolodimir", "selenski", "charl", "michel", "rio", "janeiro",
           "friedrich", "merz", "daniel", "wetzler", "liz", "truss", "eric", "bons", "manuela", 
           "schwesig", "michael", "roth", "weißen", "haus", "dimitrij", "dimitri", "peskow", "inna", 
           "hartwich", "ursula", "leyen", "eduard", "steiner", "antoni", "blinken", 
           "wendi", "sherman", "annalena", "baerbock", "erwin", "seller", "annalena_baerbock_grüne",
           "christian", "lindner", "christian_lindner", "thorsten", "jungholt", "bori", "johnson", 
           "dmytro", "kuleba", "barack", "obama", "clemens", "wergin", "le", "drian", "sigmar_gabriel", 
           "sigmar", "gabriel", "joe", "biden", "schröder", "gerhard", "nord", "stream", 
           "stefan", "kornelius", "emmanuel", "macron", "altmaier", "peter", "boris", "johnson", 
           "recep", "tayyep", "erdogan", "radek", "sikorski", "jen", "psaki", "jake", "sullivan", 
           "vitali", "klitschko", "marin", "pen", "viktor", "orbán", "lettland", "litauen", "polen", 
           "professor", "carlo", "masala", "anna", "arion", "magnitzki", "alina", "bill", "browder", "kristian", 
           "figelj", "rob", "bauer", "sergej", "mearsheimer")




### additional stopwords (german) 

stopde <- readLines("D:\\R Scripte\\Scripte\\SZ und FAZ Artikel China Textmining!\\Zeitungsmining\\lexika\\german_stopwords_full.txt")


### Tokenisation including: removing stopwords, numbers, punctuation, symbols, urls and language stemming

### Creating Uni and Bigrams

TokensUKR <- tokens(Artikel, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

#

TokensUKR.bi <- tokens_ngrams(TokensUKR, n = 2)

#

save(TokensUKR, file = "TokensUKR.Rda")

save(TokensUKR.bi, file = "TokensUKRbi.Rda")

### Tokens per Month

TokensUKR01 <- tokens(Artikel01, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

#

TokensUKR01.bi <- tokens_ngrams(TokensUKR01, n = 2)

#


TokensUKR02 <- tokens(Artikel02, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

#

TokensUKR02.bi <- tokens_ngrams(TokensUKR02, n = 2)

#

TokensUKR03 <- tokens(Artikel03, remove_numbers = T, remove_punct = T, remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("de")) %>%
  tokens_remove(ToBeRemoved) %>% 
  tokens_remove(stopde) %>%
  tokens_remove(Namen) %>% 
  tokens_wordstem(language = quanteda_options("language_stemmer"))

#

TokensUKR03.bi <- tokens_ngrams(TokensUKR03, n = 2)



### Construction of a DFM / Document Feature Matrix (based on the preprocessed tokens)
# dfm are created according to each research period. 


# clean dfms


dfm.clean <- dfm(TokensUKR)

dfm.clean01 <- dfm(TokensUKR01)

dfm.clean02 <- dfm(TokensUKR02)

dfm.clean03 <- dfm(TokensUKR03)


### Construction of a DFM as BIGRAMS


dfm.clean.bi <- dfm(TokensUKR.bi)


### Group DFM per Month


dfm.clean.grouped <-  dfm.clean %>% 
  dfm_group(groups = Month)




### Visualisation----

freqs_all <- textstat_frequency(dfm.clean)

freqs_all_bi <- textstat_frequency(dfm.clean.bi)

# per Newspaper

freqs1 <- textstat_frequency(dfm.clean.grouped)


### Wordcloud  1
# package wordcloud2


wordcloud2(freqs_all_bi,
           color = "random-dark",
           backgroundColor = "#bbb8b7",
           size = 0.75, minSize = 4,
           shuffle = FALSE, rotateRatio = 0.1,
           shape = "elipse",
           ellipticity = 0.5) 


### wordcloud 2
# package quanteda


### Comparison Cloud per Newspaper

colortest1 <- c("darkorchid3", "firebrick3",
                "mediumseagreen", "sienna2")

#

textplot_wordcloud(dfm.clean.grouped, color = colortest1,
                   min_size = 2, max_size = 4, 
                   min_count = 100, max_words = 30, 
                   rotation = 0.05, random_order = FALSE,
                   comparison = TRUE, labelsize = 1, 
                   labeloffset = 0.001) 




### Topic Modelling----
 



topicQuality(TMUKR, documents = dfm2stm$documents, M = 10)

# Time for the Iteration to set the "correct" number of topics
# Wordlists per Topics

LabelsUKR <- labelTopics(TMUKR, topics = c(5,8,10), frexweight = 1)

LabelsUKR

# Plotting prevalence per topic 

plot(TMUKR, main = "Expected Topic Proportion", type = "summary")

# Show top documents associated with respective topic

RepresentDocsUKR <- findThoughts(TMUKR, texts = Artikel, n = 3, topics = c(2))
RepresentDocsUKR

# Show quote representative for respective topic

firstdocs.UKR <- findThoughts(TMUKR,texts = ArtikelshorttextUKR, n = 25, topics = c(5))$docs[[1]]
View(firstdocs.UKR)

# plotQuote(firstdocs.UKR, main="")




