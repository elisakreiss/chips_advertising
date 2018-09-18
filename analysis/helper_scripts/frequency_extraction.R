library(tidyverse)
library(ngramr)
library(tm)
library(stopwords)
library(here)

df = read_csv(here("texts","chips_data_frequency.csv"))

df$Text = gsub(",|?|!|\\.","",df$Text)
# stopwords to add: dass
df$Text = removeWords(df$Text,stopwords::stopwords("de", source = "snowball"))

word_freq = data.frame(Product_ID=character(),Word=character(),MeanFreq=double())

add_word <- function(word_freq, freq, Product){
  aggre_freq = freq %>%
    select(Phrase,Frequency) %>%
    group_by(Phrase) %>%
    summarize(MeanFreq=mean(Frequency)) %>%
    add_column(Product_ID=as.character(Product)) %>%
    rename(Word=Phrase)
  word_freq = bind_rows(word_freq,aggre_freq)
  return(word_freq)
}

myFunction <- function(Text,Product){
  split = str_split(Text,boundary("word"))
  for (words_vector in split) {
    for (word in words_vector) {
      # print("word:")
      print(word)
      freq = ngram(word, corpus = "ger_2012", year_start = 1960, year_end = 2008, count = FALSE, tag = NULL, case_ins = TRUE)
      if (length(freq) > 1) {
        word_freq = add_word(word_freq, freq, Product)
      }
    }
    write.csv(word_freq,file = here("analysis","frequencies","no_stopwords",paste(Product,".csv",sep = "")),row.names = FALSE)
  }
}

# df$MeanFreq = myFunction(df$Text)
myFunction(df[df$Product_name=='Naturals',]$Text,"Naturals")
myFunction(df[df$Product_name=='Dorfchips',]$Text,"Dorfchips")
myFunction(df[df$Product_name=='Küstengold',]$Text,"Küstengold")
myFunction(df[df$Product_name=='HofChips',]$Text,"HofChips")
myFunction(df[df$Product_name=='Lays',]$Text,"Lays")
myFunction(df[df$Product_name=='KrosseKerle',]$Text,"KrosseKerle")
myFunction(df[df$Product_name=='Chipsfrisch',]$Text,"Chipsfrisch")
myFunction(df[df$Product_name=='Crunchips',]$Text,"Crunchips")
# myFunction(df[df$Product_name=='KettleChips',]$Text)
myFunction(df[df$Product_name=='JedenTagChips',]$Text,"JedenTagChips")
myFunction(df[df$Product_name=='ClarkysKesselChips',]$Text,"ClarkysKesselChips")
myFunction(df[df$Product_name=='CrustiCroc',]$Text,"CrustiCroc")
myFunction(df[df$Product_name=='FeurichGourmet',]$Text,"FeurichGourmet")
myFunction(df[df$Product_name=='Trafo',]$Text,"Trafo")
# myFunction(df[df$Product_name=='HandCookedChips',]$Text)
myFunction(df[df$Product_name=='DeRit',]$Text,"DeRit")
myFunction(df[df$Product_name=='FeurichChips',]$Text,"FeurichChips")
# myFunction(df[df$Product_name=='Tyrrells',]$Text)
myFunction(df[df$Product_name=='JA',]$Text,"JA")
myFunction(df[df$Product_name=='Chio',]$Text,"Chio")
myFunction(df[df$Product_name=='WorldOfChips',]$Text,"WorldOfChips")
myFunction(df[df$Product_name=='Classic',]$Text,"Classic")
myFunction(df[df$Product_name=='Lisas',]$Text,"Lisas")
myFunction(df[df$Product_name=='GutUndGünstigChips',]$Text,"GutUndGünstigChips")
myFunction(df[df$Product_name=='GutUndGünstigKessel',]$Text,"GutUndGünstigKessel")


###
###
###

Chio = read_csv(here("analysis","frequencies","no_stopwords","Chio.csv"))
Chipsfrisch = read_csv(here("analysis","frequencies","no_stopwords","Chipsfrisch.csv"))
ClarkysKesselChips = read_csv(here("analysis","frequencies","no_stopwords","ClarkysKesselChips.csv"))
Classic = read_csv(here("analysis","frequencies","no_stopwords","Classic.csv"))
Crunchips = read_csv(here("analysis","frequencies","no_stopwords","Crunchips.csv"))
CrustiCroc = read_csv(here("analysis","frequencies","no_stopwords","CrustiCroc.csv"))
DeRit = read_csv(here("analysis","frequencies","no_stopwords","DeRit.csv"))
Dorfchips = read_csv(here("analysis","frequencies","no_stopwords","Dorfchips.csv"))
FeurichChips = read_csv(here("analysis","frequencies","no_stopwords","FeurichChips.csv"))
FeurichGourmet = read_csv(here("analysis","frequencies","no_stopwords","FeurichGourmet.csv"))
GutUndGünstigChips = read_csv(here("analysis","frequencies","no_stopwords","GutUndGünstigChips.csv"))
GutUndGünstigKessel = read_csv(here("analysis","frequencies","no_stopwords","GutUndGünstigKessel.csv"))
HofChips = read_csv(here("analysis","frequencies","no_stopwords","HofChips.csv"))
JA = read_csv(here("analysis","frequencies","no_stopwords","JA.csv"))
JedenTagChips = read_csv(here("analysis","frequencies","no_stopwords","JedenTagChips.csv"))
KrosseKerle = read_csv(here("analysis","frequencies","no_stopwords","KrosseKerle.csv"))
Küstengold = read_csv(here("analysis","frequencies","no_stopwords","Küstengold.csv"))
Lays = read_csv(here("analysis","frequencies","no_stopwords","Lays.csv"))
Lisas = read_csv(here("analysis","frequencies","no_stopwords","Lisas.csv"))
Naturals = read_csv(here("analysis","frequencies","no_stopwords","Naturals.csv"))
Trafo = read_csv(here("analysis","frequencies","no_stopwords","Trafo.csv"))
WorldOfChips = read_csv(here("analysis","frequencies","no_stopwords","WorldOfChips.csv"))

df_no_stopwords = bind_rows(Chio,Chipsfrisch,ClarkysKesselChips,Classic,Crunchips,CrustiCroc,DeRit,Dorfchips,FeurichChips,FeurichGourmet,GutUndGünstigChips,GutUndGünstigKessel,HofChips,JA,JedenTagChips,KrosseKerle,Küstengold,Lays,Lisas,Naturals,Trafo,WorldOfChips)
no_stopwords_freq = df_no_stopwords %>%
  select(-Word) %>%
  group_by(Product_ID) %>%
  summarise(AvgFreq = mean(MeanFreq))

write_csv(no_stopwords_freq, here("analysis","frequencies","no_stopwords_freq.csv"))

df_no_stopwords = df_no_stopwords[df_no_stopwords$Word != "dass",]

min(df_no_stopwords$MeanFreq)
ordered = arrange(df_no_stopwords,MeanFreq)
print(ordered,n=100)
# 0.0000000537

ggplot(df_no_stopwords,aes(x=Word,y=MeanFreq)) +
  geom_point() +
  facet_wrap(~ Product_ID) +
  geom_text(aes(label=Word),hjust=1, vjust=-0.5) +
  ylim(0,0.0000000537)


string <- "Thisü is a very long character vector. Why is it so long! I think is short for long. I want to split this vector into senteces by using strssplit. Can someone help me! That would be nice."
string = str_replace_all(string,"!",".")

splitted = unlist(strsplit(string, "(?<=\\.)", perl = T))
words_per_sentence = str_count(splitted, boundary("word"))
avg_sentence_length_chars = sum(words_per_sentence)/length(words_per_sentence)



chars_per_sentence = str_count(splitted, ".")
avg_sentence_length_chars = sum(chars_per_sentence)/length(chars_per_sentence)






unlist(strsplit(string, "(?<=\\.)", perl = T))


unlist(str_count(string, boundary("sentence")))

       