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

extract_freqs <- function(Text,Product){
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

# df$MeanFreq = extract_freqs(df$Text)
extract_freqs(df[df$Product_name=='Naturals',]$Text,"Naturals")
extract_freqs(df[df$Product_name=='Dorfchips',]$Text,"Dorfchips")
extract_freqs(df[df$Product_name=='Küstengold',]$Text,"Küstengold")
extract_freqs(df[df$Product_name=='HofChips',]$Text,"HofChips")
extract_freqs(df[df$Product_name=='Lays',]$Text,"Lays")
extract_freqs(df[df$Product_name=='KrosseKerle',]$Text,"KrosseKerle")
extract_freqs(df[df$Product_name=='Chipsfrisch',]$Text,"Chipsfrisch")
extract_freqs(df[df$Product_name=='Crunchips',]$Text,"Crunchips")
# extract_freqs(df[df$Product_name=='KettleChips',]$Text)
extract_freqs(df[df$Product_name=='JedenTagChips',]$Text,"JedenTagChips")
extract_freqs(df[df$Product_name=='ClarkysKesselChips',]$Text,"ClarkysKesselChips")
extract_freqs(df[df$Product_name=='CrustiCroc',]$Text,"CrustiCroc")
extract_freqs(df[df$Product_name=='FeurichGourmet',]$Text,"FeurichGourmet")
extract_freqs(df[df$Product_name=='Trafo',]$Text,"Trafo")
# extract_freqs(df[df$Product_name=='HandCookedChips',]$Text)
extract_freqs(df[df$Product_name=='DeRit',]$Text,"DeRit")
extract_freqs(df[df$Product_name=='FeurichChips',]$Text,"FeurichChips")
# extract_freqs(df[df$Product_name=='Tyrrells',]$Text)
extract_freqs(df[df$Product_name=='JA',]$Text,"JA")
extract_freqs(df[df$Product_name=='Chio',]$Text,"Chio")
extract_freqs(df[df$Product_name=='WorldOfChips',]$Text,"WorldOfChips")
extract_freqs(df[df$Product_name=='Classic',]$Text,"Classic")
extract_freqs(df[df$Product_name=='Lisas',]$Text,"Lisas")
extract_freqs(df[df$Product_name=='GutUndGünstigChips',]$Text,"GutUndGünstigChips")
extract_freqs(df[df$Product_name=='GutUndGünstigKessel',]$Text,"GutUndGünstigKessel")


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

df_no_stopwords$Length = str_length(df_no_stopwords$Word)

ggplot(df_no_stopwords,aes(x=Length,y=MeanFreq)) +
  geom_point() +
  # facet_wrap(~ Product_ID) +
  geom_text(aes(label=Word),hjust=1, vjust=-0.5) +
  ylim(0,0.0000000537)
       