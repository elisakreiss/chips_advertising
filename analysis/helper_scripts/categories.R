library(tidyverse)

BoundPhrases = c('backen','gebacken','ausgebacken','bio','David','deutschland','aus deutschland','aus den niederlanden oder deutschland','aus besten deutschen anbauregionen','von deutschen herstellern','dorfchips','ehrlich','die einfachen dinge','mehr braucht es nicht','ernten','familienbetrieb','feinste','hohen anteil an ungesättigten fettsäuren','reich an einfach ungesättigten fettsäuren','reich an ungesättigten fettsäuren','fresenius','frittiert','funny-frische knusprigkeit','garantie','ohne geschmacksverstärker','ohne zusatz von geschmacksverstärkenden zusatzstoffen','ohne gluten','glutenfrei','goldener preis','gourmet','von hand','handgefertigte','handgekochte','ohne hefeextrakt','heimat','heiner','heiner & martin','hergestellt','herstellung','für die menschen von hier','hofchips','auf dem familieneigenen kartoffelhof','johannings kartoffelhof','kochen','gekocht','ohne konservierungsstoffe','ohne künstliche aromen','ohne künstliche farbstoffe','ländlich-rustikal','von natur aus','nr 1','ohne','pfeffer','premium','aus eigener produktion','bei der produktion','qualität','qualitäts-garantie','qualitätskontrolle','markenqualität','qualitätsversprechen','aus den regionen lüneburger heide, oberpfalz und niederbayern','reinheitsgebot','rosmarin','geröstet','gerührt','gesalzen','sauerrahm','schneiden','geschnitten','schnittlauch','sonnenblumenöl','nach alter tradition','vegetariergeeignet','vegetarier geeignet','verarbeitung','vereinigten königreich','verfeinert','verlässt das werk','aus unserem eigenen vertragsanbau','von ausgesuchten vertragsbauern','wählen','gewaschen','wir von lorenz snack-world','wochenmarkt','würzen','würzung','würzmischung','gewürze','gewürzt','zubereitung','zufriedenheitsgarantie','100%','seit 2010','1969','in 3. generation','in kessel nr 4','no artificial ingredients','never use artificial flavors and colors','we only cook our chips in sunflower oil','certified','cracked','english','england','less fat','average fat reduced','finest','food ingredients','free','fried','gluten free','gluten-free','hand cooked','hand-cooked','less','local','longer fried than regular chips','made with','more','most','natural','never','no','only','bio-organic','organic','organics','pepper','potatoes','real','salt','salted','we season our chips','signature crunch','sliced','spices','spun','no added sugar','sunflower oil','thick cut','thicker','time-honoured','traditional','unique','suitable for vegans','vinegar')
bound = data.frame(BoundPhrases,Bound="both") %>%
  rename(Phrase = BoundPhrases)
bound[] <- lapply(bound, as.character)

LeftBoundPhrases = c('ausgesucht','ausgewählt','außergewöhnlich','besonder','beste','biologisch','einzigartig','erlesen','erstklassig','extra','familieneigen','garantier','gemahlen','größte','höchste','ingredient','kontrollier','landwirt','natürliche','perfekt','pur','regional','rein','speziell','un','ungehärtet','unverfälscht','unvergleichlich','unverwechselbar','verwende','zutat')
leftbound = data.frame(LeftBoundPhrases,Bound="left") %>%
  rename(Phrase = LeftBoundPhrases)
leftbound[] <- lapply(leftbound, as.character)

RightBoundPhrases = c('frei')
rightbound = data.frame(RightBoundPhrases,Bound="right") %>%
  rename(Phrase = RightBoundPhrases)
rightbound[] <- lapply(rightbound, as.character)

NoBoundPhrases = c('essig','frisch','kartoffel','paprika','salz')
nobound = data.frame(NoBoundPhrases,Bound="none") %>%
  rename(Phrase = NoBoundPhrases)
nobound[] <- lapply(nobound, as.character)

Phrases = bind_rows(bound,leftbound,rightbound,nobound)

###

HeP = c('hohen anteil an ungesättigten fettsäuren','reich an einfach ungesättigten fettsäuren','reich an ungesättigten fettsäuren','frisch','ohne geschmacksverstärker','ohne zusatz von geschmacksverstärkenden zusatzstoffen','ohne gluten','glutenfrei','ohne hefeextrakt','ohne konservierungsstoffe','vegetariergeeignet','vegetarier geeignet','less fat','average fat reduced','gluten free','gluten-free','no added sugar','suitable for vegans')

CP = c('feinste','funny-frische knusprigkeit','nr 1','premium','less fat','finest','less','longer fried than regular chips','more','most','signature crunch','thicker','unique','außergewöhnlich','besonder','beste','einzigartig','erstklassig','extra','größte','höchste','perfekt','speziell','unvergleichlich','unverwechselbar')

NM = c('ohne','free','never','no','frei','un')

NP = c('bio','dorfchips','die einfachen dinge','mehr braucht es nicht','ohne geschmacksverstärker','ohne zusatz von geschmacksverstärkenden zusatzstoffen','ohne konservierungsstoffe','ohne künstliche aromen','ohne künstliche farbstoffe','von natur aus','100%','no artificial ingredients','never use artificial flavors and colors','natural','bio-organic','organic','organics','real','biologisch','natürliche','pur','rein','ungehärtet','unverfälscht','hand cooked','hand-cooked','ausgesucht','ausgewählt','erlesen')

HiP = c('nach alter tradition','seit 2010','1969','in 3. generation','time-honoured','traditional')

LoP = c('deutschland','aus deutschland','aus den niederlanden oder deutschland','aus besten deutschen anbauregionen','von deutschen herstellern','heimat','für die menschen von hier','ländlich-rustikal','regional','aus den regionen lüneburger heide, oberpfalz und niederbayern','vereinigten königreich','english','england','local')

GP = c('David','dorfchips','familienbetrieb','familieneigen','von hand','handgefertigte','handgekochte','heimat','heiner','heiner & martin','hofchips','auf dem familieneigenen kartoffelhof','johannings kartoffelhof','aus eigener produktion','aus unserem eigenen vertragsanbau','von ausgesuchten vertragsbauern','wählen','wir von lorenz snack-world','wochenmarkt','in kessel nr 4','landwirt')

IP = c('essig','ohne gluten','glutenfrei','ohne hefeextrakt','kartoffel','paprika','pfeffer','rosmarin','salz','sauerrahm','schnittlauch','sonnenblumenöl','vegetariergeeignet','vegetarier geeignet','würzen','würzung','würzmischung','gewürze','gewürzt','zutat','gluten free','gluten-free','ingredient','pepper','potatoes','salt','salted','we season our chips','spices','no added sugar','sunflower oil','suitable for vegans','vinegar')

PP = c('backen','gebacken','ausgebacken','ernten','frittiert','von hand','handgefertigte','handgekochte','kochen','gekocht','bei der produktion','geröstet','gerührt','gesalzen','schneiden','geschnitten','verarbeitung','verfeinert','gewaschen','würzen','würzung','gewürzt','zubereitung','in kessel nr 4','we only cook our chips in sunflower oil','cracked','fried','hand cooked','hand-cooked','salted','we season our chips','sliced','spun','thick cut','gemahlen')

QP = c('ehrlich','fresenius','garantie','goldener preis','gourmet','nr 1','premium','qualität','qualitäts-garantie','qualitätskontrolle','markenqualität','qualitätsversprechen','zufriedenheitsgarantie','garantier','kontrollier','certified')

###

Phrases$HealthPhrase = ifelse(Phrases$Phrase %in% HeP,TRUE,FALSE)
Phrases$ComparativePhrase = ifelse(Phrases$Phrase %in% CP,TRUE,FALSE)
Phrases$NegativeMarker = ifelse(Phrases$Phrase %in% NM,TRUE,FALSE)
Phrases$NaturalnessPhrase = ifelse(Phrases$Phrase %in% NP,TRUE,FALSE)
Phrases$HistoricityPhrase = ifelse(Phrases$Phrase %in% HiP,TRUE,FALSE)
Phrases$LocationPhrase = ifelse(Phrases$Phrase %in% LoP,TRUE,FALSE)
Phrases$IngredientPhrase = ifelse(Phrases$Phrase %in% IP,TRUE,FALSE)
Phrases$ProcessPhrase = ifelse(Phrases$Phrase %in% PP,TRUE,FALSE)
Phrases$GenuityPhrase = ifelse(Phrases$Phrase %in% GP,TRUE,FALSE)
Phrases$QualityPhrase = ifelse(Phrases$Phrase %in% QP,TRUE,FALSE)

Phrases$PreparationPhrase = Phrases$IngredientPhrase | Phrases$ProcessPhrase
Phrases$AuthenticityPhrase = Phrases$NaturalnessPhrase | Phrases$HistoricityPhrase | Phrases$LocationPhrase | Phrases$GenuityPhrase | Phrases$QualityPhrase | Phrases$PreparationPhrase
Phrases$DistinctivePhrase = Phrases$ComparativePhrase | Phrases$NegativeMarker

###

# phrases that are not categorized at all
Phrases[!(Phrases$HealthPhrase | Phrases$DistinctivePhrase | Phrases$AuthenticityPhrase),]$Phrase

# phrases that are categorized into more than one group
Phrases[((Phrases$HealthPhrase + Phrases$DistinctivePhrase + Phrases$AuthenticityPhrase) > 1),c('Phrase','HealthPhrase','DistinctivePhrase','AuthenticityPhrase')]

### 

cat_pattern = function(Category){
 return(paste("(",
      ifelse(nrow(Phrases[Category & Phrases$Bound == "both",])==0,"",
         paste("\\b(",str_c(Phrases[Category & Phrases$Bound == "both",]$Phrase,collapse="|"),")\\b",sep="")),
      ifelse(nrow(Phrases[Category & Phrases$Bound == "left",])==0,"",
         paste("|","\\b(",str_c(Phrases[Category & Phrases$Bound == "left",]$Phrase,collapse="|"),")",sep="")),
      ifelse(nrow(Phrases[Category & Phrases$Bound == "right",])==0,"",
         paste("|","(",str_c(Phrases[Category & Phrases$Bound == "right",]$Phrase,collapse="|"),")\\b",sep="")),
      ifelse(nrow(Phrases[Category & Phrases$Bound == "none",])==0,"",
         paste("|","(",str_c(Phrases[Category & Phrases$Bound == "none",]$Phrase,collapse="|"),")",sep="")),")",
    sep=""))
}

cat_pattern(Phrases$ComparativePhrase)
