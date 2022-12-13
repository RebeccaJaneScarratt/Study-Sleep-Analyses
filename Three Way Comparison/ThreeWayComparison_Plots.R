# Attach libraries
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(data.table)
#library(flipMultivariates)
library(psych)
library(rstudioapi)
library(rstatix)
library(tictoc)
library(cowplot)
library(readr)
library(epitools)

#library(tidyverse)

#22sept editing to now do a three-way comparison


# Set seed for reproducability
set.seed(42)

# Set working directory using RStudio API
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Read data
data.unique <- read.csv('SSM_unique_withClusters.csv')

# Remember to drop those the X columns.

data.unique <- subset(data.unique, select = -c(X))


# Read in MSSD track data
# NOTE these files needs to be acquired by you. 
# They can be found at:
# https://www.aicrowd.com/challenges/spotify-sequential-skip-prediction-challenge


mssdFiles <- c('tf_000000000000.csv',
               'tf_000000000001.csv')
mssd <- do.call(rbind, lapply(mssdFiles, fread))

# Subset to keep only the features we're interested in
mssd <- subset(mssd, select = c(duration, release_year, us_popularity_estimate, acousticness, 
                                danceability, energy, instrumentalness, key, 
                                liveness, loudness, mode, speechiness, tempo, valence))

mssd$mode <- as.factor(mssd$mode)


# Merge data

mssd$category <- 'General'
mssd$weight <- 1
data.unique$category <- 'Study'
data.unique$duration <- round(data.unique$duration_ms*0.001)

# This adds a weight to the LDA since the Study Playlist Dataset is smaller than the MSSD
# first, calculate the ratio

ratio <- dim(mssd)[1]/dim(data.unique)[1]

data.unique$weight <- ratio


# now read in the sleep data here already

spd <- read.csv('SPD_unique_withClusters.csv')
spd$category <- 'Sleep'
spd$weight <- dim(mssd)[1]/dim(spd)[1]

mergedData <- rbind(subset(data.unique, select=c(weight, category, acousticness, danceability,
                                                 energy, instrumentalness, tempo,
                                                 liveness, loudness, valence,
                                                 speechiness, key)),
                    subset(mssd, select=c(weight, category, acousticness, danceability,
                                          energy, instrumentalness, tempo,
                                          liveness, loudness, valence,
                                          speechiness, key)),
                    subset(spd, select=c(weight, category, acousticness, danceability,
                                         energy, instrumentalness, tempo,
                                         liveness, loudness, valence,
                                         speechiness, key)))


mergedData$category <- as.factor(mergedData$category)
mergedData$category <- factor(mergedData$category, levels = c('General', 'Study', 'Sleep'))


# Descriptive stats ----

descriptive <- describeBy(mergedData[c(3:12)], mergedData$category, IQR=TRUE, fast=FALSE)
write.csv(descriptive[1], 'Three Way Comparison/generalDescriptive_1312.csv')
write.csv(descriptive[2], 'Three Way Comparison/studyDescriptive_1312.csv')
write.csv(descriptive[3], 'Three Way Comparison/sleepDescriptive_1312.csv')

# Statistics ----

# KS-test to get statistical distance between distributions.

# KS-stats between all three comparisons.

KS.spd_data = ks.test(data.unique$tempo, spd$tempo)
KS.spd_general = ks.test(spd$tempo, mssd$tempo)
KS.data_general = ks.test(data.unique$tempo, mssd$tempo)

toTest = c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness',
           'liveness', 'valence', 'tempo')

compResults = data.frame(matrix(ncol=4, nrow=0))
colnames(compResults) <- c('feature', 'data_general', 'general_spd', 'spd_data')

for (x in toTest) {
  
  this.spd_data = ks.test(data.unique[[x]], spd[[x]])
  this.spd_general = ks.test(spd[[x]], mssd[[x]])
  this.data_general = ks.test(data.unique[[x]], mssd[[x]])
  
  compResults[nrow(compResults) +1, ] = c(x, this.data_general$statistic, this.spd_general$statistic, this.spd_data$statistic)
}

compResults$feature <- as.factor(compResults$feature)
compResults$data_general <- as.numeric(compResults$data_general)
compResults$general_spd <- as.numeric(compResults$general_spd)
compResults$spd_data <- as.numeric(compResults$spd_data)

# now reshape

compResults.melt <- melt(compResults, id=c('feature'))


# Hypothesis is that sleep music is more similar to study music than to general music
# I.e., lower KS statistic for sleep/study than for sleep/general and/or study/general.

compResults.kruskal <- kruskal_test(compResults.melt, formula = value ~ variable)
test <- compResults.melt %>% 
  t_test(value ~ variable,
         var.equal = FALSE)

library(ggpubr)

compResults.melt$variable <- factor(compResults.melt$variable, levels = c('general_spd', 'data_general', 'spd_data'))

comparisons <- list(c('general_spd', 'data_general'),  c('data_general', 'spd_data'), c('general_spd', 'spd_data'))

ggboxplot(compResults.melt, x='variable', y='value', color='variable') +
  stat_compare_means(comparisons = comparisons) +
  stat_compare_means(label.y = 0.8)



# T-tests/kruskal ----
# Visual inspection of normality, since shapiro doesn't scale.


# danceability
plot(density(mergedData$danceability))
t.danceability.p <- mergedData %>%
  t_test(danceability ~ category, comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.danceability.eff <- mergedData %>%
  cohens_d(danceability ~ category, comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.danceability.p, y=t.danceability.eff)
statResults <- thisResult







#instrumentalness
plot(density(mergedData$instrumentalness))

# Due to distribution, round it, and then use chi-square.
# Use Cramer's V as effect size.
mergedData$instrRound <- round(mergedData$instrumentalness)

mergedData$instrRound <- as.factor(mergedData$instrRound)
instrTable <- table(subset(mergedData, select=c('category', 'instrRound')))
instrTableProp <- prop.table(instrTable, 1)
instrTest <- chisq_test(instrTable)
instrOdds <- oddsratio(instrTable)
instrEff <- cramer_v(instrTable)

# I guess I have to do manual post-hoc tests?
# General vs Study
instrTable.GvsS <- table(subset(mergedData, category == 'General' | category == 'Study', select=c('category', 'instrRound')))
instrTable.GvsS = instrTable.GvsS[1:2,]
instrTableProp.GvsS <- prop.table(instrTable.GvsS, 1)
instrTest.GvsS <- chisq_test(instrTable.GvsS)
instrEff.GvsS <- cramer_v(instrTable.GvsS)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'instrumentalness',
                  "group1" = 'General',
                  "group2" = 'Study',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(instrTest.GvsS$statistic[1]),
                  "df" = as.numeric(instrTest.GvsS$df),
                  "p" = as.numeric(instrTest.GvsS$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = instrEff.GvsS,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)


# General vs Sleep
instrTable.GvsSl <- table(subset(mergedData, category == 'General' | category == 'Sleep', select=c('category', 'instrRound')))
instrTable.GvsSl = instrTable.GvsSl[-2,]
instrTableProp.GvsSl <- prop.table(instrTable.GvsSl, 1)
instrTest.GvsSl <- chisq_test(instrTable.GvsSl)
instrEff.GvsSl <- cramer_v(instrTable.GvsSl)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'instrumentalness',
                  "group1" = 'General',
                  "group2" = 'Sleep',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(instrTest.GvsSl$statistic[1]),
                  "df" = as.numeric(instrTest.GvsSl$df),
                  "p" = as.numeric(instrTest.GvsSl$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = instrEff.GvsSl,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)



# Study vs Sleep
instrTable.SvsSl <- table(subset(mergedData, category == 'Study' | category == 'Sleep', select=c('category', 'instrRound')))
instrTable.SvsSl = instrTable.SvsSl[2:3,]
instrTableProp.SvsSl <- prop.table(instrTable.SvsSl, 1)
instrTest.SvsSl <- chisq_test(instrTable.SvsSl)
instrEff.SvsSl <- cramer_v(instrTable.SvsSl)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'instrumentalness',
                  "group1" = 'Study',
                  "group2" = 'Sleep',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(instrTest.SvsSl$statistic[1]),
                  "df" = as.numeric(instrTest.SvsSl$df),
                  "p" = as.numeric(instrTest.SvsSl$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = instrEff.SvsSl,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)

















#acousticness

plot(density(mergedData$acousticness))

# Due to distribution, round it, and then use chi-square.
# Use Cramer's V as effect size.
mergedData$acoRound <- round(mergedData$acousticness)

mergedData$acoRound <- as.factor(mergedData$acoRound)
acoTable <- table(subset(mergedData, category == 'General' | category == 'Study', select=c('category', 'acoRound')))
acoTable <- acoTable[1:2,]
acoTableProp <- prop.table(acoTable, 1)
acoTest <- chisq_test(acoTable)
acoOdds <- oddsratio(acoTable)
acoEff <- cramer_v(acoTable)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'acousticness',
                  "group1" = 'General',
                  "group2" = 'Study',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(acoTest$statistic[1]),
                  "df" = as.numeric(acoTest$df),
                  "p" = as.numeric(acoTest$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = acoEff,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)




acoTable <- table(subset(mergedData, category == 'General' | category == 'Sleep', select=c('category', 'acoRound')))
acoTable <- acoTable[-2,]
acoTableProp <- prop.table(acoTable, 1)
acoTest <- chisq_test(acoTable)
acoOdds <- oddsratio(acoTable)
acoEff <- cramer_v(acoTable)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'acousticness',
                  "group1" = 'General',
                  "group2" = 'Sleep',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(acoTest$statistic[1]),
                  "df" = as.numeric(acoTest$df),
                  "p" = as.numeric(acoTest$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = acoEff,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)


acoTable <- table(subset(mergedData, category == 'Study' | category == 'Sleep', select=c('category', 'acoRound')))
acoTable <- acoTable[2:3,]
acoTableProp <- prop.table(acoTable, 1)
acoTest <- chisq_test(acoTable)
#acoOdds <- oddsratio(acoTable)
acoEff <- cramer_v(acoTable)

# this needs to be coerced into a list to work

thisList = list(c(".y." = 'acousticness',
                  "group1" = 'Study',
                  "group2" = 'Sleep',
                  "n1" = 0,
                  "n2" = 0,
                  "statistic" = as.numeric(acoTest$statistic[1]),
                  "df" = as.numeric(acoTest$df),
                  "p" = as.numeric(acoTest$p),
                  "p.adj" = NA,
                  "p.adj.signif" = NA,
                  "effsize" = acoEff,
                  "magnitude" = 'NA'))


thisListDF = as.data.frame(matrix(unlist(thisList), nrow=length(thisList)))
colnames(thisListDF) <- colnames(statResults)
statResults <- rbind.fill(statResults, thisListDF)
























#loudness
plot(density(mergedData$loudness))
t.loudness.p <- mergedData %>%
  t_test(loudness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.loudness.eff <- mergedData %>%
  cohens_d(loudness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.loudness.p, y=t.loudness.eff)
statResults <- rbind.fill(statResults, thisResult)

#energy
plot(density(mergedData$energy))
t.energy.p <- mergedData %>%
  t_test(energy ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.energy.eff <- mergedData %>%
  cohens_d(energy ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.energy.p, y=t.energy.eff)
statResults <- rbind.fill(statResults, thisResult)

#valence
plot(density(mergedData$valence))
t.valence.p <- mergedData %>%
  t_test(valence ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.valence.eff <- mergedData %>%
  cohens_d(valence ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.valence.p, y=t.valence.eff)
statResults <- rbind.fill(statResults, thisResult)


#speechiness
plot(density(mergedData$speechiness))
t.speechiness.p <- mergedData %>%
  t_test(speechiness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.speechiness.eff <- mergedData %>%
  cohens_d(speechiness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.speechiness.p, y=t.speechiness.eff)
statResults <- rbind.fill(statResults, thisResult)

#liveness
plot(density(mergedData$liveness))
t.liveness.p <- mergedData %>%
  t_test(liveness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.liveness.eff <- mergedData %>%
  cohens_d(liveness ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.liveness.p, y=t.liveness.eff)
statResults <- rbind.fill(statResults, thisResult)

#tempo
plot(density(mergedData$tempo))
t.tempo.p <- mergedData %>%
  t_test(tempo ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
         var.equal = FALSE)
t.tempo.eff <- mergedData %>%
  cohens_d(tempo ~ category,comparisons = list(c('General', 'Study'), c('General', 'Sleep'), c('Study', 'Sleep')),
           var.equal = FALSE)

thisResult <- merge(x=t.tempo.p, y=t.tempo.eff)
statResults <- rbind.fill(statResults, thisResult)


# Write results out
names(statResults)[names(statResults) == '.y.'] <- 'AudioFeature'
statResults = adjust_pvalue(statResults, p.col='p', output.col = 'BONFERRONI', method='bonferroni')

write.csv(statResults, 'Three Way Comparison/AllComparisons_Statistics.csv')

# Plotting ----
# read in statResults if you want to start from here

statResults <- read.csv('Three Way Comparison/AllComparisons_Statistics.csv')
statResults <- subset(statResults, select = -c(X))

# manually fill in the significance stars for some of the tests
statResults[4,10] <- '****'
statResults[5,10] <- '****'
statResults[6,10] <- 'ns'
statResults[7,10] <- '****'
statResults[8,10] <- '****'
statResults[9,10] <- '****'



# Please obtain the RainCloudPlots files from the RainCloudPlots repository,
# and place them according to the relative path of the script.
# Instructions here: https://github.com/RainCloudPlots/RainCloudPlots
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')

library(raincloudplots)
source("RainCloudPlots-master/tutorial_R/R_rainclouds.R")
source("RainCloudPlots-master/tutorial_R/summarySE.R")
library(ggsignif)

#drop the weights column
mergedData <- subset(mergedData, select = -c(weight))

# width and height variables for saved plots
w = 5
h = 4
dpi=300


# Reshape the data
meltData <- melt(subset(mergedData, id=c('category')))

# danceability
# lets just do significance stars

install.packages( c( "dplyr", "ggplot2", "plyr" ) )

theseStats = subset(statResults, AudioFeature == 'danceability')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

p <- ggplot(subset(meltData, variable=='danceability'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Danceability (0-1)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Danceability') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/danceability1312.pdf', width = w, height = h, dpi=dpi)


#loudness
theseStats = subset(statResults, AudioFeature == 'loudness')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

p <- ggplot(subset(meltData, variable=='loudness'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Loudness (dB)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Loudness') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/loudness1312.pdf', width = w, height = h, dpi=dpi)

#energy
theseStats = subset(statResults, AudioFeature == 'energy')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]


p <- ggplot(subset(meltData, variable=='energy'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Energy (0-1)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Energy') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/energy1312.pdf', width = w, height = h, dpi=dpi)


#valence 
theseStats = subset(statResults, AudioFeature == 'valence')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

p <- ggplot(subset(meltData, variable=='valence'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Valence (1=positive)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Valence') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/valence1312.pdf', width = w, height = h, dpi=dpi)



#speechiness 
theseStats = subset(statResults, AudioFeature == 'speechiness')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]


p <- ggplot(subset(meltData, variable=='speechiness'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Speechiness (0-1)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Speechiness') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/speechiness1312.pdf', width = w, height = h, dpi=dpi)


#liveness 
theseStats = subset(statResults, AudioFeature == 'liveness')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

p <- ggplot(subset(meltData, variable=='liveness'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Liveness (0-1)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Liveness') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/liveness1312.pdf', width = w, height = h, dpi=dpi)



#tempo 
theseStats = subset(statResults, AudioFeature == 'tempo')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

p <- ggplot(subset(meltData, variable=='tempo'),aes(x=category, y=value, fill = category))+
  geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Tempo (bpm)')+xlab('')+
  geom_signif(
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Tempo') +
  theme(text = element_text(size=12))
ggsave('Three Way Comparison/tempo1312.pdf', width = w, height = h, dpi=dpi)



# now the bars
#instrumentalness 
theseStats = subset(statResults, AudioFeature == 'instrumentalness')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

instrDF <- as.data.frame(instrTableProp)

p <- ggplot(instrDF, aes(x=category, y=Freq, fill=instrRound))+
  geom_col() +
  #geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  #geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Percentage of instrumentalness')+xlab('')+
  geom_signif(
    y_position = c(1.1, 1.18, 1.26),
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+#+guides(fill = "none")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Instrumentalness') +
  theme(text = element_text(size=12)) +
  labs(colour = 'instrRound')
p
ggsave('Three Way Comparison/instrumentalness1312.pdf', width = w, height = h, dpi=dpi)



#acousticness 
theseStats = subset(statResults, AudioFeature == 'acousticness')
effsize = c(theseStats['effsize'][2,1],theseStats['effsize'][3,1], theseStats['effsize'][1,1])
effsize = round(effsize, digits = 2)
sign=c(theseStats['p.adj.signif'][2,1], theseStats['p.adj.signif'][3,1], theseStats['p.adj.signif'][1,1])
bracket = data.frame(') ')
statAnnotation = data.frame(effsize, sign, bracket)
statAnnotation$both = paste(statAnnotation$sign, statAnnotation$effsize, sep= ' (') 
statAnnotation$three = paste(statAnnotation$both, statAnnotation$X...., sep='')
statAnnotation = statAnnotation[, -c(1:4)]

mergedData$acoRound <- round(mergedData$acousticness)

mergedData$acoRound <- as.factor(mergedData$acoRound)
acoTable <- table(subset(mergedData, select=c('category', 'acoRound')))
acoTableProp <- prop.table(acoTable, 1)

acoDF <- as.data.frame(acoTableProp)

p <- ggplot(acoDF, aes(x=category, y=Freq, fill=acoRound))+
  geom_col() +
  #geom_flat_violin(position = position_nudge(x = 0, y = 0),adjust = 0.4)+
  #geom_boxplot(aes(x = as.numeric(category)-0.1, y = value),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") +
  #geom_point(position = position_jitter(width = .45), size = .1, alpha=0.02)+
  ylab('Percentage of acousticness')+xlab('')+
  geom_signif(
    y_position = c(1.1, 1.18, 1.26),
    comparisons = list(
      c('General', 'Study'),
      c('Study', 'Sleep'),
      c('General', 'Sleep')),
    annotation=statAnnotation,
    step_increase = .08
  ) +
  theme_cowplot()+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle('Acousticness') +
  theme(text = element_text(size=12))
p
ggsave('Three Way Comparison/acousticness1312.pdf', width = w, height = h, dpi=dpi)
