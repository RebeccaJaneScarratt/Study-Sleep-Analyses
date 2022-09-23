# -*- coding: utf-8 -*-
"""
Created on Thu Dec  9 08:17:08 2021
@author: olehe
"""

#%% Do imports

# for handling data
import pandas as pd
import numpy as np
import os 

os.chdir('C:\\Users\\au672599\\OneDrive - Aarhus Universitet\\Documents\\PhD Rebecca\\SpotifyStudyMusic')



#%% Load some data

data = pd.read_csv('Data/SPD_withClusters_and_reducedGenre.csv', encoding='UTF-8', na_values='')
allGenres = data['Genres']

largeGenres = ['electronic',
				'world',
				'pop',
				'country',
				'blues',
				'jazz',
				'bluegrass',
				'folk',
				'classical',
				'gospel',
				'opera',
				'rock',
				'punk',
				'alternative',
				'metal',
				'rap',
				'r&b',
				'funk',
				'reggae',
				'soundtrack',
				'oldies',
				'ambient',
				'instrumental',
				'indie',
				'meditation',
				'house',
				'lo-fi',
				'christian',
				'new age',
				'sleep',
				'background',
				'lullaby']


genresRecat = ['piano cover',
				'musica de fondo',
				'spa',
				'nu age',
				'focus beats',
				'brain waves']

mapGenres = {'piano cover':'instrumental',
			 'musica de fondo':'background',
			 'spa':'meditation',
			 'nu age':'new age',
			 'focus beats':'lo-fi',
			 'brain waves':'ambient'}
#%% Make genre-reduction function

# this function gets the key in a dict with the highest value
def getKeyMaxVal(d):
	v=list(d.values())
	k=list(d.keys())
	
	if sum(d.values()) == 0:
		return 'uncategorized'
	
	return k[v.index(max(v))]

def reduceGenre(microGenres, targetGenres):
	# reduce string
	microGenres = microGenres.replace('[', '').replace(']', '').replace('\'','').split(',')
	# handle "unknown"-cases
	if len(microGenres) == 1 and microGenres[0] == 'unknown':
		thisGenre = 'unknown'
		return thisGenre
	
	# implement the algorithm here
	# turn target genres into a dict
	occurrenceDict = dict((genre,0) for genre in targetGenres)
	# now loop through microGenres
	for microGenre in microGenres:
		# check for space at start
		if microGenre[0] == ' ':
			microGenre = microGenre[1:]
		# do recategorization here
		if microGenre in genresRecat:
			microGenre = mapGenres[microGenre]
		
		
		for target in targetGenres:
			# remember to convert to same upper or lower case
			if target.lower() in microGenre.lower():
				occurrenceDict[target] += 1
		
	# now do the argmax
	thisGenre = getKeyMaxVal(occurrenceDict)
	
	
	return thisGenre


#%% Preprocess

data['reducedGenre'] = data.apply(lambda row: reduceGenre(row['Genres'], largeGenres), axis=1)



data['reducedGenre'].value_counts()

genres_in_sleep = data['reducedGenre'].value_counts()

uncategorized = data[data['reducedGenre']=='uncategorized']

uncatGenres = []

for genre in uncategorized['Genres']:
	theseGenres = genre.replace('[', '').replace(']', '').replace('\'','').split(',')
	for thisGenre in theseGenres:
		uncatGenres.append(thisGenre)

uncatGenresDF = pd.DataFrame(data=uncatGenres)
count = uncatGenresDF[0].value_counts()

#%% Make new files

genres_in_sleep.to_csv('Genres_in_Sleep.csv')

data.to_csv('SPD_withClusters_and_reducedGenre.csv')
