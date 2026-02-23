# Welcome to the Clan Census Guessing Game!

This game randomly selects one of your dragons and presents you with a few clues about them, and it's your job to guess which dragon the game has chosen! 

## HOW TO PLAY:

### 1. Download game files 

Go to 
*https://github.com/bftweezers/ClanCensusR/tree/main/Clan_Census_Guessing_Game*
and download all the files
* README.md = instructions and information (you are here)
* clancensusgame_v1.0.R = the game file 
* census_sampledata.txt = an example of a data file that the game needs to run
* censusgame_readout.pdf = a file showing what the game displays when it's running

### 2. Create your data table 

On your computer using your preferred spreadsheet editor, create a table containing information about your dragons. Each dragon should have one row, and the columns should be titled like this
* Name = the names of your dragons
* Breed = their breeds 
* Gender = gender 
* P_Color = primary color
* P_Gene = primary gene
* S_Color = secondary color
* S_Gene = secondary gene
* T_Color = tertiary color 
* T_Gene = tertiary gene
* Element = element

Fill out your clan's info and save it as a tab-delimited text file. You can find a sample of what the file should look like in

*census_sampledata.txt*

**Using weird characters may make the game unable to read your file. Stick to only name formats allowed on the site (letters only, no spaces).**

### 3. Download and install R 

R is a free statistical program used for data analysis. If you don't already have R, go to 
*https://cloud.r-project.org/*
and download and install your preferred version of R. 

### 4. Run the game file

Open R. On the menu bar, go to 
*File > Source File*
or 
*File > Source R Code*
whichever one shows up for you. 

Choose the game file
*clancensusgame_v1.0.R*
Follow the prompts to begin playing! 

## Game Inputs Quick Reference
(accessed by typing 'gamehelp' within the game)
* gamemode: Choose the game mode you'd like to play
* randommode: Set the game mode to random
* colormode: Set the game mode to Color Combo
* genemode: Set the game mode to Gene Combo
* play: Begin playing
* count: Show how many dragons fit the criteria
* clue: Get another clue about the dragon
* reveal: Reveal the answer
* export: Export the game's extended census table to your folder (disabled by default)
* gamehelp: Show options
* exit: Exit the game

## PERMISSIONS:
You are free to iterate on my script, just please give attribution! My username and a link to my profile is sufficient. 

## ACKNOWLEDGMENTS
Special thanks to my friend for playtesting! 

## Additional Info

This script was written in R v3.6.3, which is a pretty old version of R. The script doesn't require any packages and I didn't use any crazy commands so hopefully it's robust across newer versions of R. 

The game contains data transformation capabilities! Loading your data into this game generates four new columns of information about your dragons:
* Breed_Type = displays whether your dragon is an Ancient or Modern breed
* Col_Patt = displays whether your dragon is xxx, xxy, xyx, xyy, or xyz
* Col_Combo = displays your dragon's full color combo 
* Gene_Combo = displays your dragon's full gene combo

You can download the expanded data file to your computer by enabling the 'export' functionality in the script. Data will save as
*extended-census-table.txt*

You don't actually have to type 'gamemode' first in order to select a game mode. All of the commands are on the same level. 
