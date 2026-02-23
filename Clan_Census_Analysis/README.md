# Clan Census Analysis

## Input format 
You'll need to have two input files. 
### 1. Your clan's data
Tab-delimited text file containing the following columns: 
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

Avoid using spaces and special characters in your text file. See an example of a properly formatted file in
*census_sampledata.txt*
### 2. Hexcodes of colors
*allcolors_hex.txt*

## Edit script before use 
The script requires some editing before use. Define your working directory and input filename. Some commands in the script are also disabled by default behind the hash delimiter. You can enable these commands by editing your copy of the script.

## Features
### Calculate information about your dragons immediately 
These columns appended to your dataframe
* Breed_Type = lists if dragon is ancient or modern
* Col_Patt = lists dragon as xxx, xxy, xyz, etc. 
* Col_Mult = consolidates all xxy/xyx/xyy into "Double"
* Col_Combo = lists color combo as one string 
* Gene_Combo = list gene combo as one string

Save your updated dataframe by enabling the write.table command in the script. Table saved to
*clancensus_more.txt*
### Discover your most used colors, genes, etc. 
These variables generated
* csus = the whole table 
* oelemf = element frequency
* obtypef = ancient vs modern frequencies
* obreedf = most used breeds
* (prim/sec/tert)colto = most used color in each slot 
* acolto = most used color 
* otccombo = most used color combos
* cpatt = color pattern counts
* (prim/sec/tert)geneto = most used gene in each slot 
* ageneto = most used gene
* otgcombo = most used gene combos
* fcoln = colors you don't have 
### Generate various plots about your clan's color usage, gene usage, and more
(You can enable the commands by editing the script.) 
* elem_dist_flat.png = bar plot of element counts in order of abundance
* breed_dist_flat.png = bar plot of top five most abundant breeds 
* color_dist_flat.png = bar plot of color counts (excluding basic tert) in color wheel order
* color_dist_pie.png = pie chart of color counts (excluding basic tert) in color wheel order
* color_dist_ordered.png = bar plot of color counts (excluding basic tert) in order of abundance
### Functions to search your clan by color, gene, breed, etc. 
* csearch or qsearch = return rows (csearch) or names (qsearch) that match at least one string (the "OR" operator)
	* csearch("stringtofind","another","another",...) or qsearch("stringtofind",...)
* cmatch or qmatch = return rows (cmatch) or names (qmatch) that match every string (the "AND" operator)
	* cmatch(arg="stringtofind", anotherarg="another"...)
* arguments for cmatch/qmatch
	* breed = the breed of the dragons
	* btype = whether the dragons are ancient or modern
	* gend = the gender of the dragons
	* pcol = primary color 
	* pgene = primary gene 
	* scol = secondary color 
	* sgene = secondary gene 
	* tcol = tertiary color 
	* tgene = tertiary gene 
	* elem = element
	* cpatt = whether dragon is xxx, xyz, etc. 
	* col1, col2, col3 = search for colors regardless of which slot they're in
	* cmult = whether  dragons are Triple, Double, or XYZ
	* is.Basic = Boolean condition for whether the dragons are basic (TRUE), not basic (FALSE), or could be either (don't define the argument)

## Permissions
You are free to iterate on my script, just please give attribution! My username and a link to my profile is sufficient. 

## Specs 
Made in R 3.6.3
