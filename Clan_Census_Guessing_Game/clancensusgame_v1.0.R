# source("clancensusgame_v1.0.R")
 
cat("\nHello! Welcome to the Clan Census Guessing Game! (v1.0)\n")
 
# load default game mode (random game mode)
drawing <- 'csus[sample(nrow(csus), size = 1), rclmn, drop=FALSE]' 
firstrclmn <- 'sample(clmnpick, size=2)'
 
# load file path
repeat{
cat("\nPlease type the path to the folder containing your clan's data file.
Example: /Users/yourname/Documents/folder1/folder2/foldercontainingfile\n")
pathname <- readline(prompt="Folder path: ")
 
if(pathname == "exit"){
stop("Exiting the game.")
} else {
}
pathtry <- try(setwd(pathname), silent=T)
if(!inherits(pathtry, "try-error")){
cat("Path loaded successfully.\n")
break 
} else {
message("Could not locate path. Please try again.")
}
}
 
# load clan data
repeat{
cat("\nPlease type the name of your clan's data file.
Example: myclandata.txt\n")
filename <- readline(prompt="File name: ")
if(filename == "exit"){
stop("Exiting the game.")
} else {
}
filetry <- try(read.table(filename, header=T, stringsAsFactors=F) -> csus, silent=T)
if(!inherits(filetry, "try-error")){
csus$Breed_Type <- ifelse(
csus$Breed == "Aberration" | 
csus$Breed == "Aether" | 
csus$Breed == "Auraboa" | 
csus$Breed == "Banescale" | 
csus$Breed == "Cirrus" | 
csus$Breed == "Dusthide" | 
csus$Breed == "Everlux" | 
csus$Breed == "Gaoler" | 
csus$Breed == "Sandsurge" | 
csus$Breed == "Thorntail" | 
csus$Breed == "Undertide" | 
csus$Breed == "Veilspun", 
"Ancient", "Modern")
 
csus$Col_Combo <- paste(csus$P_Color, csus$S_Color, csus$T_Color, sep="_")
 
csus$Col_Patt <- ifelse(
csus$P_Color == csus$S_Color & csus$P_Color != csus$T_Color, "xxy", ifelse(
csus$P_Color == csus$T_Color & csus$P_Color != csus$S_Color, "xyx", ifelse(
csus$S_Color == csus$T_Color & csus$P_Color != csus$T_Color, "xyy", ifelse(
csus$P_Color == csus$S_Color & csus$P_Color == csus$T_Color, "xxx", 
"xyz"))))
 
csus$Gene_Combo <- paste(csus$P_Gene, csus$S_Gene, csus$T_Gene, sep="_")
 
cat("File loaded successfully.
 
What would you like to do?
 
   gamemode: Choose the game mode you'd like to play
   play: Begin playing
   gamehelp: Show options
   exit: Exit the game\n")
break 
} else {
message("Could not locate file. Please try again.\n")
}
}
 
### Main Body ###
 
# creating input field
repeat {
cat("\nPlease type the word that corresponds to your selection. 
For help, type 'gamehelp'. \n")
choice <- readline(prompt="Selection: ")
 
# displaying game mode info
if(choice == "gamemode"){
cat("\nGame Modes:
   randommode: Guess from any two dragon attributes (default)
   colormode: Guess from the dragon's color combo
   genemode: Guess from the dragon's gene combo\n")
 
# setting game mode to random mode
} else if(choice == "randommode"){
drawing <- 'csus[sample(nrow(csus), size = 1), rclmn, drop=FALSE]'
firstrclmn <- 'sample(clmnpick, size=2)'
cat("Random mode chosen. Type 'play' to begin a new game.\n")
 
# setting game mode to color combo mode
} else if(choice == "colormode"){
drawing <- 'csus[sample(nrow(csus), size = 1), "Col_Combo", drop=FALSE]'
firstrclmn <- 'integer()'
cat("Color mode chosen. Type 'play' to begin a new game.\n")
 
# setting game mode to gene combo mode
} else if(choice == "genemode"){
drawing <- 'csus[sample(nrow(csus), size = 1), "Gene_Combo", drop=FALSE]'
firstrclmn <- 'integer()'
cat("Gene mode chosen. Type 'play' to begin a new game.\n")
 
# generating a guessing prompt
} else if(choice == "play"){
clmnpick <- which(colnames(csus) %in% c("Breed", "Breed_Type", "Gender", "P_Color", "P_Gene", "S_Color", "S_Gene", "T_Color", "T_Gene", "Element", "Col_Patt"))
rclmn <- eval(parse(text = firstrclmn))
randsel <- eval(parse(text = drawing))
trandsel <- as.data.frame(t(randsel))
colnames(trandsel) <- NULL
fits <- merge(randsel, csus, by=names(randsel))
fitnum <- nrow(fits)
revans <- csus[rownames(randsel),]
trevans <- as.data.frame(t(revans))
colnames(trevans) <- NULL
 
cat("Which dragon is this?\n")
print(trandsel)
 
cat("\nType the name of the dragon!
Or type one of the following...
   count: Show how many dragons fit the criteria
   clue: Get another clue about this dragon
   reveal: Reveal the answer\n")
 
# count how many dragons fit the criteria
} else if(choice == "count"){
if(fitnum == 1){
cat(fitnum, "dragon fits this criteria\n", sep=" ")
} else {
cat(fitnum, "dragons fit this criteria\n", sep=" ")
}
 
# give additional criteria
} else if(choice == "clue"){
if(length(clmnpick) > 1){
clmnpick <- clmnpick[!(clmnpick %in% rclmn)]
rclmn <- ifelse(length(clmnpick) > 1, sample(clmnpick, size=1), clmnpick)
morr <- csus[rownames(randsel), rclmn, drop=FALSE]
tmorr <- as.data.frame(t(morr))
colnames(tmorr) <- NULL
randsel <- cbind(randsel, morr)
trandsel <- as.data.frame(t(randsel))
colnames(trandsel) <- NULL
fits <- merge(randsel, csus, by=names(randsel))
fitnum <- nrow(fits)
print(trandsel)
cat("Make a guess?\n")
} else {
cat("There are no more clues! Please make a guess, or type 'reveal' to reveal the answer.\n")
}
 
# reveal the answer
} else if(choice == "reveal"){
print(trevans)
cat("\nTo start another round, type 'play'.\n")
 
# display help options
} else if(choice == "gamehelp"){
cat("\nTo win, type the name of the dragon that matches the clues!
 
Options: 
   gamemode: Choose the game mode you'd like to play
   randommode: Set the game mode to random (default)
   colormode: Set the game mode to Color Combo
   genemode: Set the game mode to Gene Combo
   play: Begin playing
   count: Show how many dragons fit the criteria
   clue: Get another clue about the dragon
   reveal: Reveal the answer
   export: Export the game's extended census table to your folder (disabled by default)
   gamehelp: Show options
   exit: Exit the game\n")
 
# exit the game
} else if(choice == "exit"){
stop("Exiting the game.")
 
# exporting the data table 
# } else if(choice == "export"){
# cat(paste("File saved to ", pathname, "/extended-census-table.txt\n", sep=""))
# write.table(csus, "extended-census-table.txt", row.names=F, col.names=T, quote=F, sep="\t")
 
# checking a guess
} else {
if(choice == csus[rownames(randsel), "Name"]){
cat("Correct! Great job!\n")
print(trevans)
cat("\nCorrect! Great job!
To start another round, type 'play'.\n")
} else {
message("Please try again.\n")
}
}
}
