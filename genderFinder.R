
modNamesFolder <- "Data"
dir <- paste0(getwd(),"/",modNamesFolder,"/")
allNamesFile <- paste0(dir,"names.csv")
occFile <- paste0(dir,"letter_count.csv")
usersFile <- paste0(dir,"users_gender.csv")
customUserGenderFile <- paste0(dir,"custom_user_gender.csv")

# read data
usersDF <- read.csv(usersFile,stringsAsFactors=FALSE,encoding="UTF-8")
occDF <- read.csv(occFile,stringsAsFactors=FALSE,encoding="UTF-8")
namesDF <- read.csv(allNamesFile,stringsAsFactors=FALSE,encoding="UTF-8")
customUserGenderDF <- read.csv(customUserGenderFile,stringsAsFactors=FALSE,encoding="UTF-8")

# force certain users to have specified gender
if(dim(customUserGenderDF)[1]>0){
    ind <- match(customUserGenderDF$user_id, usersDF$user_id)
    usersDF$Gender[ind] <- customUserGenderDF$Gender
    usersDF$GenderPredicted[ind] <- FALSE
    usersDF$GenderFilled[ind] <- TRUE
    usersDF$Prob[ind] <- 0
}
    
    
arAlphabet <- c("ا","ب","ج","د","ه","و","ز","ح","ط","ي","ك","ل","م","ن","س","ع","ف","ص","ق","ر","ش","ت","ث","خ","ذ","ض","ظ","غ")
enAlphabet <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")

maxChars <- 4

predictGender <- function(name,occ,maxChars){
    name <- tolower(name)
    # prior probabilities
    pM <- 0.45
    pF <- 0.45
    pU <- 0.1
    countDF <- data.frame(Name=name,String="init",nMale=0,nFemale=0,nUnknown=0,pMale=pM,pFemale=pF,pUnknown=pU)
    # pDF <- data.frame(Name=name,String=NA,pMale=0.4,pFemale=0.4,pUnknown=0.1)
    letterCount <- 0
    predictSuccess <- TRUE
    for (i in 1:min(nchar(name),maxChars)){
        s <- substring(name,1,i)
        row <- which(occ$String == s)
        if (length(row)>0){
            # get number of occurances for each
            nM <- occ$nMale[row]
            nF <- occ$nFemale[row]
            nU <- occ$nUnknown[row]
            # calculate denomenator of Bayes' Rule
            den <- nM * pM + nF * pF + nU * pU
            # Update posterior using prior
            pM <- nM * pM / den
            pF <- nF * pF / den
            pU <- nU * pU / den
            countDF <- rbind(countDF,data.frame(Name=name,String=s,
                                                nMale=nM,
                                                nFemale=nF,
                                                nUnknown=nU,
                                                pMale=pM,pFemale=pF,pUnknown=pU))
            #if we get a 100% prob for male, female, or unknown we stop the loop
            if (pM == 1 || pF == 1 || pU == 1){
                break
            }
        }
    }
    # this counts how many letters we've looked at so far
    letterCount <- i
    if (pM > pF && pM > pU){
        gender <- "Male"
        prob <- pM
    }else if (pF > pM && pF > pU){
        gender <- "Female"
        prob <- pF
    }else if (pU > pM && pU > pF){
        gender <- "Unknown"
        prob <- pU
    }else{
        gender <- "NoMatch"
        prob <- 0
        predictSuccess <- FALSE
    }
    return (c(gender,prob,letterCount,predictSuccess,countDF))
}

# newNameIndeces <- which(usersDF$GenderFilled == FALSE)
## this loop fills the Gender column based on firstname exact matches
# for (i in 1:length(newNameIndeces)){
#     row <- newNameIndeces[i]
#     firstname <- usersDF$user_firstname[row]
#     ind <- which(namesDF$Name == tolower(firstname))
#     if (length(ind) > 0){
#         usersDF$Gender[row] <- namesDF$Gender[ind]
#         usersDF$Prob[row] <- 1.0
#         usersDF$GenderFilled[row] <- TRUE
#     }    
# }

newNameIndeces <- which(usersDF$GenderFilled == FALSE)
# this loop fills the Gender column based on firstname
for (i in 1:length(newNameIndeces)){
    row <- newNameIndeces[i]
    firstname <- usersDF$user_firstname[row]
    ind <- which(namesDF$Name == tolower(firstname))
    if (length(ind) > 0){
        # exact match found
        usersDF$Gender[row] <- namesDF$Gender[ind]
        usersDF$Prob[row] <- 1.0        
    }else{
        # predict gender using naive bayes classifier
        results <- predictGender(firstname,occDF,maxChars)
        gender <- results[1]
        prob <- results[2]
        letterCount <- results[3]
        predictSuccess <- results[4]
        usersDF$Gender[row] <- as.character(gender)
        usersDF$Prob[row] <- prob
        usersDF$GenderPredicted[row] <- predictSuccess
    }
    usersDF$GenderFilled[row] <- TRUE
}

usersDF$GenderPredicted <- as.logical(usersDF$GenderPredicted)
usersDF$Prob <- as.numeric(usersDF$Prob)

write.csv(usersDF,file=usersFile,row.names=FALSE)

table(factor(usersDF$Gender))
head(usersDF$user_firstname[which(usersDF$Gender == "NoMatch")],50)
tail(usersDF[which(usersDF$GenderPredicted == TRUE),c(2,3,4,5,6,8)],20)
predictGender("hanan",occDF,maxChars)
ind <- which(usersDF$Gender == "Male")
usersDF[ind[401:700],c(2,3,4,5,6,7,8)]

# commonNames <- unique(tolower(usersDF$user_firstname[which(usersDF$Gender == "Unknown")]))
# library(ggplot2)
# qplot(Gender,data=usersDF,geom="histogram")
# table(factor(usersDF$Gender))
# 
# name <- "أحمد"

