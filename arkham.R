# Arkham Horror Investigator and Great Old One Randomizer
selecthero <<- "No Investigators chosen."
selectao <<- "No Great Old Ones chosen."
selecther <<- "No Heralds chosen."
selectgd <<- "No Guardians chosen."
selectin <<- "No Institutions chosen."
nhero <<- 0
nplay <<- 0


heroes <- function(numplayers = 1, nchar = 2, baseC = TRUE, dunwichC = FALSE, kingsportC = FALSE, innsmouthC = FALSE, calvinC = FALSE){
    nplay <<- numplayers
    nhero <<- nchar
    n <- c(1:numplayers)
    selecthero <<- "No Investigators chosen."
    
    basechar <- c("Amanda Sharpe", "Joe Diamond", "Monterey Jack", "Bob Jenkins", "Carolyn Fern", "Jenny Barnes", 
              "Michael McGlen", "Ashcan Pete", "Kate Winthrop", "Gloria Goldberg", "Mandy Thompson", 
              "Vincent Lee", "Darrell Simmons", "Dexter Drake", "Sister Mary", "Harvey Walters")
    dunchar <- c("Mark Harrigan", "Wilson Richards", "Jacqueline Fine", "Diana Stanley", "Marie Lambeau",
                 "Jim Culver", "Rita Young", "Leo Anderson")
    kingchar <- c("Charlie Kane", "Daisy Walker", "Lily Chen", "Lola Hayes", "Luke Robinson", "Rex Murphy", 
                  "Tony Morgan", "Wendy Adams")
    innchar <- c("Agnes Baker", "Akachi Onyele", "Finn Edwards", "George Barnaby", "Hank Samson", "Minh Thi Phan",
                 "Norman Withers", "Patrice Hathaway", "Roland Banks", "Silas Marsh", "Skids O'Toole", "Tommy Muldoon",
                 "Trish Scarborough", "Ursula Downs", "William Yorick", "Zoey Samaras")
    calchar <- c("Calvin Wright")
    
    if(baseC == FALSE & dunwichC == FALSE & kingsportC == FALSE & innsmouthC == FALSE & calvinC == FALSE) {
        selecthero <<- "No Investigators chosen."
        return(cat("Sanity Loss: No investigator sets chosen.\n"))
    }
    
    char <- c()
    
    if(baseC == TRUE) char <- c(char, basechar)
    if(dunwichC == TRUE) char <- c(char, dunchar)
    if(kingsportC == TRUE) char <- c(char, kingchar)
    if(innsmouthC == TRUE) char <- c(char, innchar)
    if(calvinC == TRUE) char <- c(char, calchar)
    
    totchar <- numplayers * nchar
    
    if (length(char) < totchar) return(cat("Sanity Loss: Too few investigators to choose from.\n"))
    
    if (totchar > length(char)) return(cat("Sanity Loss: Not enough investigates in the pool.\n"))
    
    selecthero <<- sample(char, totchar)    
    
    k <- 1
    for (i in 1:length(n)){
        if(nchar == 1){
            cat(paste("Player ", i, ", you shall face the forthcoming threat as: \n", sep = ""))
        } else{
            cat(paste("Player ", i, ", choose between the following investigators: \n", sep = ""))
        }
        for (j in 1:nchar){
            cat("  ", selecthero[k],"\n")
            k <- k + 1
        }
        cat("\n")
    }
    
}

GOO <- function(n = 1, baseA = TRUE, dunwichA = FALSE, kingsportA = FALSE, innsmouthA = FALSE, daolothA = FALSE){
    selectao <<- "No Ancient Ones chosen."
    baseao <- c("Azathoth", "Yig", "Hastur", "Shub-Niggurath", "Ithaqua", "Nyarlathotep", "Yog-Sothoth", "Cthulhu")
    dunao <- c("Shudde M'ell", "Tsathoggua", "Abhoth", "Glaaki")
    kinao <- c("Atlach-Nacha", "Eihort", "Y'Golonac", "Yibb-Tstll")
    innao <- c("Bokrug", "Chaugnar Faugn", "Cthugha", "Ghatanothoa", "Nyogtha", "Quachil Uttaus", "Rhan-Tegoth", "Zhar")
    daoao <- c("Daoloth")
    
    if(baseA == FALSE & dunwichA == FALSE & kingsportA == FALSE & innsmouthA == FALSE & daolothA == FALSE) {
        return(cat("Sanity Loss: Vague Other Worlds (no Ancient One sets chosen).\n"))
    }
    
    ao <- c()
    
    if(baseA == TRUE) ao <- c(ao, baseao)
    if(dunwichA == TRUE) ao <- c(ao, dunao)
    if(kingsportA == TRUE) ao <- c(ao, kinao)
    if(innsmouthA == TRUE) ao <- c(ao, innao)
    if(daolothA == TRUE) ao <- c(ao, daoao)
    
    if (length(ao) < n) return(cat("Sanity Loss: Too few Ancient Ones to choose from.\n"))
    
    selectao <<- sample(ao, n)
    
    if (n > 1){
        cat("Predestined knowledge! Do you choose to face: \n \n")
        for (i in 1:n){
            cat("   ", selectao[i], "\n")
        }
    } else {
    cat("The Great Old One awakening from its slumber is: \n \n", "   ", selectao, "\n")
    cat("\n")
    }
}

herald <- function(n = 1, dph = FALSE, dhh = FALSE, kiyh = FALSE, kingh = FALSE,
                   bgwh = FALSE, innh = FALSE, lth = FALSE){
    selecther <<- "No Heralds chosen."
    dpher <- c("The Dark Pharaoh")
    dhher <- c("The Dunwich Horror")
    kiyher <- c("The King in Yellow")
    kingher <- c("Ghroth", "Tulzscha")
    bgwher <- c("The Black Goat of the Woods")
    innher <- c("Father Dagon", "Mother Hydra")
    lther <- c("Lurker at the Threshold")
    
    if(dph == FALSE & dhh == FALSE & kiyh == FALSE & kingh == FALSE & bgwh == FALSE 
       & innh == FALSE & lth == FALSE) {

        return(cat("No Heralds chosen.\n"))
    }
    
    her <- c()
    
    if(dph == TRUE) her <- c(her, dpher)
    if(dhh == TRUE) her <- c(her, dhher)
    if(kiyh == TRUE) her <- c(her, kiyher)
    if(kingh == TRUE) her <- c(her, kingher)
    if(bgwh == TRUE) her <- c(her, bgwher)
    if(innh == TRUE) her <- c(her, innher)
    if(lth == TRUE) her <- c(her, lther)
    
    if (length(her) > 0 & length(her) < n) return(cat("Sanity Loss: Too few Heralds to choose from.\n"))
    
    selecther <<- sample(her, n)
    
    if (n > 1){
        cat("Your chosen Heralds are: \n \n")
        for (i in 1:n){
            cat("   ", selecther[i], "\n")
        }
    } else {
        cat("The Herald preparing the way for the Ancient One is: \n \n", "   ", selecther, "\n")
        cat("\n")
    }
}

guardians <- function(n = 1, king = FALSE){
    selectgd <<- "No Guardians chosen."    
    kingrd <- c("Bast", "Hypnos", "Nodens")
    
    if(king == FALSE) {
        return(cat("No Guardian set chosen.\n"))
    }
    
    guar <- c()
    
    if(king == TRUE) guar <- c(guar, kingrd)
    
    if (length(guar) > 0 & length(guar) < n) return(cat("Sanity Loss: Too few Guardians to choose from.\n"))
    
    selectgd <<- sample(guar, n)
    
    if (n > 1){
        cat("Your chosen Guardians are: \n \n")
        for (i in 1:n){
            cat("   ", selectgd[i], "\n")
        }
    } else {
        cat("The Guardian assisting in your adventure is: \n \n", "   ", selectgd, "\n")
        cat("\n")
    }
}

institutions <- function(n = 1, misk = FALSE){
    selectin <<- "No Institutions chosen."
    miskin <- c("Miskatonic University", "Organized Crime", "The Bureau of Investigations")
    
    if(misk == FALSE) {
        return(cat("No Institutions set chosen.\n"))
    }
    
    inst <- c()
    
    if(misk == TRUE) inst <- c(inst, miskin)
    
    if (length(inst) > 0 & length(inst) < n) return(cat("Sanity Loss: Too few Institutions to choose from.\n"))
    
    selectin <<- sample(inst, n)
    
    if (n > 1){
        cat("Your chosen Institutions are: \n \n")
        for (i in 1:n){
            cat("   ", selectin[i], "\n")
        }
    } else {
        cat("The Institution aiding in your adventure is: \n \n", "   ", selectin, "\n")
        cat("\n")
    }
}

summary <- function(selecthero, selectao, selecther, selectgd, selectin){
    summout <- list()   
    summout["Players"] <- list(c(selecthero))
    summout["Great Old Ones"] <- list(selectao)
    summout["Heralds"] <- list(selecther)
    summout["Guardians"] <- list(selectgd)
    summout["Institutions"] <- list(selectin)

    k <- 1
    if (nplay >= 1 & nhero >= 1 & summout$Players[[1]] != "No Investigators chosen."){
        for (i in 1:nplay){
            cat(paste("Player ", i, " selected investigators: \n", sep = ""))
            for (j in 1:nhero){
                cat("   ", summout[[1]][[k]])
                k <- k + 1
            }
            cat("\n\n")
        }
    } else {
        cat(paste("No Investigators chosen.", "\n"))
        cat("\n")
    }

    cat(paste("The chosen Great Old Ones are:\n", sep = ""))
    for (i in 1:length(selectao)){
        cat(paste("   ",summout[[2]][[i]], "\n"))
    }
    cat("\n")
    
    cat(paste("The chosen Heralds are:\n", sep = ""))
    for (i in 1:length(selecther)){
        cat(paste("   ",summout[[3]][[i]], "\n"))
    }
    cat("\n")
    
    cat(paste("The chosen Guardians are:\n", sep = ""))
    for (i in 1:length(selectgd)){
        cat(paste("   ",summout[[4]][[i]], "\n"))
    }
    cat("\n")
    
    cat(paste("The chosen Institutions are:\n", sep = ""))
    for (i in 1:length(selectin)){
        cat(paste("   ",summout[[5]][[i]], "\n"))
    }
    cat("\n")

}