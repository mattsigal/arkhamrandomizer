# Arkham Horror Investigator and Great Old One Randomizer

heroes <- function(numplayers = 1, nchar = 2, dunwich = TRUE){
    n <- c(1:numplayers)
    base <- c("Amanda Sharpe", "Joe Diamond", "Monterey Jack", "Bob Jenkins", "Carolyn Fern", "Jenny Barnes", 
              "Michael McGlen", "Ashcan Pete", "Kate Winthrop", "Gloria Goldberg", "Mandy Thompson", 
              "Vincent Lee", "Darrell Simmons", "Dexter Drake", "Sister Mary", "Harvey Walters")
    dunchar <- c("Mark Harrigan", "Wilson Richards", "Jacqueline Fine", "Diana Stanley", "Marie Lambeau",
                 "Jim Culver", "Rita Young", "Leo Anderson")
    ifelse(dunwich == TRUE, chars <- c(base, dunchar), chars <- base)
    
    totchar <- numplayers * nchar
    
    if (dunwich == FALSE & totchar > 16){
        return(cat("Sanity loss: Too many choices!"))
    }
    if (dunwich == TRUE & totchar > 24){
        return(cat("Sanity loss: Too many choices!"))
    }
    
    select <- sample(chars, totchar)
    
    k <- 1
    for (i in 1:length(n)){
        if(nchar == 1){
            cat(paste("Player ", i, ", you shall face the forthcoming threat as: \n", sep = ""))
        } else{
            cat(paste("Player ", i, ", choose between the following investigators: \n", sep = ""))
        }
        for (j in 1:nchar){
            cat("  ", select[k],"\n")
            k <- k + 1
        }
        cat("\n")
    }
}

GOO <- function(n = 1, dunwich = TRUE){
    base <- c("Azathoth", "Yig", "Hastur", "Shub-Niggurath", "Ithaqua", "Nyarlathotep", "Yog-Sothoth", "Cthulhu")
    dunao <- c("Shudde M'ell", "Tsathoggua", "Abhoth", "Glaaki")
    ifelse(dunwich == TRUE, ao <- c(base, dunao), ao <- base)
    select <- sample(ao, n)
    if (n > 1){
        cat("\nPredestined knowledge! Do you choose to face: \n \n")
        for (i in 1:n){
            cat("   ", select[i], "\n")
        }
    } else {
    cat("\nThe Great Old One awakening from its slumber is: \n \n", "   ", select, "\n")
    cat("\n")
    }
}

# EXAMPLES
# heroes(4)
# heroes(4,1)
# GOO(1)
# GOO(2)