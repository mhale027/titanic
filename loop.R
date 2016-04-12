split.fam <- function(family) {
    if (all(family$fam_size == nrow(family))) {
        
        print(family)
        next
        
    } else if (all(family$single == 1)) {    
        print(family)
        next
        
        
    } else {
        ticket1 <- mlv(as.numeric(family$Ticket))[[1]]
        if (ticket1 %% 1 > 0) {ticket1 <- family[1,]$Ticket}
        fam.size1 <- mlv(as.numeric(family$fam_size))[[1]]
        
        
        #        family1 <- family[family$fam_size == fam.size1 & family$Ticket == ticket1 ,]
        family1 <- family[family$Ticket == ticket1 ,]
        rename <- paste0(family1$fam_name[1], "()")
        family1$fam_name <- rename
        
        for (j in 1:nrow(family1)) {
            id1[j] <- family1[j,]$PassengerId
            family[family$PassengerId == id1[j],] <- family1[j,]
        }
        
        
        
    }
}