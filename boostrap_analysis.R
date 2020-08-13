VAF <- read.csv("Tumor_VAF.csv",colClasses = "character")
VAF$AF_PRIMARY <- as.numeric(as.numeric(VAF$AF_PRIMARY)>0)
VAF$AF_LUNG <- as.numeric(as.numeric(VAF$AF_LUNG)>0)
VAF$AF_BONE <- as.numeric(as.numeric(VAF$AF_BONE)>0)

IDs <- unique(VAF$ID)

for(ID in IDs){
  tmp <- VAF[VAF$ID==ID,]
  lung_share <- sum(tmp$AF_PRIMARY==1 & tmp$AF_LUNG==1)
  bone_share <- sum(tmp$AF_PRIMARY==1 & tmp$AF_BONE==1)
  boostrap_val <- 0
  for(i in 1:1000){
    for(j in 1:nrow(tmp)){
      tmp[j,7:9] <- sample(tmp[j,7:9])
    }
    boostrap_val <- boostrap_val + as.numeric( (sum(tmp$AF_PRIMARY==1 & tmp$AF_LUNG==1) - sum(tmp$AF_PRIMARY==1 & tmp$AF_BONE==1)) <= (lung_share - bone_share) )
  }
  print(boostrap_val/1000)
}

#library(metaseqR)
#fisher.method(matrix(c(0.001,0.001,0.002,0.004,0.927),nrow=1))
