x="/home/laurie/Desktop/ICCAT/SCRS/Courses/SS3/Shared Documents/Data/Examples/ALB_Simple"

iUSS=function(x) {
   res=SS_output(x)$cpue[,c(2,3,4,7,8,11,12,13)]
   
   names(res)=c("name","year","season","obs","hat","se","residual","ll")
   }
