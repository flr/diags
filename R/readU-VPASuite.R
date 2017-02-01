utils::globalVariables(c("mfclVcov"))
utils::globalVariables(c("str_trim"))

iUVPASuite = function(file., sep="", quiet=TRUE, cchar='#', na.strings="NA") {
      
    # Number of Indices
    num	= (scan(file., skip=1, nlines=1, sep=sep, quiet=quiet, comment.char=cchar))-100
    
    #Description 
    desc = paste(scan(file., nlines=1, what="character", sep="\n", quiet=quiet, comment.char=cchar), ". Imported from VPA file.")

    # Pascal stuff
    v = -2; w = -1; x = 0; y = 1; z = 2; nyrs = 0

    res=list(range=NULL,smry=NULL,index=NULL)
    for(i in seq(num)) {

        # Values by Index
        v = v + 4+nyrs
        w = w + 4+nyrs
        x = x + 4+nyrs
        y = y + 4+nyrs
        z = z + 4+nyrs

        # Year range
        yrs	= scan(file., skip=w, nlines=1, sep=sep, quiet=quiet, comment.char=cchar)
        nyrs  = yrs[2]-yrs[1]+1 

        # Age range
        ages  = scan(file., skip=y, nlines=1, sep=sep, quiet=quiet, comment.char=cchar)
        nages = ages[2]-ages[1]+1

        # Alpha and beta, i.e. fishing season
        AB	= scan(file., skip=x, nlines=1, sep=sep, quiet=quiet,comment.char=cchar, na.strings=na.strings)
        alpha = AB[3]
        beta  = AB[4]
        
        cols=expand.grid(year=yrs[1]:yrs[2],age=ages[1]:ages[2])
                          
        # retrieve information for the effort slot
        eff = read.table(file=file., skip=z, nrows=nyrs ,sep=sep, comment.char=cchar, na.strings=na.strings)[1:nyrs,1]

        # retrieves catch numbers at age
        catch = read.table(file=file., skip=z, nrows=nyrs,sep=sep, comment.char=cchar, na.strings=na.strings)[1:nyrs,2:(nages+1)]

        # retreives the names for each fleet
        name  = scan(file., skip=v, nlines=1, what="character", quiet=quiet, comment.char=cchar, sep="#", strip.white=TRUE)    
        
        u.   =cbind(name=name,cols,effort=eff,catch=unlist(c(catch)))
        smry = c(range(cols$age), startf=alpha,endf=beta)
        rng  = c(range(cols$age), NA, range(cols$year))
        
        names(rng)[1:5]=c("minage","maxage","plusgroup","minyr","maxyr")
        
        res$range=rbind(res$range,rng)
        res$smry =rbind(res$smry, smry)
        res$cpue =rbind(res$cpue, u.)
        }
   
    res.=res$cpue
    res.$index=res.$catch/res.$effort
    
    row.names(res.)=NULL
    
    attributes(res.)$smry=res$smry
    
    return(res.[c(1,2,3,6,4,5)])}