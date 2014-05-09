

testit <- function(x = rnorm(99)) {
    pb <- txtProgressBar(style = 3)

    x.leng=length(x)

    nboot=1e5
    x.bootmean=numeric(nboot)


    for(i in 1:nboot) {
    x.boot=x[sample(x.leng,replace=T)]
    x.bootmean[i]=mean(x.boot)
    setTxtProgressBar(pb, i/nboot)
                      }
    close(pb)
    return(x.bootmean)
}
annie=testit()
hist(annie)
var(annie) #1/99




# Use the Tcl/Tk interface
l_ply(1:10000, identity, .progress = "tk")


 
# Text-based progress (|======|)
l_ply(1:10000, identity, .progress = "text")

# Choose a progress character, run a length of time you can see
l_ply(1:10000, identity, .progress = progress_text(char = "."))