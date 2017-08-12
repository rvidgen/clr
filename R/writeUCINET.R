# write a DL file for input to UCINET
# change nodecount once number of auths is known by importing DL to UCINET
# this is an edge list - use the edgelist file routine
writeUCINET <- function(outfile, authors, nodecount){
txt1 = paste("DL n=", nodecount, sep="")
txt2 = "format = nodelist1"
txt3 = "labels embedded:"
txt4 = "data:"

filename = outfile
write(txt1, file=filename,append=FALSE)
write(txt2, file=filename,append=TRUE)
write(txt3, file=filename,append=TRUE)
write(txt4, file=filename,append=TRUE)

numarticles = length(authors)
numarticles

for (article in 1:numarticles) {
  numauthors = length(authors[[article]])
  numcoauths = numauthors - 1
  if (numcoauths > 0) { 
    for (author in 1:numcoauths) {  
      numdl = numauthors - author
      for (dl in 1:numdl) {  
        dlco = author + dl
        txtout = paste(authors[[article]][author], authors[[article]][dlco])
        write(txtout, file=filename,append=TRUE)
        txtout = paste(authors[[article]][dlco], authors[[article]][author])
        write(txtout, file=filename,append=TRUE)
      }
    }
  }
}

}
