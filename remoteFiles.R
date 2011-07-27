library('RCurl')
## this file also requires a working curl, accessed by the system2 command

## global to store files
filemap <- data.frame('tmpfile'='notarealfile','file'='notarealfile',stringsAsFactors=FALSE)

## keep track of temporary files
map.temp.files <- function(file,tmpfile=NULL){

  result <- c()
  if( length(tmpfile) == 0 ){
    result = filemap$tmpfile[filemap$file==file]
  }else{
    print('storage case')
    filemap <<- rbind(filemap,list('tmpfile'=tmpfile,'file'=file))
    result <- filemap$tmpfile[filemap$file==file]
  }
  result

}

## delete temporary files when done with them, and remove from map
unmap.temp.files <- function(file){

  tmp <- map.temp.files(file)
  filemap <<- filemap[filemap$file==file,]
  unlink(tmp)

}

## go get the remote files, and track their temporary location
load.remote.file <- function(server,service,root,file){
  tmp <- map.temp.files(file)
  if(length(tmp)==0){
    tmp <- tempfile('remotedata')
    uri <- paste(server,service,root,sep='/')
    system2('curl',paste('--retry 4 ',uri,file,sep=''),stdout=tmp,stderr=FALSE)
    print(map.temp.files(file,tmp))
  }
  load.result <-  load(file=tmp)
  df
}


## send a request to the file serving machine for a list of files matching pattern
get.filenames <- function(server='http://localhost:3000'
                          ,service='vdsdata'
                          ,base.dir='D12'
                          ,pattern="RData$"){
  uri=paste(server,service,base.dir,sep="/")
  uri = paste(uri,paste('pattern=',pattern,sep=''),sep='?')
  print(uri)
  reader = basicTextGatherer()
  curlPerform(
              url = uri
              ,writefunction = reader$update
              )
  unlist(fromJSON(reader$value()))
}

