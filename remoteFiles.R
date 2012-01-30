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
fetch.remote.file <- function(server,service='vdsdata',root,file,refetch=FALSE){
##  tmp <- map.temp.files(file)
## if(refetch || length(tmp)==0){
    tmp <- tempfile('remotedata')
    uri <- paste(server,service,root,sep='/')
    ## try wrapping filename in quotes
    uri <- paste('"',uri,file,'"',sep='')
    print(paste('fetching',uri))
    system2('curl',paste('--retry 2 ',uri,sep=''),stdout=tmp,stderr=FALSE)
##   print(map.temp.files(file,tmp))
## }
  tmp
}
load.remote.file <- function(server,service='vdsdata',root,file){
  tmp <- fetch.remote.file(server,service,root,file)
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
  h = getCurlHandle()
  curlPerform(
              url = uri
              ,writefunction = reader$update
              ,curl=h
              )
  if(getCurlInfo(h)$response.code != 200) return (NULL)
  unlist(fromJSON(reader$value()))
}

get.wim.file <- function(file,server='http://lysithia.its.uci.edu:3000'){
  fetch.remote.file(server,service='wimdata',root='',file=file)
}


## save an R object to a file and put to the remote server
put.remote.file <- function(server='http://lysithia.its.uci.edu:3000',service='wimdata',path,o){
  tmp <- tempfile('localdata',fileext='.RData')
  save(o,file=tmp,compress='xz')
  uri <- paste(server,service,path,sep='/')
  print(uri)
  opts <- paste(' -T ', tmp,' -X PUT ',uri,sep='')
  print(opts)
  system2('curl',opts)
}

