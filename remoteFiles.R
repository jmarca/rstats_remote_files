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
    file.create(tmp)
    print(paste('created',tmp))
    uri <- paste(server,service,root,sep='/')

    ## URI escape the filename
    ## try wrapping filename in quotes
    uri <- paste('"',uri,URLencode(file),'"',sep='')
    gc()
    print(paste('fetching',uri))
    system2('curl',paste('--retry 2 -s -S -o',tmp,uri),stdout=FALSE,stderr=TRUE,wait=TRUE)
##   print(map.temp.files(file,tmp))
## }
    result='failure'
    r <- try(result <-  load(tmp, .GlobalEnv))
    if(class(r) == "try-error") {
      return(r)
    }
    unlink(tmp)
    result
}
load.remote.file <- function(server,service='vdsdata',root,file,deprecated=TRUE){
  ## deprecated
  fetch.remote.file(server,service,root,file)
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

get.wim.file <- function(file,server){
  fetch.remote.file(server,service='wimdata',root='',file=file)
}


## save an R object to a file and put to the remote server
put.remote.file <- function(server='http://calvad.ctmlabs.net',service='wimdata',path,o){
  tmp <- tempfile('localdata',fileext='.RData')
  save(o,file=tmp,compress='xz')
  uri <- paste(server,service,path,sep='/')
  print(uri)
  opts <- paste(' -T ', tmp,' -X PUT ',uri,sep='')
  print(opts)
  system2('curl',opts)
}

