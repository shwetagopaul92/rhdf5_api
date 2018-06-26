#' Function to create a file on hdf server
#' @param url character string with http server url
#' @note \url{"http://170.223.248.164:7248"}
#' @param domain character string with domain name to be created
#' @return r http response object
#' @examples
#' if (nchar(Sys.getenv("password"))>0) {
#'   tstring = sub("\\/", "", tempfile(tmpdir=""))
#'   dom = putDomain("http://170.223.248.164:7248", paste(tstring, ".hdfgroup.org", sep=""))
#'   readBin(dom$content, what="character")
#'   }
#' @export
putDomain = function(url, domain){
  require(httr)
  password = Sys.getenv("password")
  username = Sys.getenv("username")
  auth <- authenticate(username, password, type="basic")
  r = PUT(url=url, config=auth, add_headers(host=domain))
  r
}

#' Function to create a dataset in a file with extensible dimensions
#' @param url character string with http server url
#' @note \url{"http://170.223.248.164:7248/datasets"}
#' @param domain character string with domain name to be created
#' @param type character string with dataset type like H5T_IEEE_F32LE
#' @param shape integer array with initial dimensions of dataset
#' @param maxdims integer array with maximum extent of each dimension or 0 for unlimited dimension
#' @return r http response object
#' @examples
#' if (nchar(Sys.getenv("password"))>0) {
#'     tstring = sub("\\/", "", tempfile(tmpdir=""))
#'     dom = putDomain("http://170.223.248.164:7248", paste(tstring, ".hdfgroup.org", sep=""))
#'     ds = postDataset(url="http://170.223.248.164:7248/datasets",
#'         domain=paste(tstring, ".hdfgroup.org", sep=""),  
#'         type="H5T_IEEE_F32LE",
#'         shape=c(10,5), maxdims=c(0,5))
#'     ans = readBin(ds$content, what="character")
#'     ans
#'     }
#' @export
postDataset = function(url, domain, type, shape, maxdims){
  require(httr)
  password = Sys.getenv("password")
  username = Sys.getenv("username")
  auth <- authenticate(username, password, type="basic")
  args <- list(type=type, shape=shape, maxdims=maxdims)
  r = POST(url=url, body=args, config=auth, add_headers(host=domain), encode="json")
  r
}

#To grab UUID of dataset created :
#r = GET(url="http://170.223.248.164:7248/datasets",add_headers(host=domain))
#content(r)$datasets

#' Function to modify dataset shape
#' @param url character string with http server url
#' @note \url{"http://170.223.248.164:7248/datasets/dsetuuid/shape"}
#' @param domain character string with domain name to be created
#' @param newshape integer array with new dataset new dataset shape (works for a resizeable dataset)
#' @return r http response object
modifyShape = function(url, domain, newshape){
  require(httr)
  password = Sys.getenv("password")
  username = Sys.getenv("username")
  auth <- authenticate(username, password, type="basic")
  args <- list(shape=newshape)
  # PUT shape to resize the dataset
  r = PUT(url=url, body=args, config=auth, add_headers(host=domain), encode="json")
  r
}

#' Function to insert values into dataset
#' @param url character string with http server url
#' @note \url{"http://170.223.248.164:7248/datasets/dsetuuid/value"}
#' @param domain character string with domain name to be created
#' @param value list with values to be inserted into dataset
#' @param start (optional)integer/integer array with starting coordinate of selection to be updated
#' @param stop (optional)integer/integer array with ending coordinate of selection to be updated
#' @return r http response object
putValue = function(url, domain, value, start, stop){
  require(httr)
  password = Sys.getenv("password")
  username = Sys.getenv("username")
  auth <- authenticate(username, password, type="basic")
  # to update or add data to dataset, value must be a list
  if(missing(start)){
    args <- list(value=value)
  }
  else{
    args <- list(value=value, start=start, stop=stop)
  }
  r = PUT(url=url, body=args, config=auth, add_headers(host=domain), encode="json")
  r
}
