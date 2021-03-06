% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhdf5api.R
\name{putValue}
\alias{putValue}
\title{Function to insert values into dataset}
\usage{
putValue(url, domain, value, start, stop, step)
}
\arguments{
\item{url}{character string with http server url}

\item{domain}{character string with domain name to be created}

\item{value}{list with values to be inserted into dataset}

\item{start}{(optional)numeric vector with starting coordinate of selection to be updated}

\item{stop}{(optional)numeric vector with ending coordinate of selection to be updated}

\item{step}{(optional)numeric vector with step value}
}
\value{
r http response object
}
\description{
Function to insert values into dataset
}
\note{
\url{"http://170.223.248.164:7248/datasets/dsetuuid/value"}
}
\examples{
if (nchar(Sys.getenv("password"))>0) {
    tstring = sub("\\\\/", "", tempfile(tmpdir=""))
    dom = putDomain("http://170.223.248.164:7248", paste(tstring, ".hdfgroup.org", sep=""))
    ds = postDataset(url="http://170.223.248.164:7248/datasets",
        domain=paste(tstring, ".hdfgroup.org", sep=""),  
        type="H5T_IEEE_F32LE",
        shape=c(2,2), maxdims=c(0,0))
    response = GET(url="http://170.223.248.164:7248/datasets",
              add_headers(host=paste(tstring, ".hdfgroup.org", sep="")))
    dsetuuid = content(response)$datasets[[2]]     # taking the required dataset id 
    modShape = modifyShape(url=paste0("http://170.223.248.164:7248/datasets/",dsetuuid,"/shape"),
              domain=paste(tstring, ".hdfgroup.org", sep=""), newshape=c(3,3))
    mymat = matrix(1:9, nrow=3, ncol=3)
    insertVal = putValue(url=paste0("http://170.223.248.164:7248/datasets/",dsetuuid,"/value"),
               domain=paste(tstring, ".hdfgroup.org", sep=""), value=mymat,
               start=c(0,0),stop=c(3,3),step=c(1,1))
    }
}
