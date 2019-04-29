# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

nmf.divisiveWords <- function(dataset,clus_num,species=NULL) {
  res <- NMF::nmf(dataset,5)

  dims <- list( features=rownames(dataset), samples=colnames(x), basis=paste('', 1:nbasis(res), sep='') )

  dimnames(res) <- dims
  res.basis <- NMF::basis(res)
  res.coef <- NMF::coef(res)

  res.basis.temp <- cbind(res.basis,apply(res.basis, 1, max))
  rownames(res.basis.temp) <- sub(".txt", "", rownames(res.basis.temp))
  res.assign <- colnames(res.basis.temp)[apply(res.basis.temp,1,which.max)]

  res.assign <- data.frame(res.assign)
  rownames(res.assign) <- rownames(res.basis.temp)

  res.quant <- stats::quantile( res.coef, p = 0.75 )
  res.report <- apply(res.assign, 1, function(x){ colnames(res.coef)[res.coef[x,] > res.quant] })
  #if species then return species, else return whole thing.
  if(!is.na(species)){
    return(res.report[species])
  } else {
    return(res.report)
  }
}

corpus.similarDocs <- function() {}

corpus.clustGuessing <- function() {}

