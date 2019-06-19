.GlobalEnv$oc.init <- function(...) {
    ## remove myself from the global env since my job is done
    if (identical(.GlobalEnv$oc.init, oc.init)) rm(oc.init, envir=.GlobalEnv)

    Rserve:::ocap(call.script, "call.script")
}

auto.convert.ext <- c(js = "application/javascript", css ="text/css", html = "text/html",
                      png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
                      tiff = "image/tiff", tif = "image/tiff", svg = "image/svg+xml",
                      pdf = "application/pdf"
                      )

ext2mime <- function(fn) {
    fn <- basename(fn)
    type <- "text/plain"
    if (length(grep(".", fn, fixed=TRUE))) {
        nt <- auto.convert.ext[tolower(gsub(".*\\.","",fn))]
        if (!any(is.na(nt))) type <- as.vector(nt)
    }
    type
}

URIdecode <- function(o) sapply(o, function(o) URLdecode(gsub("+", " ", o, fixed=TRUE)))

URIparse <- function(o) {
    if (is.raw(o)) o <- rawToChar(o)
    body <- strsplit(o, "&", TRUE)[[1]]
    vals <- gsub("[^=]+=", "", body)
    if (length(vals)) vals <- URIdecode(vals)
    keys <- gsub("=.*$", "", body)
    names(vals) <- keys
    vals    
}

call.script <- function(packed, ...)
    tryCatch({
        w <- which(packed == as.raw(0L))[1:3]
        url <- rawToChar(packed[1L : (w[1L] - 1L)])
        query <- if (w[2L] > w[1L] + 1L) rawToChar(packed[(w[1L] + 1L):(w[2L] - 1L)]) else character()
        headers <- if (w[3L] > w[2L] + 1L) packed[(w[2L] + 1L):(w[3L] - 1L)] else raw()
        body <- if (w[3L] < length(packed)) packed[(w[3L] + 1L):length(packed)] else NULL
	# ulog(paste0("INFO: call.script: ", url, " (body.len=", length(body), ")"))
        hs <- rawToChar(headers)
        if (length(grep("Content-Type: application/x-www-form-urlencoded", hs, TRUE)))
            body <- URIparse(body)
        if (length(query))
            query <- URIparse(query)
        res <- .http.request(url, query, body, headers)
        ## if the result is a list containing "file" entry we need to serve it
        if ("file" %in% names(res))
            res <- tryCatch(list(rawToChar(readBin(res$file, raw(), file.info(res$file)$size)),
                                 ext2mime(res$file)),
                            error=function(e) paste("cannot open", res$file))
        res
    }, error=function(e) list(paste0("Evaluation error: ", e), "text/plain", character(), 500L))

## this serves Rserve's built-in HTTP server
.http.request <- function(url, query, body, headers, ...) {
    ## process everything else
    if (isTRUE(url == "") || isTRUE(url == "/")) url <- "/index.html"

    path.info <- NULL
    ## serve files from the htdocs directory
    fn <- file.path("/var/www", url)
    self.path <- gsub("//+", "/", file.path("/", url))
    if (!file.exists(fn)) {
      ## try to support PATH_INFO-like access
      htdocs <- "/var/www"
      if (file.exists(htdocs)) {
        exf <- strsplit(url, "/", TRUE)[[1L]]
        valid <- htdocs
        self.path <- "/"
        while (length(exf) && isTRUE(file.info(valid)$isdir)) {
          valid <- file.path(valid, exf[1L])
          self.path <- file.path(self.path, exf[1L])
          exf <- exf[-1L]
        }
        if (isTRUE(!file.info(valid)$isdir)) {
          fn <- valid
          path.info <- paste(exf, collapse=.Platform$file.sep)
          self.path <- gsub("//+", "/", self.path)
        }
      }
    }
    fn <- gsub("//+", "/", fn)
    if (!file.exists(fn))
        list("ERROR: item at the specified URL not found.", "text/html", character(), 404L)
    else {
        ## if the file is an R script, run it (via FastRWeb) instead of serving the content
        if (length(grep("\\.R$", fn))) {
            ## first, figure out our hostname + port for back-references
          port <- ""
          host <- if (length(headers)) {
              h <- strsplit(rawToChar(headers), "[\n\r]+")[[1]]
              l <- strsplit(h, "[\t ]*:[ \t]*")
              names(l) <- sapply(l, function(x) tolower(x[1]))
              if (length(l[["host"]]) > 2L) port <- paste(":", l[["host"]][3L], sep='')
              l[["host"]][2L]
          } else NULL
          if (is.null(host)) host <- "localhost"
          hosturl <- paste("http://", host, port, sep='')

          ## create an env to eval in
          ## FIXME: we use our NS because historically scripts have been using
          ##        undocumented calls, but we should ween them off that
          env <- new.env(parent=environment(.http.request))
          env$path.info <- path.info
          ## source the script in env
          source(fn, env)
          if (!is.function(env[["run"]])) stop("script does not contain a run() function")
          ## run the run() function but like .httpd
          res <- env[["run"]](url, query, body, headers)
          ## we have to add no-cache since the result is dynamic
          tryCatch({ ## if anything goes wrong, just leave the result alone ...
              if (is.list(res) && length(res) > 0) {
                  if (length(res) > 2 && length(res[[3]]) && !identical(res[[3]], "")) { ## has headers
                      ## only mess with them if there is no Cache-control in sight ...
                      if (!length(grep("cache-control:", as.character(res[[3]]), TRUE)))
                          res[[3]] <- paste0(paste(res[[3]], collapse="\r\n"), "\r\nCache-control: no-cache")
                  } else { ## append our only header
                      res[[3]] <- "Cache-control: no-cache"
                  }
              }
          }, error=function(e) NULL)
          return(res)
      }

        s <- file.info(fn)$size
        f <- file(fn, "rb")
        r <- readBin(f, raw(), s)
        close(f)
        ## deduce content type by extension
        ct <- "text/html"
        ctl <- list("text/javascript"=".js",
                    "image/png"=".png",
                    "image/jpeg"=".jpg",
                    "image/jpeg"=".jpeg",
                    "image/svg+xml"=".svg",
                    "text/css"=".css")
        for (i in seq_along(ctl))
            if (length(grep(paste("\\",ctl[[i]],"$",sep=''), fn, TRUE))) {
                ct <- names(ctl)[i]
                break
            }
        list(r, ct)
    }
}
