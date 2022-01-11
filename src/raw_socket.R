clientSocket <- make.socket(host="127.0.0.1", port=40516, server=FALSE)

for (i in 1:10) {
  rp <- rpois(1,10)
  Sys.sleep(1)
  msg <- "303132"
  write.socket(clientSocket, msg)
}

close.socket(clientSocket)
