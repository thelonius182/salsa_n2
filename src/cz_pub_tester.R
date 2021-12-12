library(redux)

salsa_redis_server <- hiredis()
salsa_redis_server <- hiredis(url = 'redis-16417.c135.eu-central-1-1.ec2.cloud.redislabs.com',
                              port = 16417,
                              password = 'qfsnc0qTE0CR2YVwdJu4YypqnRM1F5E5')

salsa_redis_server$PUBLISH("nipq_tst01", "goodbye")

transform <- function(x) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
          ": got a message: ",
          x$value)
  x$value
}

res <- salsa_redis_server$subscribe(
  "nipq_tst01",
  transform = transform,
  terminate = function(x)
    identical(x, "goodbye"),
  n = 1
)


