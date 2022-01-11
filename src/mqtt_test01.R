library(rmqtt)

# BACKEND sends a message to FRONTEND
mqtt_topic_publish(topic = 'BACKEND_MSGS', 
                   message_to_send = "R-client says 'Hello front-end!")

# BACKEND receives a message from FRONTEND
mqtt_topic_subscribe(topic = "FRONTEND_MSGS",
                     num.messages = 1)
