# irc

This is a REST service for irc.

## Usage

- POST to "/" with params server and nick will result in a string id
  of a new bot instance, a "bid"
- DELETE to /{bid} will get rid of the bot id'ed by that bid
- GET /{bid}/channels has a body which is a clojure set of channels
  the identified bot is in
- POST /{bid}/channel/{channel} will cause the identified bot to join
  the given channel (channel will most likely need to be url encoded)
- GET /{bid}/channel/{channel} is the set of users in the channel
- DELETE /{bid}/channel/{channel} parts the channel
- GET /{bid}/events is a map of event ids to events
- GET /{bid}/event/{eid} is the event
- DELETE /{bid}/event/{eid} deletes the event
- see com.thelastcitadel.irc/irc

## Deployment

- build with `lein ring uberwar`
- run war with your favorite servlet container
- I like jetty-runner

## Types

I chose this project to play with core.typed, so it is typed more or less.

## License

Copyright © 2013 Kevin Downey

Distributed under the Eclipse Public License, the same as Clojure.
