# SimpleChat

This is a little toy Ocaml project.

It's supposed to implement a simple chat program. One half's a server, one half's a client, and they can talk to each other.

## Building Natively

### Prerequisites

I developed SimpleChat using Ocaml 4.04.2 and the following libraries:
* core
* lwt - 3.3.0
* bin_prot
* ppx_bin_prot
* ppx_deriving
* ppx_sexp_conv
* conduit-lwt-unix
* jbuilder
* ANSITerminal
* cohttp, cohttp-lwt, and cohttp-lwt-unix
* yojson

You can install them using opam:
```
$ opam switch 4.04.2
...
```

```
$ opam install core lwt.3.3.0 bin_prot ppx_deriving ppx_bin_prot ppx_sexp_conv conduit-lwt-unix jbuilder ANSITerminal cohttp cohttp-lwt cohttp-lwt-unix
...
```

### jbuilder

Just run jbuilder on the client and server programs:
```
$ jbuilder build src/sc_client.exe src/sc_server.exe
...
```

## Building with Docker

Building on live systems is never perfect, so I find docker to be a really useful alternative for deployment / sharing code with others.

From the root directory of the project:
```
$ docker build -t briancaine/ocaml-simplechat .
...
```

## Running

SimpleChat has a client/server model and it supports multiple different UI implementations.

The basic usage for the client is:
```
$ ./_build/default/src/sc_client.exe UI_MODULE IP_ADDRESS PORT
```

The basic usage for the server is:
```
$ ./_build/default/src/sc_client.exe UI_MODULE PORT
```

The two supported UI modules are:
* text
* web

I started writing a fancier text interface using ANSITerminal (and the remnants are still in the repo), but it turned out to be much harder than I expected. I was kind of hoping for a "printf + colors" setup, but it's much more complicated than that.

So instead I wrote a web UI that works decently enough.

If you're using docker, the commands are the same, but you should prefix them with:
```
$ docker run --rm -ti --net=host briancaine/ocaml-simplechat *rest of command as above*
```

Docker can be a little strange with how passing signals through to the process goes (ie ctrl+c might not send the interrupt as expected), so you might need to manually kill the docker sandboxed process (`docker ps` and then `docker kill -f *container-name*`) to get it to quit.

It's on my todo list of things to fix.

## Design

### UI

I created a module type ([`SimpleChat.UI_type`](https://github.com/briancaine/ocaml-simplechat/blob/4915eaca5b938aef8ed0c8c73190118bfac73c1f/src/SimpleChat.ml#L8)) to represent a module implementing a UI.

It has three operations:
* signalling that we're starting a server
* signalling that we're attempting to connect to a server
* handling the connection (same function for server and client)

Each UI implementation registers a module with `SimpleChat.add_ui_type NAME MODULE`.

Currently the implementations are manually registered in the main executables (sc_client, sc_server), but in the future, someone could add some sort of dynamic linking / plugin mechanism.

### Protocol

The protocol abstracts a pair of unidirectional event streams.

```ocaml
module Event = struct

  type t =
    | Message             of Message.t
    | MessageConfirmation of MessageConfirmation.t
    | ConnectionClosed
    | ConnectionWarning   of string
    | ConnectionError     of string [@@deriving sexp, bin_io]

...

end
```

The main `UI_type.run` function will receive a stream of events from the partner, and can also send events of its own.

* `ConnectionClosed` is sent when the other user intentionally ends the chat, for whatever reason.
* `ConnectionError` is sent when the connection unexpectedly terminates, whether on the local side or the remote side.

  Both of ^ those events will terminate the chat.

* `ConnectionWarning` is currently only sent when a client confirms a nonexistent (or already confirmed) message. But it could be used for more general warnings in the future (from either side).

* `Message` and `MessageConfirmation` are probably self explanatory.

## Afterthoughts

### Wire Protocol

I think I'm pretty happy with my wire protocol. I used bin_io to serialize the structures.

The code that turns an input channel into a stream could be improved, because it allocates several new buffers each cycle. Reusing a single buffer each read would be better.

### UI

In the past, I tried to use Core's command line parsing library. It's powerful, but also very complicated.

I tried to get by without using it, and while my code works, I think I'd want to incorporate command line parsing into the UI module type.

Also, the UI module type lays out how the actual chatting happens very clearly. But it doesn't cover setting up and tearing down chat sessions all that well.

In the future, I'd pay more attention to that.
