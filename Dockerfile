FROM ocaml/opam

RUN sudo apt-get -y update

RUN opam repository set-url default https://opam.ocaml.org && \
  opam switch -y 4.04.2

RUN opam install -y core lwt bin_prot ppx_deriving ppx_bin_prot ppx_sexp_conv conduit-lwt-unix jbuilder
RUN opam install -y utop

RUN opam install -y lwt.3.3.0 # apparently 3.0.0 wasn't preprocessing properly?

COPY ./src ./src

# sometimes stray build stuff gets dragged in, but dragged in as root
# so we should be able to overwrite them if necessary
RUN sudo chmod -R --quiet 777 src

RUN opam config exec -- jbuilder build src/sc_server.exe src/sc_client.exe

