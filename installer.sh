#!/bin/bash

mkdir -p $1

ocamlfind ocamlopt \
 -package ocsigenserver \
 -package ocsigenserver.ext.staticmod \
 -package ocsigenserver.ext.ocsipersist-dbm \
 -package eliom.ppx.server \
 -package eliom.ppx.type \
 -package eliom \
 -package eliom.server \
 -package eliom.syntax \
 -package eliom.syntax.common \
 -package eliom.syntax.server \
 -package stdlib \
 -package yojson \
 -package lwt.ppx \
 -package ocsigen-toolkit \
 -package js_of_ocaml-ppx_deriving_json \
 -package ppx_derivers \
 server_main.cmx -o $1/ovsdb_browser_server -linkpkg -linkall -thread


cat > $1/findlib.conf <<EOF
destdir="./lib"
path=".//lib"
ocamlc="ocamlc.opt"
ocamlopt="ocamlopt.opt"
ocamldep="ocamldep.opt"
ocamldoc="ocamldoc.opt"
EOF

cp -r local $1/
cp -r static $1/
cp mime.types $1/local/etc/ovsdb_browser/
cp ovsdb_browser_server.conf $1/
cp run.sh $1/
