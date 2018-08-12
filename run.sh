#!/bin/bash

export OCAMLFIND_CONF=./findlib.conf
./ovsdb_browser_server -c ovsdb_browser_server.conf
