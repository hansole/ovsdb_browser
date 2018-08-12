#!/bin/bash
ovsdb-server ./simple.db --no-chdir --pidfile=/tmp/pid-7 --log-file=/tmp/db-7.log --verbose=ANY:ANY:file:warn --remote=punix:/tmp/db-7.sock --unixctl=/tmp/ctl-7.sock --remote=ptcp:6640:0.0.0.0

#ovsdb-tool create simple.db ovsdb_browser/tests/schema/simple.schema
