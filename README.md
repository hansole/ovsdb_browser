# OVSDB browser

This is an application that lets you browse the content of
[OVSDB](http://docs.openvswitch.org/en/latest/ref/ovsdb.7/) databases.
The content is shown in a Web GUI.

The tools coming with OVSDB provides the same functionality, and much
more. However, I find the usage of these tools can be a bit
complicated and the output a bit "overwhelming" for some of the tasks
I'm doing.

I was not able to find any tools that made it easy browsing the
databases. So that's the goal of this application.

## Features

The set of features it supports is based on my needs. So you will
probably find that it is missing features.

It has to main way to access the data. "Browse" the tables or
"monitor" the whole database.

When browsing, you can browse or monitor individual tables. If you
browse, the you have the possibility to start and stop monitoring of
individual columns in the table. If you monitor the table instead, you
will monitor all columns in the table.

The monitor of the whole database was added as a quick way to figure
out where "sh.t happens". It shows all updates and in addition reports
the notifications per second for each table. So it should be quick to
drill down and figure out what gets updated frequently.

It is possible to do modification of simple values, e.g string or
integers that has no dependencies.

It show a simple dependency between the tables.

![GUI](/ovsdb_browser_gui.png)


## Build and run

The application is written in [OCaml](https://ocaml.org/) using the
[Ocsigen](https://ocsigen.org/) framework. It has only been tested
under Ubuntu 16.04, but it will probably run on other Linux
distributions that has proper OCaml support.

It is easiest to build and install using
[OPAM](https://opam.ocaml.org/).

Install the packages 'oscigen-start' and 'yojson'. I think that should
install all the dependencies. (Version 3.2.0 of 'js\_of\_ocaml' has a
bug, so downgrade the js\_of\_ocaml* packages to version 3.1.0.)

'make test.opt' will build the server and connect to the OVSDB server
at '127.0.0.1:6640'. You can change the DB host with "OVSDB\_HOST" and
port with "OVSDB\_PORT" environment variables before starting the
server. A better option might be to use the default ports and use a
ssh-tunnel to the DB server.

To use it, point your browser to http://127.0.0.1:8080

## Deploy

If you want to run it on another node than where you have built it,
you can use the script:

`installer.sh <target_dir>`

This will create an executable and the dependencies and put it in the
target folder. You should be able to copy this to other nodes and run
it there.

(I was not able to figure out how to make the whole server as a single
executable, as
[reported](https://sympa.inria.fr/sympa/arc/ocsigen/2018-06/msg00000.html)
on the mailing list for the framework. So the actually application is
still loaded dynamically into the web-server. If I find a solution,
that will be fixed at some time.)

## Issues

This application is work in progress, so it still have several bugs
and issues. Here is an incomplete list of known issues:

* Notifications between the backend and the frontend leak memory. So
  after some (quite long) time, it will run out of resources and stop.
  I'm not sure if I'm doing something wrong, or it is a bug in the
  framework. Have
  [reported](https://github.com/ocsigen/eliom/issues/569) this to the
  framework developers.

* Don't handle lost connection to the database server well.

* Table header in _monitor_ mode should not have pop-up.

* The pop-up should be a pop-up, not "under".

* When selecting "browse" or "monitor", the other option should be
  disabled until current selection is un-selected.

* GUI don't indicate lost connection to backend.

* In some cases it might be issues with backend state if page is
  reloaded.

## Improvements

Here is a un-ordered (incomplete) list of things that might be
improved in the not so distant future.

* Improve GUI with graphic elements

* Position of elements should not move around when other elements are
  updated. Need better control of layout.

* Feedback to user on failed updates

* Delete rows, including rows that has dependencies to other tables.

* Insert/clone rows, with help also inserting in dependent tables.

