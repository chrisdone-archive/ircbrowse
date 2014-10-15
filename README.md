## About

IRC Browse is a web service for browsing IRC logs. The [IRC Browse site is here](http://ircbrowse.net/).

## Database setup

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser ircbrowse -P'
    $ sudo su postgres --command 'createdb ircbrowse -O ircbrowse'

Update the database to the latest migration:

    $ dist/build/ircbrowse/ircbrowse ircbrowse.conf --create-version

## Stackage version

    remote-repo: stackage:http://www.stackage.org/stackage/805701d44dd044b9d44d3021826344cfe67ab411

## Adding a channel

* Add to VNC's join list
* Add to Ircbrowse.TypesImport
* Add to Ircbrowse.Import's importRecent
* Insert an entry into entry_count
