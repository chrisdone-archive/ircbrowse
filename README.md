# ircbrowse

IRC Browse is a web service for browsing IRC logs. The [IRC Browse site is here](http://ircbrowse.net/).

## Building & running

Build:

   $ stack install

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser ircbrowse -P'
    $ sudo su postgres --command 'createdb ircbrowse -O ircbrowse'

Update the database to the latest migration:

    $ stack exec -- ircbrowse ircbrowse.conf --create-version

Run:

   $ stack exec -- ircbrowse ircbrowse.conf

## Adding a channel

* Add to VNC's join list
* Add to Ircbrowse.TypesImport
* Add to Ircbrowse.Import's importRecent
* Insert an entry into entry_count
