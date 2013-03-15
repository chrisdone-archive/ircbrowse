## About

IRC Browse is a web service for browsing IRC logs. The [IRC Browse site is here](http://ircbrowse.net/).

## Database setup

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser ircbrowse -P'
    $ sudo su postgres --command 'createdb ircbrowse -O ircbrowse'

Update the database to the latest migration:

    $ dist/build/ircbrowse/ircbrowse ircbrowse.conf --create-version
