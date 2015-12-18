# ircbrowse

IRC Browse is a web service for browsing IRC logs. The
[IRC Browse site is here](http://ircbrowse.net/).

## Adding a channel

Add to
[`Ircbrowse.Types.Import`](https://github.com/chrisdone/ircbrowse/edit/master/src/Ircbrowse/Types/Import.hs)
and open a pull request.

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
