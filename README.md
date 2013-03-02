## Database setup

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser perse -P'
    $ sudo su postgres --command 'createdb perse -O perse'

Update the database to the latest migration:

    $ dist/build/perse/perse perse.conf --create-version
