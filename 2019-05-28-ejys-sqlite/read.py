# Based on https://pythonspot.com/sqlite-database-with-pandas/
import sqlite3
import pandas as pd

from util import create_connection

if __name__ == "__main__":
    conn = create_connection('../data/sqlite.db')
    query = "SELECT genus, species FROM bee_types WHERE genus = 'Megachile';"

    df = pd.read_sql_query(query, conn)
    print(df)