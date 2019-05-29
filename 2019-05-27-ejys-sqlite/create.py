# Adapted from http://www.sqlitetutorial.net/sqlite-python/creating-database/
# and https://stackoverflow.com/questions/2887878/
from sqlite3 import Error
import sqlite3
import pandas as pd

from util import create_connection


def create_table(conn, create_table_sql):
    try:
        c = conn.cursor()
        c.execute(create_table_sql)
    except Error as e:
        print(e)


def create_table_xl(filepath, conn, table_name):
    df = pd.read_excel(filepath)
    df.to_sql(table_name, conn, if_exists='append', index=False)


def main():
    db = "../data/sqlite.db"
    xl = "../data/01-in/ascher-bee-data/2019-05-23-Apoidea world consensus file Sorted by name 2019.xlsx"
    table_name = 'bee_types'

    sql_create_projects_table = """ CREATE TABLE IF NOT EXISTS projects (
                                    id integer PRIMARY KEY,
                                    name text NOT NULL,
                                    begin_date text,
                                    end_date text
                                    ); """

    conn = create_connection(db)

    if conn is not None:
        # Method 1: creating a table with SQL
        create_table(conn, sql_create_projects_table)
        # Method 2: creating table and porting data with pandas
        create_table_xl(xl, conn, table_name)

    else:
        print('Error! cannot create the database connection.')

if __name__ == '__main__':
    main()
    
