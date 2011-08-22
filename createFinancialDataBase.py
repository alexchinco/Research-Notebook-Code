#! /usr/bin/env python
import os
import csv
import string
import random
import numpy as np
from datetime import datetime
from pymongo import *




class CreateDataBase:
    """
    This class connects to a Mongo DB financial database that I created and then populates 2 different collections containing information on monthly CRSP and annual COMPUSTAT data held in CSV files that I downloaded from the WRDS web interface.

    @author Alex Chinco
    @date 08/21/11
    @version 1.0
    """


    def __init__(self):

        self.CRSP_FILE_NAME      = 'crsp_monthly_data.csv'
        self.COMPUSTAT_FILE_NAME = 'compustat_annual_data.csv'

        self.CRSP_VALUE_ERROR_FILE_NAME      = 'crsp_value_errors.csv'
        self.COMPUSTAT_VALUE_ERROR_FILE_NAME = 'compustat_value_errors.csv'

        self.BASE_YEAR = 1950

        self.fillCrspDB()
        self.fillCompustatDB()



    def formatCrspData(self, row):
        """
        This method accepts a single row observation from the monthly CRSP data and formats it to be uploaded into a Mongo DB collection.
        """
        
        cusip        = row[1]
        ticker       = row[2]
        permno       = row[3]
        shares       = int(row[8])
        price        = float(row[9])
        volume       = int(row[10])
        cumDivReturn = float(row[11])
        exDivReturn  = float(row[12])
        
        year  = int(np.floor(int(row[0]) / 10000))
        month = int(np.floor(int(row[0]) / 100 - year * 100))
        date  = 12 * (year - self.BASE_YEAR) + (month - 1)

        sic4 = int(row[5])
        sic3 = int(np.floor(sic4/10))
        sic2 = int(np.floor(sic4/100))
        
        if (row[4] == 1):
            exchange = "nyse"
        elif (row[4] == 2):
            exchange = "amex"
        elif (row[4] == 3):
            exchange = "nasdaq"
        else:
            exchange = "other"
            
        if (row[6] == "A"):
            status = "active"
        elif (row[6] == "H"):
            status = "halted"
        elif (row[6] == "S"):
            status = "suspended"
        else:
            status = "unknown"
        
        obs = {"date":date,
               "year":year,
               "month":month,
               "cusip":cusip,
               "ticker":ticker,
               "permno":permno,
               "sic4":sic4,
               "sic3":sic3,
               "sic2":sic2,
               "shares":shares,
               "price":price,
               "volume":volume,
               "exchange":exchange,
               "status":status,
               "cumDivReturn":cumDivReturn,
               "exDivReturn":exDivReturn
               }

        return obs



    def formatCompustatData(self, row):
        """
        This method accepts a single row observation from the annual COMPUSTAT data and formats it to be uploaded into a Mongo DB collection.
        """
        
        gvkey      = row[0]
        permno     = row[1]
        fiscalYear = int(row[5])
        ticker     = row[10]
        cusip      = row[11]
        assets     = float(row[14])
        bookValue  = float(row[15])
        equity     = float(row[16])
        cash       = float(row[17])
        ebit       = float(row[18])
        employees  = float(row[19])
        sales      = float(row[20])
        
        year  = int(np.floor(int(row[4]) / 10000))
        month = int(np.floor(int(row[4]) / 100 - year * 100))
        date  = 12 * (year - self.BASE_YEAR) + (month - 1)

        sic4 = int(row[25])
        sic3 = int(np.floor(sic4/10))
        sic2 = int(np.floor(sic4/100))
        
        obs = {"gvkey":gvkey,
               "permno":permno,
               "date":date,
               "year":year,
               "month":month,
               "fiscalYear":fiscalYear,
               "ticker":ticker,
               "cusip":cusip,
               "assets":assets,
               "bookValue":bookValue,
               "equity":equity,
               "cash":cash,
               "ebit":ebit,
               "employees":employees,
               "sales":sales,
               "sic4":sic4,
               "sic3":sic3,
               "sic2":sic2
               }
        
        return obs



    def fillCrspDB(self):
        """
        This method opens up a connection to the Mongo DB collection containing information from CRSP, and then populates this collection using data from a CSV file containing monthly data from 1950 to 2010.
        """
        
        reader = csv.reader(open(self.CRSP_FILE_NAME, 'rb'), delimiter = ",")
        writer = csv.writer(open(self.CRSP_VALUE_ERROR_FILE_NAME, 'wb'), delimiter = ",")
        count = 0

        mongo = Connection()
        print mongo.server_info()
        print mongo.database_names()
        db    = mongo.financial.crsp

        for row in reader:            
            if (count == 0):
                writer.writerow(row)
            else:
                try:
                    obs = self.formatCrspData(row)
                    db.insert(obs)
                except ValueError:
                    writer.writerow(row)
            count = count + 1

        mongo.disconnect()

        print "CRSP DataBase Complete!"
        
        


    def fillCompustatDB(self):
        """
        This method opens up a connection to the Mongo DB collection containing information from COMPUSTAT, and then populates this collection using data from a CSV file containing annual COMPUSTAT data from 1950 to 2010.
        """

        reader = csv.reader(open(self.COMPUSTAT_FILE_NAME, 'rb'), delimiter = ",")
        writer = csv.writer(open(self.COMPUSTAT_VALUE_ERROR_FILE_NAME, 'wb'), delimiter = ",")
        count  = 0

        mongo = Connection()
        print mongo.server_info()
        print mongo.database_names()
        db    = mongo.financial.compustat

        for row in reader:            
            if (count == 0):
                writer.writerow(row)
            else:
                try:
                    obs = self.formatCompustatData(row)
                    db.insert(obs)
                except ValueError:
                    writer.writerow(row)
            count = count + 1

        mongo.disconnect()

        print "COMPUSTAT DataBase Complete!"







if __name__ == "__main__":

    CreateDataBase()


