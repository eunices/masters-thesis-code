print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("WELCOME TO CLEANING SCRIPTS FOR BEE TYPE DATA")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# Scripts
#############
start <- proc.time()
source('2019-06-19-ascher-type-data/init.r')
source('2019-06-19-ascher-type-data/clean.r')
source('2019-06-19-ascher-type-data/df1.r')
source('2019-06-19-ascher-type-data/df2.r')
source('2019-06-19-ascher-type-data/df3.r')
source('2019-06-19-ascher-type-data/df4.r')
print(proc.time()-start)
