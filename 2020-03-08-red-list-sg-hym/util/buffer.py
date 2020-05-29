import ogr, os, datetime as dt

os.chdir("C:/data/")

def createBuffer(inputfn, outputBufferfn, bufferDist, i, max_feature, batch_size):
    print("MAX FEATURE ", max_feature, "/", "BATCH SIZE ", batch_size)
    inputds = ogr.Open(inputfn)
    inputlyr = inputds.GetLayer()
    shpdriver = ogr.GetDriverByName('GPKG')
    if os.path.exists(outputBufferfn):
        shpdriver.DeleteDataSource(outputBufferfn)
    outputBufferds = shpdriver.CreateDataSource(outputBufferfn)
    bufferlyr = outputBufferds.CreateLayer(outputBufferfn, geom_type=ogr.wkbPolygon)
    featureDefn = bufferlyr.GetLayerDefn()
    counter_all = 0
    counter_written = 0
    for feature in inputlyr:
        while (counter_written < batch_size):
            counter_all += 1
            if(counter_all > (max_feature-batch_size)):
                counter_written += 1
                ingeom = feature.GetGeometryRef()
                geomBuffer = ingeom.Buffer(bufferDist)
                outFeature = ogr.Feature(featureDefn)
                outFeature.SetGeometry(geomBuffer)
                bufferlyr.CreateFeature(outFeature)
                outFeature = None
                print(dt.datetime.now(), " -- Feature ", counter_written, counter_all)


def main(inputfn, outputBufferfn, bufferDist):
    n_files = 10
    n_features = 1300
    batch_size = int(n_features/n_files)
    for i in range(1, (n_files+1)): 
        max_feature = i * batch_size
        outputfn = outputBufferfn + "-" + str(i) + ".gpkg"
        createBuffer(inputfn, outputfn, bufferDist, i, max_feature, batch_size)

inputfn="Figure1_reclass-green-pol-fixed-filtered-noholes-svy21.gpkg"
outputBufferfn="Figure1_reclass-green-pol-fixed-filtered-noholes-svy21-buf20m"
bufferDist=20

main(inputfn, outputBufferfn, bufferDist)

