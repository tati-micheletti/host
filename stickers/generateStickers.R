
# It is possible to locally load the table.
moduleTable <- data.table::fread(file.path(getwd(), "stickers/moduleTable.csv"))

#If the table is null on the moduleStiker call,
# it will download automatically using reproducible::prepInputs.

moduleSticker(moduleName = "LandR.CS", moduleTable = moduleTable)
moduleSticker(moduleName = "fireSense", moduleTable = moduleTable)
moduleSticker(moduleName = "SCFM", moduleTable = moduleTable)
moduleSticker(moduleName = "fireSense + SCFM", moduleTable = moduleTable)
moduleSticker(moduleName = "birds", moduleTable = moduleTable)
moduleSticker(moduleName = "birds.CS", moduleTable = moduleTable)
moduleSticker(moduleName = "caribouRSF", moduleTable = moduleTable, savedSticker = "caribouRSF")
moduleSticker(moduleName = "caribouTEK", moduleTable = moduleTable, savedSticker = "caribouTEK")
moduleSticker(moduleName = "caribouLambda", moduleTable = moduleTable, savedSticker = "caribouLambda")
moduleSticker(moduleName = "carbon", moduleTable = moduleTable)
moduleSticker(moduleName = "climate", moduleTable = moduleTable)
moduleSticker(moduleName = "trends", moduleTable = moduleTable)
moduleSticker(moduleName = "MPB", moduleTable = moduleTable)
moduleSticker(moduleName = "hotspot", moduleTable = moduleTable)
moduleSticker(moduleName = "harvest", moduleTable = moduleTable)
moduleSticker(moduleName = "diversity", moduleTable = moduleTable)
