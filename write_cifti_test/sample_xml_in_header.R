# Hard coded writing of xml file that corresponds to MSC-01 parcellation
# myc 20190814

library(XML)

cml <- newXMLNode("CIFTI", attrs = c(Version="2"))
newXMLNode("Matrix", parent=cml)
newXMLNode("MatrixIndicesMap", attrs = c(AppliesToMatrixDimension="0",
                                         IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES",
                                         NumberOfSeriesPoints="1",
                                         SeriesExponent="0",
                                         SeriesStart="0",
                                         SeriesStep="0",
                                         SeriesUnit="SECOND"), parent=cml[["Matrix"]])

newXMLNode("MatrixIndicesMap", attrs = c(AppliesToMatrixDimension="1",
                  IndicesMapToDataType="CIFTI_INDEX_TYPE_BRAIN_MODELS"), parent=cml[["Matrix"]])

newXMLNode("BrainModel", attrs = c(IndexOffset="0",
                                  IndexCount="29696",
                                  ModelType="CIFTI_MODEL_TYPE_SURFACE",
                                  BrainStructure="CIFTI_STRUCTURE_CORTEX_LEFT",
                                  SurfaceNumberOfVertices="32492"), parent=cml[["Matrix"]][[2]])
newXMLNode("VertexIndices", parent=cml[["Matrix"]][[2]][[1]])

xmlValue(cml[["Matrix"]][[2]][[1]][[1]]) <- paste(as.integer(cii_fj1$BrainMode[[1]]), "")

# Right Hemipshere?
newXMLNode("MatrixIndicesMap", attrs = c(AppliesToMatrixDimension="0",
                                         IndicesMapToDataType="CIFTI_INDEX_TYPE_SERIES",
                                         NumberOfSeriesPoints="1",
                                         SeriesExponent="0",
                                         SeriesStart="0",
                                         SeriesStep="0",
                                         SeriesUnit="SECOND"), parent=cml[["Matrix"]])

newXMLNode("BrainModel", attrs = c(IndexOffset="29696",
                                   IndexCount="29716",
                                   ModelType="CIFTI_MODEL_TYPE_SURFACE",
                                   BrainStructure="CIFTI_STRUCTURE_CORTEX_RIGHT",
                                   SurfaceNumberOfVertices="32492"), parent=cml[["Matrix"]][[2]])
newXMLNode("VertexIndices", parent=cml[["Matrix"]][[2]][[2]])

xmlValue(cml[["Matrix"]][[2]][[2]][[1]]) <- paste(as.integer(cii_fj1$BrainMode[[2]]), "")