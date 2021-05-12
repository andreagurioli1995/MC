(* ::Package:: *)

BeginPackage["mypck`"]








TestManipulate::usage="funzione per gestione clustering"


Begin["Private`"]





imgD =Image[CompressedData["
1:eJztyjEKwkAQBdAlNqnUK3iLtJa2EQ+Q4CakWWETEK/oqRzvkPI9+MPMZy7j
q5+alNLaxuiH97XW4XM/x/Eo6zKX/LyVLc+5duMhymP8niL//QsAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA7OQHCTDfyQ==
"], "Byte", ColorSpace -> "RGB", Interleaving -> True, MetaInformation -> Association["Exif" -> Association["Orientation" -> Association["CameraTopOrientation" -> Top, "Mirrored" -> False], "Software" -> "Shotwell 0.30.10", "ExifTag" -> 68, "PixelXDimension" -> 527, "PixelYDimension" -> 526], "XMP" -> Association["XMPBasicSchema" -> Association["CreatorTool" -> "Adobe Photoshop CS6 (Macintosh)"], "XMPMediaManagementSchema" -> Association["InstanceID" -> "xmp.iid:E62194301E5D11E5941BFB3BD6212510", "DocumentID" -> "xmp.did:E62194311E5D11E5941BFB3BD6212510", "DerivedFrom" -> Association["Reference" -> Association["InstanceID" -> "xmp.iid:E621942E1E5D11E5941BFB3BD6212510", "DocumentID" -> "xmp.did:E621942F1E5D11E5941BFB3BD6212510"]]]]]];
selectedFile=Null;
edge = False;
TestManipulate[]:=Column[{Manipulate[
If[Or[x =="KMeans",x =="KMedoids",x ==  "Spectral"],imgA=ClusteringComponents[ImageResize[imgD,430],cl,Method->x],imgA=ClusteringComponents[ImageResize[imgD,430],Method->x]]//Colorize,Row[{Item[FileNameSetter[Dynamic[selectedFile]],Alignment->Center],Button["Import",imgD=Import[selectedFile],Enabled->Dynamic[selectedFile=!=Null]]}], Delimiter,
Row[{Control[{{x, "KMeans","Method"}, {"KMeans","KMedoids","Spectral"}}],Control[{{cl, 2, "N. cluster"}, 2, 15, 1, Appearance -> "Labeled"}]}],Delimiter,
Row[{Control[{{x, "KMeans","Method"}, {"MeanShift"-> "Mean Shift","GaussianMixture"-> "Gaussian Mixture","NeighborhoodContraction"->"Neighborhood Contraction"}}]}],
Delimiter,Button["Save image",Export[FileNameJoin[{NotebookDirectory[],StringJoin["clusterImage.jpg"]}],imgA//Colorize,"JPEG"], ImageSize->Large]],
Manipulate[
If[edge == True,imgDrop = ImageTake[ImageAdd[imgA//Colorize,EdgeDetect[imgA//Colorize]],{top,bottom},{left,right}],imgDrop = ImageTake[imgA//Colorize,{top,bottom},{left,right}]],{top,1,ImageDimensions[imgA//Colorize][[2]]},{bottom,ImageDimensions[imgA//Colorize][[2]],1},{left,1,ImageDimensions[imgA//Colorize][[1]]},{right,ImageDimensions[imgA//Colorize][[1]],1}, Delimiter,Row[{Button["Apply Edge Detection",edge = True,ImageSize->Large],Button["Remove Edge Detection",edge = False, ImageSize->Large]}],Button["Save image",Export[FileNameJoin[{NotebookDirectory[],StringJoin["takeCluster.jpg"]}],imgDrop,"JPEG"], ImageSize->Large],Delimiter]}];  


End[]
