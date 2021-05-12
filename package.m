(* ::Package:: *)

BeginPackage["mypck`"]








TestManipulate::usage="funzione per gestione clustering"


Begin["Private`"]





imgD =Import[FileNameJoin[{NotebookDirectory[],StringJoin["slide-vuota.png"]}]];
imgA =Import[FileNameJoin[{NotebookDirectory[],StringJoin["slide-vuota.png"]}]];
selectedFile=Null;
edge = False;
TestManipulate[]:=Column[{Manipulate[
If[Or[x =="KMeans",x =="KMedoids",x ==  "Spectral"],imgA=ClusteringComponents[ImageResize[imgD,430],cl,Method->x],imgA=ClusteringComponents[ImageResize[imgD,430],Method->x]]//Colorize,Row[{Item[FileNameSetter[Dynamic[selectedFile]],Alignment->Center],Button["Import",imgD=Import[selectedFile],Enabled->Dynamic[selectedFile=!=Null]]}], Delimiter,
Row[{Control[{{x, "KMeans","Method"}, {"KMeans","KMedoids","Spectral"}}],Control[{{cl, 2, "N. cluster"}, 2, 15, 1, Appearance -> "Labeled"}]}],Delimiter,
Row[{Control[{{x, "KMeans","Method"}, {"MeanShift"-> "Mean Shift","GaussianMixture"-> "Gaussian Mixture","NeighborhoodContraction"->"Neighborhood Contraction"}}]}],
Delimiter,Button["Save image",Export[FileNameJoin[{NotebookDirectory[],StringJoin["clusterImage.jpg"]}],imgA//Colorize,"JPEG"], ImageSize->Large]], 
Manipulate[
If[edge == True,imgDrop = ImageTake[ImageAdd[imgA//Colorize,EdgeDetect[imgA//Colorize]],{top,bottom},{left,right}],imgDrop = ImageTake[imgA//Colorize,{top,bottom},{left,right}]],Control[{{top, 1, "top"}, 1, ImageDimensions[imgA//Colorize][[2]]}],Control[{{bottom, ImageDimensions[imgA//Colorize][[2]], "bottom"}, ImageDimensions[imgA//Colorize][[2]],1}],Control[{{left, 1, "left"}, 1, ImageDimensions[imgA//Colorize][[1]]}],Control[{{right, ImageDimensions[imgA//Colorize][[1]], "right"}, ImageDimensions[imgA//Colorize][[1]],1}], Delimiter,Row[{Button["Apply Edge Detection",edge = True,ImageSize->Large],Button["Remove Edge Detection",edge = False, ImageSize->Large]}],Button["Save image",Export[FileNameJoin[{NotebookDirectory[],StringJoin["takeCluster.jpg"]}],imgDrop,"JPEG"], ImageSize->Large],Delimiter]}];  


End[]
