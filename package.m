(* ::Package:: *)

BeginPackage["mypck`"]








TestManipulate::usage="funzione per gestione clustering"


Begin["Private`"]





TestManipulate[img_]:=Manipulate[ClusteringComponents[img,Cl,Method->x]//Colorize,{x,{"Agglomerate","KMedoids"}},{Cl,1,20,1}];








End[]
