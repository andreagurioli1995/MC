(* ::Package:: *)

BeginPackage["mypck`"](*inizializzazione package*)








TestManipulate::usage="funzione per gestione clustering"(*definizione funzione testmanipulate*)


Begin["Private`"](*inizio sezione private del package*)





imgD = Import[FileNameJoin[{NotebookDirectory[], StringJoin["slide-vuota.png"]}]];
(*definizione immagine di partenza*)
imgA = Import[FileNameJoin[{NotebookDirectory[], StringJoin["slide-vuota.png"]}]];(*definizione immagine di partenza*)
selectedFile = Null; (*Inizializzazione file selezionato a null*)
edge = False;(*inizializzazione edge detection in takeimage a false *)
TestManipulate[] :=
    Column[{Manipulate[
        If[Or[x == "KMeans", x == "KMedoids", x == "Spectral"](*if sulla variabile x per il metodo selezionato all'interno della manipulate*),
            imgA = ClusteringComponents[ImageResize[imgD, 430], cl, Method -> x](*se true, utilizzo dei metodi con numero di cluster definito da "cl"*)
            ,
            imgA = ClusteringComponents[ImageResize[imgD, 430], Method -> x](*se false, il numero di cluster non \[EGrave] richiesto, verr\[AGrave] quindi omesso*)
        ] // Colorize(*colorize utilizzato per parsing immagine da formato vettoriale ad immagine *), Row[{Item[FileNameSetter[Dynamic[selectedFile],"Open",{"Immagini"->{"*.jpg","*.png","*.jpeg"}}], Alignment -> Center](*selezione dinamica dell'immagine scelta con controllo formato file, mostra browse*), Button["Import", imgD = Import[selectedFile](*import del file selezionato precedentemente*), Enabled -> Dynamic[selectedFile =!= Null](*abilita il tasto imprt solo quando viene selezionato un file*)]}](*row viene utilizzata per organizzare il tool, inserendo la parte interessata in un unica riga*), Delimiter(*linea grafica di separazione*),
        Row[{Control[{{x, "KMeans", "Method"}, {"KMeans", "KMedoids", "Spectral"}}](*selezione del metodo di clustering, metodo default Kmeans*), Control[{{cl, 2, "N. cluster"}, 2, 15, 1, Appearance -> "Labeled"}]}(*selezione numero di cluster da applicare ad algoritmo*)], Delimiter(*linea grafica di separazione*),
        Row[{Control[{{x, "KMeans", "Method"}, {"MeanShift" -> "Mean Shift", "GaussianMixture" -> "Gaussian Mixture", "NeighborhoodContraction" -> "Neighborhood Contraction"}(*selezione metodo di clustering senza numero di cluster preselezionato*)}](*selezione metodo clustering senza numero di cluster predefinito*)}],
        Delimiter(*linea di delimitazione grafica*), Button["Save image", Export[FileNameJoin[{NotebookDirectory[], StringJoin["clusterImage.jpg"]}], imgA // Colorize, "JPEG"], ImageSize -> Large](*salvataggio immagine clusterizzata, all'interno della notebook directory*)
    ](*manipulate relativa a clustering*),
    Manipulate[
        If[edge == True,
            imgDrop = ImageTake[ImageAdd[imgA // Colorize, EdgeDetect[imgA // Colorize]](*l'edge detection viene apploicata sommando il risultato all'immagine clusterizzata precedentemente*), {top, bottom}, {left, right}](*con edge detection selezionata, viene applicata image take che va a partizionare l'immagine attraverso i valori definiti dalla manipulate, verr\[AGrave] poi applicata edge detection su immagine partizionata*)
            ,
            imgDrop = ImageTake[imgA // Colorize, {top, bottom}, {left, right}](*casistica in cui edge detection non \[EGrave] selezionato, viene applicato solo partizionamento dell'immagine*)
        ], Control[{{top, 1, "top"}, 1, ImageDimensions[imgA // Colorize][[2]]}], Control[{{bottom, ImageDimensions[imgA // Colorize][[2]], "bottom"}, ImageDimensions[imgA // Colorize][[2]], 1}], Control[{{left, 1, "left"}, 1, ImageDimensions[imgA // Colorize][[1]]}], Control[{{right, ImageDimensions[imgA // Colorize][[1]], "right"}, ImageDimensions[imgA // Colorize][[1]], 1}(*selezione valori take image*)], Delimiter(*linea di delimitazione grafica*), Row[{Button["Apply Edge Detection", edge = True, ImageSize -> Large](*applicazione edge detectione, settando variabile a true*), Button["Remove Edge Detection", edge = False, ImageSize -> Large](*rimozione edge detection, settando edge a false*)}], Button["Save image", Export[FileNameJoin[{NotebookDirectory[], StringJoin["takeCluster.jpg"]}], imgDrop, "JPEG"], ImageSize -> Large](*salvataggio immagine elaborata su notebook directory*), Delimiter(*linea di delimitazione grafica*)
    ]}](*manipulate relativa ad edge detection e partizionamento immagine*);


End[](*termina Private*)
EndPackage[](*termina Package*)
