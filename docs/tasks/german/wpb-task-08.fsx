(**
---
title: Übung 08
category: Übungen(deutsch)
categoryindex: 2
index: 9
---
*)

(**

[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/csbiology/BIO-BTE-12-V-4/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

# Task08 - Descriptive statistics in FSharp

## Streuungsmaße mit FSharp.Stats

Zunächst werden einige Bibliotheken geladen:

Hilfe und Dokumentation der Funktionen von Plotly.NET finden Sie hier: https://plotly.github.io/Plotly.NET/
Hilfe und Dokumentation der Funktionen von FSharp.Stats finden Sie hier: https://fslab.org/FSharp.Stats/

*)

(******)
#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: Plotly.NET, 2.0.0-beta8"
#r "nuget: Plotly.NET.Interactive, 2.0.0-beta8"

//Hier werden Plotly.NET und FSharp.Stats geöffnet.
open Plotly.NET
open Plotly.NET.StyleParam
open FSharp.Stats

(**
### Task 1.1

Erstellen Sie eine Normalverteilung, welche 3.50 als Mittelwert und 1.20 als Standardabweichung hat.
Binden Sie diese Verteilung an den Namen `myDist`.

*)
(******)
open FSharp.Stats.Distributions


(**
### Task 1.2

Greifen Sie auf das Feld `Variance` der zuvor erstellten Verteilung zu. Ziehen Sie ausserdem ein zufaelligen Wert aus der Verteilung.

*)
(******)

(**
### Task 1.3

Unten sehen Sie eine Funktion, die eine Stichprobe aus einer Verteilung entnimmt. Verstehen Sie die Funktion und erstellen Sie eine Stichprobe aus `myDist` mit n=10. Binden Sie das Ergebnis an `mySample`.

*)
(******)

let getSample (dist:Distributions.Distribution<float,float>) n = 
    let distribution = dist
    Array.init n (fun _ -> distribution.Sample())

(**
### Task 1.4

Erstellen Sie eine Funktion, die einen Mittelwert mean (float), eine Standardabweichung stabw (float) und eine Stichprobengroesse n (int) als Parameter bekommt. 
Aus mean und stabw soll eine Normalverteilung erstellt werden. Die Funktion soll ein float array der Laenge n als Stichprobe aus der Verteilung zurueckgeben.
Als Vorlage kann `getSample` aus 1.3 dienen. Binden Sie die Funktion an den Namen `getSampleOfGaussWith`.

*)
(******)


(**
### Task 1.5

Bestimmen Sie den Mittelwert und die Standardabweichung von mySample. 

*)
(******)


(**
### Task 1.6

Unten sehen Sie eine Funktion, die aus einer Stichprobe den Standardfehler berechnet. 

  - a) Erstellen Sie eine Liste mit dem Namen 'semsOfSample'. Die Liste soll aus 100 Standardfehlern bestehen, die auf Stichproben folgender Verteilung beruhen: 

    - mean = 6.0

    - stabw = 1.5

    - Stichprobengroesse: jeweils 12

  - b) Erstellen Sie eine Liste mit dem Namen 'stDevsOfSample', die aus Stichproben der gleichen Verteilung jeweils die Standardabweichung berechnet.

*)
(******)

let sem (sample:float []) =
    let sd = Seq.stDev sample
    let n = float (Seq.length sample)
    sd / sqrt n



(**
### Task 1.7 (optional)

Visualisieren Sie die beiden Listen mit einem beliebigen Diagramm. Vergleichen Sie die Werte mit der Standardabweichung der originalen Verteilung (ein Satz).

*)
(******)

(**
## T-Test mit FSharp.Stats

### Task 2.1 

Erstellen Sie jeweils eine Stichprobe mit Stichprobengroöße n=10 aus Verteilungen mit folgenden Parametern

  - a) mean=12., stabw=1.4
  
  - b) mean=14., stabw=1.4

Binden Sie die Stichproben an die Namen 'sampleA' und 'sampleB'. Oft wird in FSharp.Stats mit Vektoren gerechnet. Dies ist auch beim T-Test der Fall. Die Transformation
von Sequenzen, Listen oder Arrays in Vektoren erfolgt mit: 'Vector.ofSeq [Sequenz]' oder 'vector [Sequenz]' 

*)
(******)



(**
### Task 2.2

Wenden Sie einen Zwei-Stichproben T-Test auf beide Samples an. Nehmen Sie gleiche Varianzen der beiden Grundverteilungen an.

*)
(******)

(**
### Task 2.3

Unten sehen Sie eine Funktion deren Funktionskoerper fehlt. Innerhalb der Funktion sollen zwei Stichproben
mit Stichprobengroesse = n aus Verteilungen mit mean1, mean2 und jeweils gleicher stabw erstellt werden.
Nehmen Sie gleiche Varianzen der beiden Grundverteilungen an.
Auf die im Funktionskoerper erstellten Stichproben soll ein TTest angewandt werden. Der p Value des Tests
soll Rueckgabewert der Funktion sein.

*)
(******)

let getPValueOfTTest mean1 mean2 stabw1 stabw2 equal n =
    1. //replace me

(**
### Task 2.4

In den untenstehenden Funktion werden 1000 Ttests mit den gegebenen Parametern gemacht und die p values als Histogramm geplottet.
Erstellen Sie Abbildungen mit 

  - a) gleichen means mit grosser Stichprobengroesse

  - b) gleichen means mit kleiner Stichprobengroesse

  - c) unterschiedlichen means

Sehen Sie sich das Verhalten des p Values an. Wie viele p Values sind jeweils signifikant?

  - a)

  - b)

  - c)

*)
(******)

let plotPValue mean1 mean2 stabw1 stabw2 equal n =
    let pValues = Array.init 1000000 (fun i -> 
        if i%100000=0 then printfn "%i" i
        getPValueOfTTest mean1 mean2 stabw1 stabw2 equal n)
    let pVals =
        Chart.Histogram(pValues,Xbins=Bins.init(0.,1.,0.05),HistNorm=HistNorm.Probability)
        |> Chart.withX_AxisStyle ("p value",MinMax=(0.,1.))
        |> Chart.withY_AxisStyle ("p value",MinMax=(0.045,0.06))
        |> Chart.withTitle (sprintf "%.1f %.1f s1:%.1f s2:%.1f n:%i %b" mean1 mean2 stabw1 stabw2 n equal)
        |> Chart.withX_AxisStyle "p values" 
        |> Chart.withY_AxisStyle "relative count"
        |> Chart.withTraceName "pVal distribution"

    let distA = Continuous.normal mean1 stabw1
    let distB = Continuous.normal mean2 stabw2
    let dists = 
        [
            [-5. ..0.1 .. 5.] |> List.map (fun x -> x,distA.PDF x) |> Chart.Area |> Chart.withTraceName (sprintf "m:%.1f s:%.1f" mean1 stabw1)
            [-5. ..0.1 .. 5.] |> List.map (fun x -> x,distB.PDF x) |> Chart.Area |> Chart.withTraceName (sprintf "m:%.1f s:%.1f" mean2 stabw2)
        ]
        |> Chart.Combine
        |> Chart.withY_AxisStyle "pdf"

    [pVals;dists]
    |> Chart.Stack (1,0.2)
    |> Chart.Show

