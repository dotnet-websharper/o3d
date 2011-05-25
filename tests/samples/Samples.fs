namespace IntelliFactory.WebSharper.O3D.Samples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.O3D
open IntelliFactory.WebSharper.Html5

module Main =

    [<JavaScript>]
    let Samples () =
        let x = O3D.CurveKey(Input=3.0)
        Div [
        ]


[<JavaScriptType>]
type Samples() = 
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = Main.Samples() :> _

