namespace IntelliFactory.WebSharper.O3D.Samples

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.O3D
open IntelliFactory.WebSharper.Html5
//open IntelliFactory.WebSharper.O3D.Samples.Pool

module Main =

    // [<JavaScript>]
    // let Samples' () =
    //     let div = Div [Attr.Style "width:100%; height: 100%;"; Attr.Id "o3d"]
    //     let pool = new Pool()
    //     div
    //     |>! OnAfterRender (fun d ->
    //         pool.InitClient()
    //     )

    [<JavaScript>]
    let vertexShaderString = "
        // World View Projection matrix that will transform the input vertices
        // to screen space.
        attribute vec4 position;

        uniform mat4 world;
        uniform mat4 view;
        uniform mat4 projection;

        /**
         * The vertex shader simply transforms the input vertices to screen space.
         */
        void main() {
            // Multiply the vertex positions by the worldViewProjection matrix to
            // transform them to screen space.
            gl_Position = projection * view * world * position;
        }"

    [<JavaScript>]
    let pixelShaderString = "
        /**
         * This pixel shader just returns the color red.
         */
        void main() {
            gl_FragColor = vec4(1, 0, 0, 1);  // Red.
        }"

    [<JavaScript>]
    let createCube (pack : O3D.Pack) (material : O3D.Material) =
        let cubeShape = pack.CreateShape()
        let cubePrimitive = pack.CreatePrimitive()
        let streamBank = pack.CreateStreamBank()
        cubePrimitive.Material <- material
        cubePrimitive.Owner <- cubeShape
        cubePrimitive.StreamBank <- streamBank
        cubePrimitive.primitiveType <- O3D.Primitive.TRIANGLELIST
        cubePrimitive.NumberPrimitives <- 12
        cubePrimitive.NumberVertices <- 8
        cubePrimitive.CreateDrawElement(pack, null)
        let positionsArray = [|
            -0.5; -0.5;  0.5;  // vertex 0
             0.5; -0.5;  0.5;  // vertex 1
            -0.5;  0.5;  0.5;  // vertex 2
             0.5;  0.5;  0.5;  // vertex 3
            -0.5;  0.5; -0.5;  // vertex 4
             0.5;  0.5; -0.5;  // vertex 5
            -0.5; -0.5; -0.5;  // vertex 6
             0.5; -0.5; -0.5;  // vertex 7
        |]
        let indicesArray = [|
            0; 1; 2;   2; 1; 3;  // face 1
            2; 3; 4;   4; 3; 5;  // face 2
            4; 5; 6;   6; 5; 7;  // face 3
            6; 7; 0;   0; 7; 1;  // face 4
            1; 7; 3;   3; 7; 5;  // face 5
            6; 0; 4;   4; 0; 2;  // face 6
        |]
        let positionsBuffer = pack.CreateVertexBuffer()
        let positionsField = positionsBuffer.CreateFloatField(3)
        positionsBuffer.Set(positionsArray) |> ignore
        let indexBuffer = pack.CreateIndexBuffer()
        indexBuffer.Set(indicesArray) |> ignore
        streamBank.SetVertexStream(O3D.Stream.POSITION,
                                   0, positionsField, 0) |> ignore
        cubePrimitive.IndexBuffer <- indexBuffer
        cubeShape

    // let g_clock = ref 0.

    [<JavaScript>]
    let renderCallback (cubeTransform : O3D.Transform) (event : O3D.RenderEvent) =
        // g_clock := !g_clock + event.ElapsedTime
        cubeTransform.Identity()
        // cubeTransform.RotateY(2. * float !g_clock)

    [<JavaScript>]
    let Samples() =
        Div [Attr.Id "o3d"]
        |>! OnAfterRender (fun d ->
           // If you replace the following with (), it doesn't fail
           O3DJS.Webgl.MakeClients(fun (clients : Dom.Element[]) -> ())

//                let o3dElement = clients.[0]
//                let g_client = As<O3D.Client> <| JavaScript.Get "client" o3dElement
//                let g_pack = g_client.CreatePack()
//                let viewInfo = O3DJS.Rendergraph.CreateBasicView(g_pack, g_client.Root,
//                                                                 g_client.RenderGraphRoot)
//                viewInfo.DrawContext.Projection <-
//                    O3DJS.Math.Matrix4.Perspective(O3DJS.Math.DegToRad 30.,
//                                                   float g_client.Width/float g_client.Height,
//                                                   1., 5000.)
//                viewInfo.DrawContext.View <- O3DJS.Math.Matrix4.LookAt((0., 1., 5.),
//                                                                       (0., 0., 0.),
//                                                                       (0., 1., 0.))
//                let redEffect = g_pack.CreateEffect()
//                redEffect.LoadVertexShaderFromString vertexShaderString |> ignore
//                redEffect.LoadPixelShaderFromString pixelShaderString |> ignore
//                let redMaterial = g_pack.CreateMaterial()
//                redMaterial.DrawList <- viewInfo.PerformanceDrawList
//                redMaterial.Effect <- redEffect
//                let cubeShape = createCube g_pack redMaterial
//                let cubeTransform = g_pack.CreateTransform()
//                cubeTransform.AddShape cubeShape
//                cubeTransform.Parent <- g_client.Root
//                g_client.SetRenderCallback (renderCallback cubeTransform))
        )

[<JavaScriptType>]
type Samples() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body =
        Main.Samples() :> _

