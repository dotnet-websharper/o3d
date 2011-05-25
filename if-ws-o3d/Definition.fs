namespace IntelliFactory.WebSharper.O3DExtension

open IntelliFactory.WebSharper.Dom
open IntelliFactory.WebSharper.Html5

module O3D =
    open IntelliFactory.WebSharper.InterfaceGenerator

    module Util =

        let ConstantStrings type' strings =
            strings |> List.map (fun s ->
                s =? type'
                |> WithGetterInline ("\"" + s + "\"")
                :> CodeModel.IClassMember
            )

        let ClassWithInitArgs name args =
            Pattern.Config name {Optional=args; Required=[]}

    open Util

    //////// O3D ////////

    let Float2 = T<float> * T<float>
    let Float3 = T<float> * T<float> * T<float>
    let Float4 = T<float> * T<float> * T<float> * T<float>
    let BoundingBox = Float3 * Float3 * Float3
    let Matrix4 = Float4 * Float4 * Float4 * Float4
    let Quat = Float4

    let ObjectBase = Type.New()
    let ObjectBaseClass =
        Class "o3d.ObjectBase"
        |=> ObjectBase
        |+> Protocol
            [
                "isAClassName" => T<string> ^-> T<bool>
                "className" =? T<string>
                "clientId" =? T<int>
            ]

    let NamedObjectBase = Type.New()
    let NamedObjectBaseClass =
        ClassWithInitArgs "o3d.NamedObjectBase"
            [
                "name", T<string>
            ]
        |=> NamedObjectBase
        |=> Inherits ObjectBase

    let NamedObject = Type.New()
    let NamedObjectClass =
        Class "o3d.NamedObject"
        |=> NamedObject
        |=> Inherits NamedObjectBase

    let Param = Type.New()
    let ParamClass =
        Class "o3d.Param"
        |=> Inherits NamedObjectBase
        |=> Param
        |+> Protocol
            [
                "bind" => Param ^-> T<bool>
                "unbindInput" => T<unit> ^-> T<unit>
                "unbindOutput" => Param ^-> T<unit>
                "unbindOutputs" => T<unit> ^-> T<unit>
                "inputConnection" =? Param
                "outputConnections" =? Type.ArrayOf Param
                "readOnly" => T<bool>
                "updateInput" => T<bool>
            ]

    let ParamType = Type.New()
    let ParamTypeClass =
        Class "o3d.ParamType"
        |=> ParamType
        |+> ConstantStrings ParamType [
                "o3d.ParamBoolean"
                "o3d.ParamBoundingBox"
                "o3d.ParamDrawContext"
                "o3d.ParamDrawList"
                "o3d.ParamEffect"
                "o3d.ParamFloat"
                "o3d.ParamFloat2"
                "o3d.ParamFloat3"
                "o3d.ParamFloat4"
                "o3d.ParamFunction"
                "o3d.ParamInteger"
                "o3d.ParamMaterial"
                "o3d.ParamMatrix4"
                "o3d.ParamParamArray"
                "o3d.ParamRenderSurface"
                "o3d.ParamRenderDepthStencilSurface"
                "o3d.ParamSampler"
                "o3d.ParamSkin"
                "o3d.ParamSteamBank"
                "o3d.ParamState"
                "o3d.ParamString"
                "o3d.ParamTexture"
                "o3d.ParamTransform"
                "o3d.ProjectionParamMatrix4"
                "o3d.ProjectionInverseParamMatrix4"
                "o3d.ProjectionTransposeParamMatrix4"
                "o3d.ProjectionInverseTransposeParamMatrix4"
                "o3d.ViewParamMatrix4"
                "o3d.ViewInverseParamMatrix4"
                "o3d.ViewTransposeParamMatrix4"
                "o3d.ViewInverseTransposeParamMatrix4"
                "o3d.ViewProjectionParamMatrix4"
                "o3d.ViewProjectionInverseParamMatrix4"
                "o3d.ViewProjectionTransposeParamMatrix4"
                "o3d.ViewProjectionInverseTransposeParamMatrix4"
                "o3d.WorldParamMatrix4"
                "o3d.WorldInverseParamMatrix4"
                "o3d.WorldTransposeParamMatrix4"
                "o3d.WorldInverseTransposeParamMatrix4"
                "o3d.WorldViewParamMatrix4"
                "o3d.WorldViewInverseParamMatrix4"
                "o3d.WorldViewTransposeParamMatrix4"
                "o3d.WorldViewInverseTransposeParamMatrix4"
                "o3d.WorldViewProjectionParamMatrix4"
                "o3d.WorldViewProjectionInverseParamMatrix4"
                "o3d.WorldViewProjectionTransposeParamMatrix4"
                "o3d.WorldViewProjectionInverseTransposeParamMatrix4"
            ]

    let ParamObject = Type.New()
    let ParamObjectClass =
        Class "o3d.ParamObject"
        |=> ParamObject
        |=> Inherits NamedObject
        |+> Protocol
            [
                "copyParams" => ParamObject ^-> T<unit>
                "createParam" => T<string> * ParamType ^-> Param
                "getParam" => T<string> ^-> Param
                "removeParam" => Param ^-> T<bool>
                "params" =? Type.ArrayOf Param
            ]

    let RawData = Type.New()
    let RawDataClass =
        Class "o3d.RawData"
        |=> RawData
        |=> Inherits ParamObject
        |+> Protocol
            [
                "discard" => T<unit> ^-> T<unit>
                "flush" => T<unit> ^-> T<unit>
                "length" => T<int>
                "stringValue" => T<string>
                "uri" => T<string>
            ]

    let ArchiveRequest = Type.New()
    let ArchiveRequestClass =
        Class "o3d.ArchiveRequest"
        |=> ArchiveRequest
        |=> Inherits ObjectBase
        |+> Protocol
            [
                "open" => T<string>?method_ * T<string>?uri ^-> T<unit>
                "send" => T<unit> ^-> T<unit>
                "bytesReceived" =? T<int>
                "data" =? RawData
                "done" =? T<bool>
                "error" =? T<string>
                "onfileavailable" =! RawData ^-> T<unit>
                "onreadystatechange" =! T<unit> ^-> T<unit>
                "readyState" =? T<int>
                "streamLength" =? T<int>
                "success" =? T<bool>
                "uri" =? T<string>
            ]

    let CurveKey = Type.New()
    let CurveKeyClass =
        ClassWithInitArgs "o3d.CurveKey"
            [
                "input", T<float>
                "output", T<float>
            ]
        |=> CurveKey
        |=> Inherits ObjectBase
        |+> Protocol
            [
                "destroy" => T<unit> ^-> T<unit>
            ]

    let BezierCurveKey = Type.New()
    let BezierCurveKeyClass =
        ClassWithInitArgs "o3d.BezierCurveKey"
            [
                "inTangent", Float2
                "outTangent", Float2
            ]
        |=> BezierCurveKey
        |=> Inherits CurveKey

    let LinearCurveKey = Type.New()
    let LinearCurveKeyClass =
        Class "o3d.LinearCurveKey"
        |=> LinearCurveKey
        |=> Inherits CurveKey

    let StepCurveKey = Type.New()
    let StepCurveKeyClass =
        Class "o3d.StepCurveKey"
        |=> StepCurveKey
        |=> Inherits CurveKey

    let Bitmap_Semantic = Type.New()
    let Bitmap_SemanticClass =
        Pattern.EnumStrings "o3d.Bitmap.Semantic" [
            "FACE_POSITIVE_X"
            "FACE_NEGATIVE_X"
            "FACE_POSITIVE_Y"
            "FACE_NEGATIVE_Y"
            "FACE_POSITIVE_Z"
            "FACE_NEGATIVE_Z"
            "IMAGE"
            "SLICE"
        ]
        |=> Bitmap_Semantic

    let Texture_Format = Type.New()
    let Texture_FormatClass =
        Pattern.EnumStrings "o3d.Texture.Format" [
            "UNKNOWN_FORMAT"
            "XRGB8"
            "ARGB8"
            "ABGR16F"
            "R32F"
            "ABGR32F"
            "DXT1"
            "DXT3"
            "DXT5"
        ]
        |=> Texture_Format

    let Texture = Type.New()
    let TextureClass =
        Class "o3d.Texture"
        |=> Texture
        |=> Inherits ParamObject
        |=> Nested [Texture_FormatClass]
        |+> Protocol
            [
                "generateMips" => T<int> * T<int> ^-> T<unit>
                "alphaIsOne" =? T<bool>
                "format_" =? Texture_Format
                |> WithSourceName "format"
                "levels" =? T<int>
            ]

    let Bitmap = Type.New()
    let BitmapClass =
        Class "o3d.Bitmap"
        |=> Bitmap
        |=> Inherits ParamObject
        |=> Nested [Bitmap_SemanticClass]
        |+> Protocol
            [
                "flipVertically" => T<unit> ^-> T<unit>
                "generateMips" => T<int>?srclevel * T<int>?numlevels ^-> T<unit>
                "format" =? Texture_Format
                "height" =? T<int>
                "numMipmaps" =? T<int>
                "semantic_" =? Bitmap_Semantic
                |> WithSourceName "semantic"
                "width" =? T<int>
            ]

    let RenderSurfaceBase = Type.New()
    let RenderSurfaceBaseClass =
        Class "o3d.RenderSurfaceBase"
        |=> RenderSurfaceBase
        |=> Inherits ParamObject
        |+> Protocol
            [
                "height" =? T<int>
                "width" =? T<int>
            ]

    let RenderSurface = Type.New()
    let RenderSurfaceClass =
        Class "o3d.RenderSurface"
        |=> RenderSurface
        |=> Inherits RenderSurfaceBase
        |+> Protocol
            [
                "texture" =? Texture
            ]

    let Canvas = Type.New()

    let Texture2D = Type.New()
    let Texture2DClass =
        Class "o3d.Texture2D"
        |=> Texture2D
        |=> Inherits Texture
        |+> Protocol
            [
                "drawImage" => Bitmap * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth * T<int>?dstheight ^-> T<unit>
                "drawImage" => Bitmap * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth^-> T<unit>
                "drawImage" => Canvas * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth * T<int>?dstheight ^-> T<unit>
                "drawImage" => Canvas * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth^-> T<unit>
                "getRect" => T<int>?level * T<int>?x * T<int>?y * T<int>?width * T<int>?height ^-> Type.ArrayOf T<float>
                "getRenderSurface" => T<int>?level ^-> RenderSurface
                "set" => T<int>?level * (Type.ArrayOf T<float>)?data ^-> T<unit>
                "setFromBitmap" => Bitmap?source ^-> T<unit>
                "setRect" => T<int>?level * T<int>?dstx * T<int>?dsty * T<int>?srcwidth * (Type.ArrayOf T<float>)?data ^-> T<unit>
                "height" =? T<int>
                "width" =? T<int>
            ]

    let TextureCUBE_CubeFace = Type.New()
    let TextureCUBE_CubeFaceClass =
        Pattern.EnumStrings "o3d.TextureCUBE.CubeFace" [
            "FACE_POSITIVE_X"
            "FACE_NEGATIVE_X"
            "FACE_POSITIVE_Y"
            "FACE_NEGATIVE_Y"
            "FACE_POSITIVE_Z"
            "FACE_NEGATIVE_Z"
        ]
        |=> TextureCUBE_CubeFace

    let TextureCUBE = Type.New()
    let TextureCUBEClass =
        Class "o3d.TextureCUBE"
        |=> TextureCUBE
        |=> Inherits Texture
        |+> Protocol
            [
                "drawImage" => Bitmap * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?face * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth * T<int>?dstheight ^-> T<unit>
                "drawImage" => Bitmap * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?face * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth^-> T<unit>
                "drawImage" => Canvas * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?face * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth * T<int>?dstheight ^-> T<unit>
                "drawImage" => Canvas * T<int>?srclevel * T<int>?srcx * T<int>?srcy * T<int>?srcwidth * T<int>?srcheight * T<int>?face * T<int>?dstlevel * T<int>?dstx * T<int>?dsty * T<int>?dstwidth^-> T<unit>
                "getRect" => TextureCUBE_CubeFace * T<int>?level * T<int>?x * T<int>?y * T<int>?width * T<int>?height ^-> Type.ArrayOf T<float>
                "getRenderSurface" => TextureCUBE_CubeFace * T<int>?level ^-> RenderSurface
                "set" => TextureCUBE_CubeFace * T<int>?level * Type.ArrayOf T<float> ^-> T<unit>
                "setFromBitmap" => TextureCUBE_CubeFace * Bitmap ^-> T<unit>
                "setRect" => TextureCUBE_CubeFace * T<int>?level * T<int>?dstx * T<int>?dsty * T<int>?srcwidth * Type.ArrayOf T<float> ^-> T<unit>
                "edgeLength" =? T<int>
            ]

    let Field = Type.New()

    let FieldType = Type.New()
    let FieldTypeClass =
        Class "o3d.FieldType"
        |=> FieldType
        |+> ConstantStrings FieldType [
                "FloatField"
                "UInt32Field"
                "UByteNField"
            ]

    let FloatField = Type.New()
    let FloatFieldClass =
        Class "o3d.FloatField"
        |=> FloatField
        |=> Inherits Field
        |+> Protocol
            [
                "getAt" => T<int>?startIndex * T<int>?numElements ^-> Type.ArrayOf T<float>
                "setAt" => T<int>?startIndex * Type.ArrayOf T<float> ^-> T<unit>
            ]

    let UByteNField = Type.New()
    let UByteNFieldClass =
        Class "UByteNField"
        |=> UByteNField
        |+> Protocol
            [
                "getAt" => T<int>?start * T<int>?count ^-> Type.ArrayOf T<int>
                "setAt" => T<int>?start * Type.ArrayOf T<int> ^-> T<unit>
            ]

    let UInt32Field = Type.New()
    let UInt32FieldClass =
        Class "UInt32Field"
        |=> UInt32Field
        |+> Protocol
            [
                "getAt" => T<int>?start * T<int>?count ^-> Type.ArrayOf T<int>
                "setAt" => T<int>?start * Type.ArrayOf T<int> ^-> T<unit>
            ]

    let Buffer = Type.New()
    let BufferClass =
        Class "o3d.Buffer"
        |=> Buffer
        |=> Inherits NamedObject
        |+> Protocol
            [
                "allocateElements" => T<int> ^-> T<bool>
                "createField" => FieldType * T<int> ^-> Field
                "createUByteNField" => T<int> ^-> UByteNField
                "createFloatField" => T<int> ^-> FloatField
                "createUInt32Field" => T<int> ^-> UInt32Field
                "removeField" => Field ^-> T<unit>
                "set" => RawData * T<int>?srcoffset * T<int>?length ^-> T<unit>
                "set" => RawData * T<int>?srcoffset ^-> T<unit>
                "set" => RawData ^-> T<unit>
                "fields" =? Type.ArrayOf Field
                "numElements" =? T<int>
                "totalComponents" =? T<int>
            ]

    let CanvasFontMetrics = Type.New()
    let CanvasFontMetricsClass =
        Class "o3d.CanvasFontMetrics"
        |=> CanvasFontMetrics
        |+> Protocol
            [
                "ascent" =? T<float>
                "bottom" =? T<float>
                "descent" =? T<float>
                "leading" =? T<float>
                "top" =? T<float>
            ]

    let CanvasPaint_Style = Type.New()
    let CanvasPaint_StyleClass =
        Pattern.EnumStrings "o3d.CanvasPaint.Style" [
            "NORMAL"
            "BOLD"
            "ITALIC"
            "BOLD_ITALIC"
        ]
        |=> CanvasPaint_Style

    let CanvasPaint_TextAlign = Type.New()
    let CanvasPaint_TextAlignClass =
        Pattern.EnumStrings "o3d.CanvasPaint.TextAlign" [
            "LEFT"
            "CENTER"
            "RIGHT"
        ]
        |=> CanvasPaint_TextAlign

    let CanvasShader_TileMode = Type.New()
    let CanvasShader_TileModeClass =
        Pattern.EnumStrings "o3d.CanvasShader.TileMode" [
            "CLAMP"
            "REPEAT"
            "MIRROR"
        ]
        |=> CanvasShader_TileMode

    let CanvasShader = Type.New()
    let CanvasShaderClass =
        Class "o3d.CanvasShader"
        |=> CanvasShader
        |=> Inherits ParamObject
        |+> Protocol
            [
            ]

    let CanvasPaint = Type.New()
    let CanvasPaintClass =
        ClassWithInitArgs "o3d.CanvasPaint"
            [
                "color", Float4
                "shader", CanvasShader
                "textAlign", CanvasPaint_TextAlign
                "textSize", T<int>
                "textStyle", CanvasPaint_Style
                "textTypeface", T<string>
            ]
        |=> CanvasPaint
        |=> Inherits ParamObject
        |+> Protocol
            [
                "getFontMetrics" => T<unit> ^-> CanvasFontMetrics
                "measureText" => T<string> ^-> Float4
                "setOutline" => T<float>?radius * Float4?color ^-> T<unit>
                "setShadow" => T<float>?radius * T<float>?offsetx * T<float>?offsety * Float4?color ^-> T<unit>
            ]

    let CanvasClass =
        Class "o3d.Canvas"
        |=> Inherits ParamObject
        |=> Canvas
        |+> Protocol
            [
                "clear" => Float4 ^-> T<unit>
                "copyToTexture" => Texture2D ^-> T<unit>
                "drawBitmap" => Texture2D * T<int>?left * T<int>?bottom ^-> T<unit>
                "drawRect" => T<int>?left * T<int>?top * T<int>?right * T<int>?bottom * CanvasPaint ^-> T<unit>
                "drawText" => T<string> * T<int>?x * T<int>?y * CanvasPaint ^-> T<unit>
                "drawTextOnPath" => T<string> * (Type.ArrayOf Float2)?positions * T<int>?hoffset * T<int>?voffset * CanvasPaint ^-> T<unit>
                "restoreMatrix" => T<unit> ^-> T<unit>
                "rotate" => T<float>?degrees ^-> T<unit>
                "saveMatrix" => T<unit> ^-> T<unit>
                "scale" => T<float>?sx * T<float>?sy ^-> T<unit>
                "setSize" => T<int>?width * T<int>?height ^-> T<unit>
                "translate" => T<float>?dx * T<float>?dy ^-> T<unit>
                "height" =? T<int>
                "width" =? T<int>
            ]

    let CanvasLinearGradient = Type.New()
    let CanvasLinearGradientClass =
        ClassWithInitArgs "o3d.CanvasLinearGradient"
            [
                "colors", Type.ArrayOf Float4
                "endPoint", Float2
                "positions", Type.ArrayOf T<int>
                "startPoint", Float2
                "tileMode", CanvasShader_TileMode
            ]
        |=> CanvasLinearGradient
        |=> Inherits CanvasShader

    let RenderNode = Type.New()
    let RenderNodeClass =
        ClassWithInitArgs "o3d.RenderNode"
            [
                "active", T<bool>
                "priority", T<float>
                "parent", RenderNode
            ]
        |=> RenderNode
        |=> Inherits ParamObject
        |+> Protocol
            [
                "getRenderNodesByClassNameInTree" => T<string> ^-> Type.ArrayOf RenderNode
                "getRenderNodesByNameInTree" => T<string> ^-> Type.ArrayOf RenderNode
                "getRenderNodesInTree" => T<unit> ^-> Type.ArrayOf RenderNode
                "children" =? Type.ArrayOf RenderNode
            ]

    let ClearBuffer = Type.New()
    let ClearBufferClass =
        ClassWithInitArgs "o3d.ClearBuffer"
            [
                "clearColor", Float4
                "clearColorFlag", T<bool>
                "clearDepth", T<float>
                "clearDepthFlag", T<bool>
                "clearStencil", T<int>
                "clearStencilFlag", T<bool>
            ]
        |=> ClearBuffer
        |=> Inherits RenderNode

    let Client_RenderMode = Type.New()
    let Client_RenderModeClass =
        Pattern.EnumStrings "o3d.RenderMode" [
            "RENDERMODE_CONTINUOUS"
            "RENDERMODE_ON_DEMAND"
        ]
        |=> Client_RenderMode

    let RenderDepthStencilSurface = Type.New()
    let RenderDepthStencilSurfaceClass =
        Class "o3d.RenderDepthStencilSurface"
        |=> RenderDepthStencilSurface
        |=> Inherits RenderSurfaceBase

    let FileRequestType = Type.New()
    let FileRequestTypeClass =
        Pattern.EnumStrings "o3d.FileRequestType" [
            "TEXTURE"
            "RAWDATA"
        ]
        |=> FileRequestType

    let FileRequest = Type.New()
    let FileRequestClass =
        Class "o3d.FileRequest"
        |=> FileRequest
        |=> Inherits ObjectBase
        |+> Protocol
            [
                "open" => T<string>?method_ * T<string>?uri * T<bool>?async ^-> T<unit>
                "send" => T<unit> ^-> T<unit>
                "data" =? RawData
                "done" =? T<bool>
                "error" =? T<string>
                "generateMipmaps" =? T<bool>
                "onreadystatechange" =! T<unit -> unit>
                "readyState" =? T<int>
                "success" =? T<bool>
                "texture" =? Texture
                "uri" =? T<string>
            ]

    let ObjectType = Type.New()
    let ObjectTypeClass =
        Class "o3d.ObjectType"
        |=> ObjectType
        |+> ConstantStrings ObjectType [
            "o3d.Bitmap"
            "o3d.Canvas"
            "o3d.CanvasLinearGradient"
            "o3d.CanvasPaint"
            "o3d.ClearBuffer"
            "o3d.Counter"
            "o3d.Curve"
            "o3d.DrawContext"
            "o3d.DrawElement"
            "o3d.DrawList"
            "o3d.DrawPass"
            "o3d.Effect"
            "o3d.FunctionEval"
            "o3d.IndexBuffer"
            "o3d.Material"
            "o3d.ParamArray"
            "o3d.ParamObject"
            "o3d.Primitive"
            "o3d.RenderFrameCounter"
            "o3d.RenderNode"
            "o3d.RenderSurfaceSet"
            "o3d.Sampler"
            "o3d.SecondCounter"
            "o3d.Shape"
            "o3d.Skin"
            "o3d.SkinEval"
            "o3d.SourceBuffer"
            "o3d.State"
            "o3d.StateSet"
            "o3d.StreamBank"
            "o3d.Texture2D"
            "o3d.TextureCUBE"
            "o3d.TickCounter"
            "o3d.Transform"
            "o3d.TreeTraversal"
            "o3d.VertexBuffer"
            "o3d.Viewport"
            "o3d.Matrix4AxisRotation"
            "o3d.Matrix4Composition"
            "o3d.Matrix4Scale"
            "o3d.Matrix4Translation"
            "o3d.ParamOp2FloatsToFloat2"
            "o3d.ParamOp3FloatsToFloat3"
            "o3d.ParamOp4FloatsToFloat4"
            "o3d.ParamOp16FloatsToMatrix4"
            "o3d.TRSToMatrix4"
        ]

    let Pack = Type.New()
    let PackClass =
        Class "o3d.Pack"
        |=> Pack
        |=> Inherits NamedObject
        |+> Protocol
            [
                "createArchiveRequest" => T<unit> ^-> ArchiveRequest
                "createBitmapsFromRawData" => RawData ^-> Bitmap
                "createDepthStencilSurface" => T<int>?width * T<int>?height ^-> RenderDepthStencilSurface
                "createFileRequest" => FileRequestType ^-> FileRequest
                "createObject" => ObjectType ^-> ObjectBase // TODO: individual methods ?
                "createRawDataFromDataUrl" => T<string> ^-> RawData
                "createTexture2D" => T<int>?width * T<int>?height * Texture_Format * T<int>?levels * T<bool>?enableRenderSurfaces ^-> Texture2D
                "createTextureCUBE" => T<int>?edgeLength * Texture_Format * T<int>?levels * T<bool>?enableRenderSurface ^-> TextureCUBE
                "createTextureFromRawData" => RawData * T<bool>?generateMips ^-> Texture
                "destroy" => T<unit> ^-> T<unit>
                "getObjects" => T<string>?name * ObjectType ^-> Type.ArrayOf ObjectBase // TODO : individual methods?
                "getObjectsByClassName" => ObjectType ^-> Type.ArrayOf ObjectBase
                "removeObject" => ObjectBase ^-> T<bool>
                "objects" =? Type.ArrayOf ObjectBase
            ]

    let DisplayMode = Type.New()
    let DisplayModeClass =
        Class "o3d.DisplayMode"
        |=> DisplayMode
        |+> Protocol
            [
                "height" =? T<int>
                "id" =? T<int>
                "refreshRate" =? T<float>
                "width" =? T<int>
            ]

    let Event_Button = Type.New()
    let Event_ButtonClass =
        Pattern.EnumStrings "o3d.Event.Button" [
            "BUTTON_LEFT"
            "BUTTON_RIGHT"
            "BUTTON_MIDDLE"
            "BUTTON_4"
            "BUTTON_5"
        ]
        |=> Event_Button

    let Event_Type = Type.New()
    let Event_TypeClass =
        Pattern.EnumStrings "o3d.Event.Type" [
            "invalid"
            "click"
            "dblclick"
            "mousedown"
            "mousemove"
            "mouseup"
            "wheel"
            "keydown"
            "keypress"
            "keyup"
            "resize"
        ]
        |=> Event_Type

    let Event = Type.New()
    let EventClass =
        Class "o3d.Event"
        |=> Event
        |=> Nested [Event_ButtonClass; Event_TypeClass]
        |+> Protocol
            [
                "altKey" =? T<bool>
                "button_" =? Event_Button
                |> WithSourceName "button"
                "charCode" =? T<int>
                "ctrlKey" =? T<bool>
                "deltaX" =? T<int>
                "deltaY" =? T<int>
                "fullscreen" =? T<bool>
                "height" =? T<int>
                "keyCode" =? T<int>
                "metaKey" =? T<bool>
                "screenX" =? T<int>
                "screenY" =? T<int>
                "shiftKey" =? T<bool>
                "type_" =? Event_Type
                |> WithSourceName "type"
                "width" =? T<int>
                "x" =? T<int>
                "y" =? T<int>
            ]

    let RenderEvent = Type.New()
    let RenderEventClass =
        Class "o3d.RenderEvent"
        |=> RenderEvent
        |+> Protocol
            [
                "activeTime" =? T<float>
                "drawElementsCulled" =? T<int>
                "drawElementsProcessed" =? T<int>
                "drawElementsRendered" =? T<int>
                "elapsedTime" =? T<float>
                "primitivesRendered" =? T<int>
                "renderTime" =? T<float>
                "transformsCulled" =? T<int>
                "transformsProcessed" =? T<int>
            ]

    let TickEvent = Type.New()
    let TickEventClass =
        Class "o3d.TickEvent"
        |=> TickEvent
        |+> Protocol
            [
                "elapsedTime" =? T<float>
            ]

    let ClientInfo = Type.New()
    let ClientInfoClass =
        Class "o3d.ClientInfo"
        |=> ClientInfo
        |+> Protocol
            [
                "bufferMemoryUsed" =? T<int>
                "nonPowerOfTwoTextures" =? T<bool>
                "numObjects" =? T<int>
                "softwareRenderer" =? T<bool>
                "textureMemoryUsed" =? T<int>
            ]

    let Cursor_CursorType = Type.New()
    let Cursor_CursorTypeClass =
        Pattern.EnumStrings "o3d.CursorType" [
            "DEFAULT"
            "NONE"
            "CROSSHAIR"
            "POINTER"
            "E_RESIZE"
            "NE_RESIZE"
            "NW_RESIZE"
            "N_RESIZE"
            "SE_RESIZE"
            "SW_RESIZE"
            "S_RESIZE"
            "W_RESIZE"
            "MOVE"
            "TEXT"
            "WAIT"
            "PROGRESS"
            "HELP"
        ]
        |=> Cursor_CursorType

    let Cursor = Type.New()
    let CursorClass =
        Class "o3d.Cursor"
        |=> Cursor
        |=> Nested [Cursor_CursorTypeClass]

    let Renderer_InitStatus = Type.New()
    let Renderer_InitStatusClass =
        Pattern.EnumStrings "o3d.Cursor.InitStatus" [
            "UNINITIALIZED"
            "SUCCESS"
            "GPU_NOT_UP_TO_SPEC"
            "OUT_OF_RESOURCES"
            "INITIALIZATION_ERROR"
        ]
        |=> Renderer_InitStatus

    let Renderer_DisplayModes = Type.New()
    let Renderer_DisplayModesClass =
        Pattern.EnumStrings "o3d.Renderer.DisplayModes" [
            "DISPLAY_MODE_DEFAULT"
        ]
        |=> Renderer_DisplayModes

    let Renderer = Type.New()
    let RendererClass =
        Class "o3d.Renderer"
        |=> Renderer
        |=> Nested [Renderer_InitStatusClass; Renderer_DisplayModesClass]

    let DrawList_SortMethod = Type.New()
    let DrawList_SortMethodClass =
        Pattern.EnumStrings "o3d.DrawList.SortMethod" [
            "BY_PERFORMANCE"
            "BY_Z_ORDER"
            "BY_PRIORITY"
        ]
        |=> DrawList_SortMethod

    let DrawList = Type.New()
    let DrawListClass =
        Class "o3d.DrawList"
        |=> DrawList
        |=> Inherits NamedObject
        |=> Nested [DrawList_SortMethodClass]

    let Stream_Semantic = Type.New()
    let Stream_SemanticClass =
        Pattern.EnumStrings "o3d.Stream.Semantic" [
            "UNKNOWN_SEMANTIC"
            "POSITION"
            "NORMAL"
            "TANGENT"
            "BINORMAL"
            "COLOR"
            "TEXCOORD"
        ]
        |=> Stream_Semantic

    let Stream = Type.New()
    let StreamClass =
        Class "o3d.Stream"
        |=> Stream
        |=> Nested [Stream_SemanticClass]
        |+> Protocol
            [
                "field" =? Field
                "semantic_" =? Stream_Semantic
                |> WithSourceName "semantic"
                "semanticIndex" =? T<int>
                "startIndex" =? T<int>
            ]

    let Effect_MatrixLoadOrder = Type.New()
    let Effect_MatrixLoadOrderClass =
        Pattern.EnumStrings "o3d.Effect.MatrixLoadOrder" [
            "ROW_MAJOR"
            "COLUMN_MAJOR"
        ]
        |=> Effect_MatrixLoadOrder

    let EffectParameterInfo = Type.New()
    let EffectParameterInfoClass =
        let semantic =
            Pattern.EnumStrings "o3d.Effect.Semantic" [
                "UPPERCASE"
            ]
        Class "o3d.EffectParameterInfo"
        |=> EffectParameterInfo
        |=> Nested [semantic]
        |+> Protocol
            [
                "className" =? T<string>
                "name" =? T<string>
                "numElements" =? T<int>
                "sasClassName" =? T<string>
                "semantic_" =? semantic
                |> WithSourceName "semantic"
            ]

    let EffectStreamInfo = Type.New()
    let EffectStreamInfoClass =
        Class "o3d.EffectStreamInfo"
        |=> EffectStreamInfo
        |+> Protocol
            [
                "semantic" =? Stream_Semantic
                "semanticIndex" =? T<int>
            ]

    let Effect = Type.New()
    let EffectClass =
        Class "o3d.Effect"
        |=> Effect
        |=> Inherits ParamObject
        |=> Nested [Effect_MatrixLoadOrderClass]
        |+> Protocol
            [
                "createSASParameters" => ParamObject ^-> T<unit>
                "createUniformParameters" => ParamObject ^-> T<unit>
                "getParameterInfo" => T<unit> ^-> EffectParameterInfo
                "getStreamInfo" => T<unit> ^-> EffectStreamInfo
                "loadFromFXString" => T<string> ^-> T<bool>
                "matrixLoadOrder" => Effect_MatrixLoadOrder
                "source" => T<string>
            ]

    let State_BlendingEquation = Type.New()
    let State_BlendingEquationClass =
        Pattern.EnumStrings "o3d.State.BlendingEquation" [
            "BLEND_ADD"
            "BLEND_SUBSTRACT"
            "BLEND_REVERSE_SUBSTRACT"
            "BLEND_MIN"
            "BLEND_MAX"
        ]
        |=> State_BlendingEquation

    let State_BlendingFunction = Type.New()
    let State_BlendingFunctionClass =
        Pattern.EnumStrings "o3d.State.BlendingFunction" [
            "BLENDFUNC_ZERO"
            "BLENDFUNC_ONE"
            "BLENDFUNC_SOURCE_COLOR"
            "BLENDFUNC_INVERSE_SOURCE_COLOR"
            "BLENDFUNC_SOURCE_ALPHA"
            "BLENDFUNC_INVERSE_SOURCE_ALPHA"
            "BLENDFUNC_DESTINATION_ALPHA"
            "BLENDFUNC_INVERSE_DESTINATION_ALPHA"
            "BLENDFUNC_DESTINATION_COLOR"
            "BLENDFUNC_INVERSE_DESTINATION_COLOR"
            "BLENDFUNC_SOURCE_ALPHA_SATURATE"
        ]
        |=> State_BlendingFunction

    let State_Comparison = Type.New()
    let State_ComparisonClass =
        Pattern.EnumStrings "o3d.State.Comparison" [
            "CMP_NEVER"
            "CMP_LESS"
            "CMP_EQUAL"
            "CMP_LEQUAL"
            "CMP_GREATER"
            "CMP_NOTEQUAL"
            "CMP_GEQUAL"
            "CMP_ALWAYS"
        ]
        |=> State_Comparison

    let State_Cull = Type.New()
    let State_CullClass =
        Pattern.EnumStrings "o3d.State.Cull" [
            "CULL_NONE"
            "CULL_CW"
            "CULL_CCW"
        ]
        |=> State_Cull

    let State_Fill = Type.New()
    let State_FillClass =
        Pattern.EnumStrings "o3d.State.Fill" [
            "POINT"
            "WIREFRAME"
            "SOLID"
        ]
        |=> State_Fill

    let State_StencilOperation = Type.New()
    let State_StencilOperationClass =
        Pattern.EnumStrings "o3d.State.StencilOperation" [
            "STENCIL_KEEP"
            "STENCIL_ZERO"
            "STENCIL_REPLACE"
            "STENCIL_INCREMENT_SATURATE"
            "STENCIL_DECREMENT_SATURATE"
            "STENCIL_INVERT"
            "STENCIL_INCREMENT"
            "STENCIL_DECREMENT"
        ]
        |=> State_StencilOperation

    let State = Type.New()
    let StateClass =
        Class "o3d.State"
        |=> State
        |=> Nested [State_BlendingEquationClass
                    State_BlendingFunctionClass
                    State_ComparisonClass
                    State_CullClass
                    State_FillClass
                    State_StencilOperationClass]
        |+> Protocol
            [
                "getStateParam" => T<string> ^-> Param // TODO
            ]

    let Material = Type.New()
    let MaterialClass =
        ClassWithInitArgs "o3d.Material"
            [
                "drawList", DrawList
                "effect", Effect
                "state", State
            ]
        |=> Material
        |=> Inherits ParamObject

    let RayIntersectionInfo = Type.New()
    let RayIntersectionInfoClass =
        Class "o3d.RayIntersectionInfo"
        |=> RayIntersectionInfo
        |+> Protocol
            [
                "intersected" =? T<bool>
                "position" =? Float3
                "primitiveIndex" =? T<int>
                "valid" =? T<bool>
            ]

    let Element = Type.New()

    let DrawElement = Type.New()
    let DrawElementClass =
        ClassWithInitArgs "o3d.DrawElement"
            [
                "material", Material
                "owner", Element
            ]
        |=> DrawElement
        |=> Inherits ParamObject

    let Shape = Type.New()

    let ElementClass =
        ClassWithInitArgs "o3d.Element"
            [
                "boundingBox", BoundingBox
                "cull", T<bool>
                "owner", Shape
                "priority", T<int>
                "zSortPoint", Float3
            ]
        |=> Element
        |=> Inherits ParamObject
        |+> Protocol
            [
                "createDrawElement" => Pack * Material ^-> T<unit>
                "getBoundingBox" => T<int> ^-> BoundingBox
                "intersectRay" => T<int>?positionStreamIndex * State_Cull * Float3?start * Float3?end_ ^-> RayIntersectionInfo
                "drawElements" =? DrawElement
            ]

    let ShapeClass =
        Class "o3d.Shape"
        |=> Inherits ParamObject
        |=> Shape
        |+> Protocol
            [
                "createDrawElements" => Pack * Material ^-> T<unit>
                "elements" =? Type.ArrayOf Element
            ]

    let Transform = Type.New()
    let TransformClass =
        ClassWithInitArgs "o3d.Transform"
            [
                "boundingBox", BoundingBox
                "cull", T<bool>
                "localMatrix", Matrix4
                "parent", Transform
                "visible", T<bool>
            ]
        |=> Inherits ParamObject
        |=> Transform
        |+> Protocol
            [
                "addShape" => Shape ^-> T<unit>
                "axisRotate" => T<float>?radians * Float3?axis ^-> T<unit>
                "createDrawElements" => Pack * Material ^-> T<unit>
                "getTransformsByNameInTree" => T<string> ^-> Type.ArrayOf Transform
                "getTransformsInTree" => T<unit> ^-> Type.ArrayOf Transform
                "getUpdatedWorldMatrix" => T<unit> ^-> Matrix4
                "identity" => T<unit> ^-> T<unit>
                "quaternionRotate" => Quat ^-> T<unit>
                "removeShape" => Shape ^-> T<bool>
                "rotateX" => T<float>?radians ^-> T<unit>
                "rotateY" => T<float>?radians ^-> T<unit>
                "rotateZ" => T<float>?radians ^-> T<unit>
                "rotateZYX" => Float3?radiansXYZ ^-> T<unit>
                "scale" => Float3 ^-> T<unit>
                "translate" => Float3 ^-> T<unit>
                "children" =? Type.ArrayOf Transform
                "shapes" =? Type.ArrayOf Shape
                "worldMatrix" =? Matrix4
            ]

    let Client = Type.New()
    let ClientClass =
        Class "o3d.Client"
        |=> Client
        |=> Nested [Client_RenderModeClass]
        |+> Protocol
            [
                "cancelFullscreenDisplay" => T<unit> ^-> T<unit>
                "cleanup" => T<unit> ^-> T<unit>
                "clearErrorCallback" => T<unit> ^-> T<unit>
                "clearEventCallback" => T<string> ^-> T<unit>
                "clearFullscreenClickRegion" => T<unit> ^-> T<unit>
                "clearLastError" => T<unit> ^-> T<unit>
                "clearLostResourcesCallback" => T<unit> ^-> T<unit>
                "clearPostRenderCallback" => T<unit> ^-> T<unit>
                "clearRenderCallback" => T<unit> ^-> T<unit>
                "clearTickCallback" => T<unit> ^-> T<unit>
                "createPack" => T<unit> ^-> Pack
                "getDisplayModes" => T<unit> ^-> DisplayMode
                "getMessageQueueAddress" => T<unit> ^-> T<string>
                "getObjectById" => T<int> ^-> ObjectBase
                "getObjects" => T<string>?name * ObjectType ^-> Type.ArrayOf ObjectBase
                "getObjectsByClassName" => ObjectType ^-> Type.ArrayOf ObjectBase
                "invalidateAllParameters" => T<unit> ^-> T<unit>
                "profileReset" => T<unit> ^-> T<unit>
                "profileToString" => T<unit> ^-> T<string>
                "render" => T<unit> ^-> T<unit>
                "renderTree" => RenderNode ^-> T<unit>
                "setErrorCallback" => (T<string> ^-> T<unit>) ^-> T<unit>
                "setErrorTexture" => Texture ^-> T<unit>
                "setEventCallback" => T<string> * (Event ^-> T<unit>) ^-> T<unit>
                "setFullscreenClickRegion" => T<int>?x * T<int>?y * T<int>?width * T<int>?height * T<int>?modeId ^-> T<unit>
                "setLostResourcesCallback" => (T<unit> ^-> T<unit>) ^-> T<unit>
                "setPostRenderCallback" => (RenderEvent ^-> T<unit>) ^-> T<unit>
                "setRenderCallback" => (RenderEvent ^-> T<unit>) ^-> T<unit>
                "setTickCallback" => (TickEvent ^-> T<unit>) ^-> T<unit>
                "toDataUrl" => T<string> ^-> T<string>
                "clientId" =? T<int>
                "clientInfo" =? ClientInfo
                "cursor" =? Cursor_CursorType
                "fullscreen" =? T<bool>
                "height" =? T<int>
                "lastError" =? T<string>
                "objects" =? Type.ArrayOf ObjectBase
                "rendererInitStatus" =? Renderer_InitStatus
                "renderGraphRoot" =? RenderNode
                "renderMode_" =? Client_RenderMode
                |> WithSourceName "renderMode"
                "root" =? Transform
                "width" =? T<int>
            ]

    let Counter_CountMode = Type.New()
    let Counter_CountModeClass =
        Pattern.EnumStrings "o3d.Counter.CountMode" [
            "CONTINUOUS"
            "ONCE"
            "CYCLE"
            "OSCILLATE"
        ]
        |=> Counter_CountMode

    let Counter = Type.New()
    let CounterClass =
        ClassWithInitArgs "o3d.Counter"
            [
                "end", T<float>
                "forward", T<bool>
                "multiplier", T<float>
                "running", T<bool>
                "start", T<float>
            ]
        |=> Counter
        |=> Inherits ParamObject
        |=> Nested [Counter_CountModeClass]
        |+> Protocol
            [
                "addCallback" => T<float> * (T<unit> ^-> T<unit>) ^-> T<unit>
                "advance" => T<float> ^-> T<unit>
                "getCallbackCounts" => Type.ArrayOf T<float>
                "removeAllCallbacks" => T<unit> ^-> T<unit>
                "removeCallback" => T<float> ^-> T<bool>
                "reset" => T<unit> ^-> T<unit>
                "setCount" => T<float> ^-> T<unit>
                "count" =? T<float>
                "countMode_" =@ Counter_CountMode
                |> WithSourceName "countMode"
            ]

    let Function = Type.New()
    let FunctionClass =
        Class "o3d.Function"
        |=> Function
        |=> Inherits NamedObject
        |+> Protocol
            [
                "evaluate" => T<float> ^-> T<float>
            ]

    let Curve_Infinity = Type.New()
    let Curve_InfinityClass =
        Pattern.EnumStrings "Infinity" [
            "CONSTANT"
            "LINEAR"
            "CYCLE"
            "CYCLE_RELATIVE"
            "OSCILLATE"
        ]
        |=> Curve_Infinity

    let Curve = Type.New()
    let CurveClass =
        ClassWithInitArgs "o3d.Curve"
            [
                "postInfinity", Curve_Infinity
                "preInfinity", Curve_Infinity
                "sampleRate", T<float>
                "useCache", T<bool>
            ]
        |=> Curve
        |=> Inherits Function
        |=> Nested [Curve_InfinityClass]
        |+> Protocol
            [
                "addBezierKeys" => Type.ArrayOf T<float> ^-> T<unit>
                "addLinearKeys" => Type.ArrayOf T<float> ^-> T<unit>
                "addStepKeys" => Type.ArrayOf T<float> ^-> T<unit>
                "createKey" => T<string> ^-> CurveKey
                "createBezierKey" => T<unit> ^-> BezierCurveKey
                |> WithInline "$this.createKey('o3d.BezierCurveKey')"
                "createLinearKey" => T<unit> ^-> LinearCurveKey
                |> WithInline "$this.createKey('o3d.LinearCurveKey')"
                "createStepKey" => T<unit> ^-> StepCurveKey
                |> WithInline "$this.createKey('o3d.StepCurveKey')"
                "isDiscontinuous" => T<unit> ^-> T<bool>
                "set" => RawData * T<int>?offset * T<int>?length ^-> T<bool>
                "set" => RawData * T<int>?offset ^-> T<bool>
                "set" => RawData ^-> T<bool>
            ]

    let DrawContext = Type.New()
    let DrawContextClass =
        ClassWithInitArgs "o3d.DrawContext"
            [
                "projection", Matrix4
                "view", Matrix4
            ]
        |=> DrawContext
        |=> Inherits ParamObject

    let DrawPass = Type.New()
    let DrawPassClass =
        ClassWithInitArgs "o3d.DrawPass"
            [
                "drawList", DrawList
                "sortMethod", DrawList_SortMethod
            ]
        |=> DrawPass
        |=> Inherits RenderNode

    let FieldClass =
        Class "o3d.Field"
        |=> Inherits NamedObject
        |=> Field
        |+> Protocol
            [
                "buffer" =? Buffer
                "numComponents" =? T<int>
                "offset" =? T<int>
                "size" =? T<int>
            ]

    let FunctionEval = Type.New()
    let FunctionEvalClass =
        ClassWithInitArgs "o3d.FunctionEval"
            [
                "functionObject", Function
                "input", T<float>
            ]
        |=> FunctionEval
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? T<float>
            ]

    let IndexBuffer = Type.New()
    let IndexBufferClass =
        Class "o3d.IndexBuffer"
        |=> IndexBuffer
        |=> Inherits Buffer
        |+> Protocol
            [
                "set" => Type.ArrayOf T<int> ^-> T<bool>
                "setAt" => T<int>?startIndex * Type.ArrayOf T<int> ^-> T<unit>
            ]

    let Matrix4AxisRotation = Type.New()
    let Matrix4AxisRotationClass =
        ClassWithInitArgs "o3d.Matrix4AxisRotation"
            [
                "angle", T<float>
                "axis", Float3
                "inputMatrix", Matrix4
            ]
        |=> Matrix4AxisRotation
        |=> Inherits ParamObject
        |+> Protocol
            [
                "outputMatrix" =? Matrix4
            ]

    let Matrix4Composition = Type.New()
    let Matrix4CompositionClass =
        ClassWithInitArgs "o3d.Matrix4Composition"
            [
                "inputMatrix", Matrix4
                "localMatrix", Matrix4
            ]
        |=> Matrix4Composition
        |=> Inherits ParamObject
        |+> Protocol
            [
                "outputMatrix" =? Matrix4
            ]

    let Matrix4Scale = Type.New()
    let Matrix4ScaleClass =
        ClassWithInitArgs "o3d.Matrix4Scale"
            [
                "inputMatrix", Matrix4
                "scale", Float3
            ]
        |=> Matrix4Scale
        |=> Inherits ParamObject
        |+> Protocol
            [
                "outputMatrix" =? Matrix4
            ]

    let Matrix4Translation = Type.New()
    let Matrix4TranslationClass =
        ClassWithInitArgs "o3d.Matrix4Translation"
            [
                "inputMatrix", Matrix4
                "translation", Float3
            ]
        |=> Matrix4Translation
        |=> Inherits ParamObject
        |+> Protocol
            [
                "outputMatrix" =? Matrix4
            ]

    let ParamArray = Type.New()
    let ParamArrayClass =
        Class "o3d.ParamArray"
        |=> ParamArray
        |=> Inherits NamedObject
        |+> Protocol
            [
                "length" =? T<int>
                "params" =? Type.ArrayOf Param
            ]

    let ParamOf = Generic / fun t ->
        Class "Param"
        |=> Inherits Param
        |+> Protocol
            [
                "value" =? t
            ]

    let ParamOp16FloatsToMatrix4 = Type.New()
    let ParamOp16FloatsToMatrix4Class =
        ClassWithInitArgs "ParamOp16FloatsToMatrix4"
            [
                "input0", T<float>
                "input1", T<float>
                "input2", T<float>
                "input3", T<float>
                "input4", T<float>
                "input5", T<float>
                "input6", T<float>
                "input7", T<float>
                "input8", T<float>
                "input9", T<float>
                "input10", T<float>
                "input11", T<float>
                "input12", T<float>
                "input13", T<float>
                "input14", T<float>
                "input15", T<float>
            ]
        |=> ParamOp16FloatsToMatrix4
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? Matrix4
            ]

    let ParamOp2FloatsToFloat2 = Type.New()
    let ParamOp2FloatsToFloat2Class =
        ClassWithInitArgs "ParamOp2FloatsToFloat2"
            [
                "input0", T<float>
                "input1", T<float>
            ]
        |=> ParamOp2FloatsToFloat2
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? Float2
            ]

    let ParamOp3FloatsToFloat3 = Type.New()
    let ParamOp3FloatsToFloat3Class =
        ClassWithInitArgs "ParamOp3FloatsToFloat3"
            [
                "input0", T<float>
                "input1", T<float>
                "input2", T<float>
            ]
        |=> ParamOp3FloatsToFloat3
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? Float3
            ]

    let ParamOp4FloatsToFloat4 = Type.New()
    let ParamOp4FloatsToFloat4Class =
        ClassWithInitArgs "ParamOp4FloatsToFloat4"
            [
                "input0", T<float>
                "input1", T<float>
                "input2", T<float>
                "input3", T<float>
            ]
        |=> ParamOp4FloatsToFloat4
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? Float4
            ]

    let StreamBank = Type.New()
    let StreamBankClass =
        Class "StreamBank"
        |=> StreamBank
        |+> Protocol
            [
                "getVertexStream" => Stream_Semantic * T<int> ^-> Stream
                "removeVertexStream" => Stream_Semantic * T<int> ^-> T<bool>
                "setVertexStream" => Stream_Semantic * T<int> * Field * T<int> ^-> T<bool>
                "vertexStreams" => Type.ArrayOf Stream
            ]

    let Primitive_PrimitiveType = Type.New()
    let Primitive_PrimitiveTypeClass =
        Pattern.EnumStrings "Primitive_PrimitiveType" [
            "POINTLIST"
            "LINELIST"
            "LINESTRIP"
            "TRIANGLELIST"
            "TRIANGLESTRIP"
            "TRIANGLEFAN"
        ]
        |=> Primitive_PrimitiveType

    let Primitive = Type.New()
    let PrimitiveClass =
        ClassWithInitArgs "Primitive"
            [
                "indexBuffer", IndexBuffer
                "numberPrimitives", T<int>
                "numberVertices", T<int>
                "primitiveType", Primitive_PrimitiveType
                "startIndex", T<int>
                "streamBank", StreamBank
            ]
        |=> Primitive
        |=> Inherits Element
        |=> Nested [Primitive_PrimitiveTypeClass]

    let ProjectionParamMatrix4 = Type.New()
    let ProjectionParamMatrix4Class =
        Class "ProjectionParamMatrix4"
        |=> ProjectionParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ProjectionInverseParamMatrix4 = Type.New()
    let ProjectionInverseParamMatrix4Class =
        Class "ProjectionInverseParamMatrix4"
        |=> ProjectionInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ProjectionTransposeParamMatrix4 = Type.New()
    let ProjectionTransposeParamMatrix4Class =
        Class "ProjectionTransposeParamMatrix4"
        |=> ProjectionTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ProjectionInverseTransposeParamMatrix4 = Type.New()
    let ProjectionInverseTransposeParamMatrix4Class =
        Class "ProjectionInverseTransposeParamMatrix4"
        |=> ProjectionInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let RenderFrameCounter = Type.New()
    let RenderFrameCounterClass =
        Class "RenderFrameCounter"
        |=> RenderFrameCounter
        |=> Inherits Counter

    let RenderSurfaceSet = Type.New()
    let RenderSurfaceSetClass =
        ClassWithInitArgs "RenderSurfaceSet"
            [
                "renderDepthStencilSurface", RenderDepthStencilSurface
                "renderSurface", RenderSurface
            ]
        |=> RenderSurfaceSet
        |=> Inherits RenderNode

    let Sampler_AddressMode = Type.New()
    let Sampler_AddressModeClass =
        Pattern.EnumStrings "Sampler_AddressMode"
            [
                "WRAP"
                "MIRROR"
                "CLAMP"
                "BORDER"
            ]
        |=> Sampler_AddressMode

    let Sampler_FilterType = Type.New()
    let Sampler_FilterTypeClass =
        Pattern.EnumStrings "Sampler_FilterType"
            [
                "NONE"
                "POINT"
                "LINEAR"
                "ANISOTROPIC"
            ]
        |=> Sampler_FilterType

    let Sampler = Type.New()
    let SamplerClass =
        ClassWithInitArgs "Sampler"
            [
                "addressModeU", Sampler_AddressMode
                "addressModeV", Sampler_AddressMode
                "addressModeW", Sampler_AddressMode
                "borderColor", Float4
                "magFilter", Sampler_FilterType
                "maxAnisotropy", T<int>
                "minFilter", Sampler_FilterType
                "mipFilter", Sampler_FilterType
                "texture", Texture
            ]
        |=> Sampler
        |=> Inherits ParamObject
        |=> Nested [Sampler_FilterTypeClass; Sampler_AddressModeClass]

    let SecondCounter = Type.New()
    let SecondCounterClass =
        Class "SecondCounter"
        |=> SecondCounter
        |=> Inherits Counter

    let Skin = Type.New()
    let SkinClass =
        ClassWithInitArgs "Skin"
            [
                "influences", Type.ArrayOf (Type.ArrayOf T<int>)
                "inverseBindPoseMatrices", Type.ArrayOf Matrix4
            ]
        |=> Skin
        |=> Inherits NamedObject
        |+> Protocol
            [
                "getVertexInfluences" => T<int> ^-> Type.ArrayOf T<int>
                "set" => RawData * T<int>?offset * T<int>?length ^-> T<bool>
                "set" => RawData * T<int>?offset ^-> T<bool>
                "set" => RawData ^-> T<bool>
                "setInverseBindPoseMatrix" => T<int> * Matrix4 ^-> T<unit>
                "setVertexInfluences" => T<int> * Type.ArrayOf T<int> ^-> T<unit>
            ]

    let VertexSource = Type.New()
    let VertexSourceClass =
        Class "VertexSource"
        |=> VertexSource
        |=> Inherits ParamObject
        |+> Protocol
            [
                "bindStream" => VertexSource * Stream_Semantic * T<int> ^-> T<bool>
                "unbindStream" => Stream_Semantic * T<int> ^-> T<bool>
            ]

    let SkinEval = Type.New()
    let SkinEvalClass =
        ClassWithInitArgs "SkinEval"
            [
                "base", Matrix4
                "matrices", ParamArray
                "skin", Skin
            ]
        |=> SkinEval
        |=> Inherits VertexSource
        |+> Protocol
            [
                "getVertexStream" => Stream_Semantic * T<int> ^-> Stream
                "removeVertexStream" => Stream_Semantic * T<int> ^-> T<bool>
                "setVertexStream" => Stream_Semantic * T<int> * Field * T<int> ^-> T<bool>
                "vertexStreams" =? Type.ArrayOf Stream
            ]

    let VertexBufferBase = Type.New()
    let VertexBufferBaseClass =
        Class "VertexBufferBase"
        |=> VertexBufferBase
        |=> Inherits Buffer
        |+> Protocol
            [
                "get" => T<unit> ^-> Type.ArrayOf T<float>
                "getAt" => T<int>?start * T<int>?count ^-> Type.ArrayOf T<float>
                "set" => Type.ArrayOf T<float> ^-> T<bool>
                "setAt" => T<int> * Type.ArrayOf T<float> ^-> T<unit>
            ]

    let SourceBuffer = Type.New()
    let SourceBufferClass =
        Class "SourceBuffer"
        |=> SourceBuffer
        |=> Inherits VertexBufferBase

    let StateSet = Type.New()
    let StateSetClass =
        ClassWithInitArgs "StateSet"
            [
                "state", State
            ]
        |=> StateSet
        |=> Inherits RenderNode

    let TickCounter = Type.New()
    let TickCounterClass =
        Class "TickCounter"
        |=> TickCounter
        |=> Inherits Counter

    let TreeTraversal = Type.New()
    let TreeTraversalClass =
        ClassWithInitArgs "TreeTraversal"
            [
                "transform", Transform
            ]
        |=> TreeTraversal
        |=> Inherits RenderNode
        |+> Protocol
            [
                "registerDrawList" => DrawList * DrawContext * T<bool> ^-> T<unit>
                "unregisterDrawList" => DrawList ^-> T<bool>
            ]

    let TRSToMatrix4 = Type.New()
    let TRSToMatrix4Class =
        ClassWithInitArgs "TRSToMatrix4"
            [
                "rotateX", T<float>
                "rotateY", T<float>
                "rotateZ", T<float>
                "scaleX", T<float>
                "scaleY", T<float>
                "scaleZ", T<float>
                "translateX", T<float>
                "translateY", T<float>
                "translateZ", T<float>
            ]
        |=> TRSToMatrix4
        |=> Inherits ParamObject
        |+> Protocol
            [
                "output" =? Matrix4
            ]

    let VertexBuffer = Type.New()
    let VertexBufferClass =
        Class "VertexBuffer"
        |=> VertexBuffer
        |=> Inherits VertexBufferBase

    let ViewParamMatrix4 = Type.New()
    let ViewParamMatrix4Class =
        Class "ViewParamMatrix4"
        |=> ViewParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewInverseParamMatrix4 = Type.New()
    let ViewInverseParamMatrix4Class =
        Class "ViewInverseParamMatrix4"
        |=> ViewInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewTransposeParamMatrix4 = Type.New()
    let ViewTransposeParamMatrix4Class =
        Class "ViewTransposeParamMatrix4"
        |=> ViewTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewInverseTransposeParamMatrix4 = Type.New()
    let ViewInverseTransposeParamMatrix4Class =
        Class "ViewInverseTransposeParamMatrix4"
        |=> ViewInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewProjectionParamMatrix4 = Type.New()
    let ViewProjectionParamMatrix4Class =
        Class "ViewProjectionParamMatrix4"
        |=> ViewProjectionParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewProjectionInverseParamMatrix4 = Type.New()
    let ViewProjectionInverseParamMatrix4Class =
        Class "ViewProjectionInverseParamMatrix4"
        |=> ViewProjectionInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewProjectionTransposeParamMatrix4 = Type.New()
    let ViewProjectionTransposeParamMatrix4Class =
        Class "ViewProjectionTransposeParamMatrix4"
        |=> ViewProjectionTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let ViewProjectionInverseTransposeParamMatrix4 = Type.New()
    let ViewProjectionInverseTransposeParamMatrix4Class =
        Class "ViewProjectionInverseTransposeParamMatrix4"
        |=> ViewProjectionInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let Viewport = Type.New()
    let ViewportClass =
        ClassWithInitArgs "Viewport"
            [
                "depthRange", Float2
            ]
        |=> Viewport
        |=> Inherits RenderNode
        |+> Protocol
            [
                "ViewPort" =@ Float4
                |> WithSourceName "viewport"
            ]

    let WorldViewParamMatrix4 = Type.New()
    let WorldViewParamMatrix4Class =
        Class "WorldViewParamMatrix4"
        |=> WorldViewParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewInverseParamMatrix4 = Type.New()
    let WorldViewInverseParamMatrix4Class =
        Class "WorldViewInverseParamMatrix4"
        |=> WorldViewInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewTransposeParamMatrix4 = Type.New()
    let WorldViewTransposeParamMatrix4Class =
        Class "WorldViewTransposeParamMatrix4"
        |=> WorldViewTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewInverseTransposeParamMatrix4 = Type.New()
    let WorldViewInverseTransposeParamMatrix4Class =
        Class "WorldViewInverseTransposeParamMatrix4"
        |=> WorldViewInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewProjectionParamMatrix4 = Type.New()
    let WorldViewProjectionParamMatrix4Class =
        Class "WorldViewProjectionParamMatrix4"
        |=> WorldViewProjectionParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewProjectionInverseParamMatrix4 = Type.New()
    let WorldViewProjectionInverseParamMatrix4Class =
        Class "WorldViewProjectionInverseParamMatrix4"
        |=> WorldViewProjectionInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewProjectionTransposeParamMatrix4 = Type.New()
    let WorldViewProjectionTransposeParamMatrix4Class =
        Class "WorldViewProjectionTransposeParamMatrix4"
        |=> WorldViewProjectionTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldViewProjectionInverseTransposeParamMatrix4 = Type.New()
    let WorldViewProjectionInverseTransposeParamMatrix4Class =
        Class "WorldViewProjectionInverseTransposeParamMatrix4"
        |=> WorldViewProjectionInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldParamMatrix4 = Type.New()
    let WorldParamMatrix4Class =
        Class "WorldParamMatrix4"
        |=> WorldParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldInverseParamMatrix4 = Type.New()
    let WorldInverseParamMatrix4Class =
        Class "WorldInverseParamMatrix4"
        |=> WorldInverseParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldTransposeParamMatrix4 = Type.New()
    let WorldTransposeParamMatrix4Class =
        Class "WorldTransposeParamMatrix4"
        |=> WorldTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let WorldInverseTransposeParamMatrix4 = Type.New()
    let WorldInverseTransposeParamMatrix4Class =
        Class "WorldInverseTransposeParamMatrix4"
        |=> WorldInverseTransposeParamMatrix4
        |=> Inherits (ParamOf Matrix4)

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.O3D" [
                ArchiveRequestClass
                BezierCurveKeyClass
                BitmapClass
                BufferClass
                CanvasClass
                CanvasFontMetricsClass
                CanvasLinearGradientClass
                CanvasPaintClass
                CanvasShaderClass
                ClearBufferClass
                ClientClass
                ClientInfoClass
                CounterClass
                CurveClass
                CurveKeyClass
                DisplayModeClass
                DrawContextClass
                DrawElementClass
                DrawListClass
                DrawPassClass
                EffectClass
                EffectParameterInfoClass
                EffectStreamInfoClass
                ElementClass
                EventClass
                FieldClass
                FileRequestClass
                FloatFieldClass
                FunctionClass
                FunctionEvalClass
                IndexBufferClass
                LinearCurveKeyClass
                MaterialClass
                Matrix4AxisRotationClass
                Matrix4CompositionClass
                Matrix4ScaleClass
                Matrix4TranslationClass
                NamedObjectClass
                NamedObjectBaseClass
                ObjectBaseClass
                PackClass
                ParamClass
                ParamArrayClass
                ParamObjectClass
                ParamOp16FloatsToMatrix4Class
                ParamOp2FloatsToFloat2Class
                ParamOp3FloatsToFloat3Class
                ParamOp4FloatsToFloat4Class
                Generic - ParamOf
                PrimitiveClass
                ProjectionInverseParamMatrix4Class
                ProjectionInverseTransposeParamMatrix4Class
                ProjectionParamMatrix4Class
                ProjectionTransposeParamMatrix4Class
                RawDataClass
                RayIntersectionInfoClass
                RenderDepthStencilSurfaceClass
                RendererClass
                RenderEventClass
                RenderFrameCounterClass
                RenderNodeClass
                RenderSurfaceClass
                RenderSurfaceBaseClass
                RenderSurfaceSetClass
                SamplerClass
                SecondCounterClass
                ShapeClass
                SkinClass
                SkinEvalClass
                SourceBufferClass
                StateClass
                StateSetClass
                StepCurveKeyClass
                StreamClass
                StreamBankClass
                TextureClass
                Texture2DClass
                TextureCUBEClass
                TickCounterClass
                TickEventClass
                TransformClass
                TreeTraversalClass
                TRSToMatrix4Class
                UByteNFieldClass
                UInt32FieldClass
                VertexBufferClass
                VertexBufferBaseClass
                VertexSourceClass
                ViewInverseParamMatrix4Class
                ViewInverseTransposeParamMatrix4Class
                ViewParamMatrix4Class
                ViewportClass
                ViewProjectionInverseParamMatrix4Class
                ViewProjectionInverseTransposeParamMatrix4Class
                ViewProjectionParamMatrix4Class
                ViewProjectionTransposeParamMatrix4Class
                ViewTransposeParamMatrix4Class
                WorldInverseParamMatrix4Class
                WorldInverseTransposeParamMatrix4Class
                WorldParamMatrix4Class
                WorldTransposeParamMatrix4Class
                WorldViewInverseParamMatrix4Class
                WorldViewInverseTransposeParamMatrix4Class
                WorldViewParamMatrix4Class
                WorldViewProjectionInverseParamMatrix4Class
                WorldViewProjectionInverseTransposeParamMatrix4Class
                WorldViewProjectionParamMatrix4Class
                WorldViewProjectionTransposeParamMatrix4Class
                WorldViewTransposeParamMatrix4Class
            ]
        ]
