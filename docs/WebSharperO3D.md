# Overview

This WebSharper™ Extension provides a set of classes and functions almost
identical to the ones documented in the [O3D API][o3d-api]. When used
in WebSharper™ projects, these stub classes delegate the work to the
actual classes implemented by the browser.

After adding the reference to the project all the classes can be found
under the `IntelliFactory.WebSharper.O3D` module.

The O3D WebSharper Extension is as far as possible a one-to-one
mapping of O3D. Therefore, a description of the O3D pipeline and
the structure of an O3D program are beyond the scope of this manual.
We recommend to have a look at online resources such as
[the O3D developer's guide][o3d-devguide] for such information. Instead, we
present here the differences and subtleties between the extension
and the JavaScript API.

O3D exists in two forms: a plugin for Google Chrome, and a JavaScript API
on top of WebGL. The Chrome plugin has been deprecated by Google in May 2010;
therefore, although the WebSharper™ extension includes complete functionality
for both backends, this documentation assumes that WebGL is used.

# Differences with JavaScript

## Initialization

The standard distribution of O3DJS requires you to use the function
`o3djs.require` to include the functionality you need, and to set
the `o3djs.base.o3d` object. This extension, however, includes a
minified version of the complete O3D and O3DJS source and sets
`o3djs.base.o3d`. Therefore, the only initialization code needed
is the following:

```fsharp
[<JavaScript>]
let O3dDisplay() =
    Div [Attr.Id "o3d"; Attr.Style "width: 600px; height: 600px;"]
    |>! OnAfterRender (fun d ->
        O3DJS.Webgl.MakeClients (fun clients ->
            let client = clients.[0].Client
            // O3D is now fully initialized, you can load your resources
        )
    )
```

## Mathematics

O3D manipulates vectors and quaternions as arrays of numbers, and
matrices as arrays of arrays. This implies that it uses the same
functions to manipulate all data sizes. This allows for some
hard-to-debug errors such as taking the dot-product of two vectors of
different sizes.

Since WebSharper translates F# tuples as JavaScript arrays, we add
some extra safety by manipulating mathematical data as tuples. All
functions in the module `O3DJS.Math` are overloaded to accept tuples
as arguments. They also have overloads accepting arrays, if you need
to work without this added safety, for example if you need to multiply
arbitrary-sized matrices.

Moreover, we also use F#'s support for function overloading to shorten
function names. Indeed, contrary to JavaScript, there is no need to
distinguish between eg. `math.addVector` and `math.addMatrix`;
therefore both are merged in a single `Math.Add` function with
appropriate overloads.

As an example, the following code from [the Pool sample][o3d-pool]:

```javascript
var Vr = o3djs.math.subVector(
             o3djs.math.addVector(o3djs.math.cross(w2, r2), v2),
             o3djs.math.addVector(o3djs.math.cross(w1, r1), v1));
var Vrn = o3djs.math.mulScalarVector(o3djs.math.dot(Vn, N), N);
var Vrt = o3djs.math.subVector(Vr, Vrn);
```

is translated into WebSharper as:

```fsharp
let Vr = O3DJS.Math.Sub(
             O3DJS.Math.Add(O3DJS.Math.Cross(w2, r2), v2),
             O3DJS.Math.Add(O3DJS.Math.Cross(w1, r1), v1))
let Vrn = O3DJS.Math.Mul(O3DJS.Math.Dot(Vn, N), N)
let Vrt = O3DJS.Math.Sub(Vr, Vrn)
```

## Parameters

In O3D, the methods `Pack.createObject`, `Pack.getObjects`,
`State.getStateParam`, `Curve.createKey`, `Buffer.createField` and
`ParamObject.createParam` return an object whose type depends on the
argument passed. This is not suitable for use in F#. Therefore, these
methods have been translated into series of individual methods, each
of which handles one of the possible return types.

For example, the following JavaScript snippet:

```javascript
var transform = pack.createObject('Transform');
transform.parent = root;
var myParam = transform.createParam('objectCenter', 'ParamFloat2');
```

translates into:

```fsharp
let transform = pack.CreateTransform()
transform.Parent <- root
let myParam = transform.CreateParamFloat2 "objectCenter"
```

In JavaScript, the type of `myParam` is `ParamFloat2`. In WebSharper, we
use a parameterized type: the type of `myParam` is `Param<float * float>`.

Note also that you can use property assignment to shorten the construction
of `transform` into a single line:

```fsharp
let transform = pack.CreateTransform(Parent = root)
let myParam = transform.CreateParamFloat2 "objectCenter"
```

[o3d-pool]: http://code.google.com/p/o3d/source/browse/trunk/samples_webgl/o3d-webgl-samples/pool.html
[o3d-api]: http://code.google.com/apis/o3d/docs/api_reference.html
[o3d-devguide]: http://code.google.com/apis/o3d/docs/devguideintro.html
