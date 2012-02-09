namespace SharpRay.Core

type Ray(start: Vector, dir: Vector) =
    member r.Start = start
    member r.Dir = dir

type Surface =
    abstract Diffuse : Vector -> Color
    abstract Specular : Vector -> Color
    abstract Reflect : Vector -> float
    abstract Roughness : float

module Surfaces =
    let shiny = { new Surface with
        member x.Diffuse v = Color.White
        member x.Specular v = Color(0.5, 0.5, 0.5)
        member x.Reflect v = 0.5
        member x.Roughness = 50.0
    }

    let checkerboard = { new Surface with
        member x.Diffuse p = if (floor p.Z + floor p.X) % 2.0 <> 0.0 then Color.White else Color.Black
        member x.Reflect p = if (floor p.Z + floor p.X) % 2.0 <> 0.0 then 0.1 else 0.7
        member x.Specular _ = Color.White
        member x.Roughness = 150.0
    }

type Light(pos: Vector, color: Color) =
    member x.Pos = pos
    member x.Color = color

type Camera(pos: Vector, forward: Vector, up: Vector, right: Vector) =
    member x.Pos = pos
    member x.Forward = forward
    member x.Up = up
    member x.Right = right

module Cam =
    let create (pos: Vector) (lookAt: Vector) =
        let forward = Vec.normalize(lookAt - pos)
        let down = Vector(0.0, -1.0, 0.0)
        let right = 1.5 * Vec.normalize(Vec.cross forward down)
        let up = 1.5 * Vec.normalize(Vec.cross forward right)

        Camera(pos, forward, up, right)

type SceneObject =
    abstract Surface : Surface
    abstract Intersect : Intersect : Ray -> Intersection option
    abstract Normal : Vector -> Vector

and Intersection(thing: SceneObject, ray: Ray, dist: float) =
    member x.Thing = thing
    member x.Ray = ray
    member x.Dist = dist

type Plane(normal: Vector, offset: float, surface: Surface) =
    interface SceneObject with
        member x.Surface = surface
        member x.Normal _ = normal
        member x.Intersect ray =
            let denom = Vec.dot normal ray.Dir
            if denom > 0.0
                then None
                else Some(Intersection(x, ray, ((Vec.dot normal ray.Start) + offset) / -denom))

type Sphere(center: Vector, radius: float, surface: Surface) =
    interface SceneObject with
        member x.Surface = surface
        member x.Normal p = Vec.normalize(p - center)
        member x.Intersect ray =
            let eo = center - ray.Start
            let v = Vec.dot eo ray.Dir
            if v < 0.0 
                then None
                else
                    let disc = radius**2.0 - ((Vec.dot eo eo) - v**2.0)
                    let dist = if disc < 0.0 then 0.0 else v - sqrt disc
                    if dist = 0.0 then None else Some (Intersection(x, ray, dist))

type Scene(things: SceneObject array, lights: Light array, camera: Camera) =
    member x.Things = things
    member x.Lights = lights
    member x.Camera = camera
    member x.BackgroundColor = Color.Black

type ScreenDimensions(width: int, height: int) =
    let recenterX (x: float) = (x - (float width / 2.0)) / (2.0 * float width)
    let recenterY (y: float) = -(y - (float height / 2.0)) / (2.0 * float height)

    member s.Width = width
    member s.Height = height
    member s.GetPoint (x: float) (y: float) (camera: Camera) = Vec.normalize(camera.Forward + recenterX x*camera.Right + recenterY y*camera.Up)

module Tracer =
    let MaxDepth = 5

    let inline square x = x*x

    let intersect (ray: Ray) (scene: Scene): Intersection option =
        let isects =
            scene.Things 
            |> Seq.map (fun obj -> obj.Intersect(ray))
            |> Seq.filter (fun obj -> obj.IsSome)
            |> Seq.map (fun obj -> obj.Value)
        if Seq.isEmpty isects then None else Some (Seq.minBy (fun o -> o.Dist) isects)

    let testRay ray scene =
        match intersect ray scene with
        | None   -> 0.0
        | Some x -> x.Dist

    let rec traceRay ray scene depth =
        match intersect ray scene with
        | None   -> scene.BackgroundColor
        | Some x -> shade x scene depth

    and naturalColor (thing: SceneObject) (pos: Vector) (norm: Vector) (rd: Vector) (scene: Scene) =
        let mutable ret = Color.Black
        for light in scene.Lights do
            let ldis = light.Pos - pos
            let livec = Vec.normalize ldis
            let neatIntersection = testRay (Ray(pos, livec)) scene
            let visible = (square neatIntersection > ldis.MagnitudeSquared) || (neatIntersection = 0.0)
            if visible then
                let illum = Vec.dot livec norm
                let lcolor = if illum > 0.0 then illum * light.Color else Color.Black
                let specular = Vec.dot livec (Vec.normalize rd)
                let scolor = if specular > 0.0 then specular**thing.Surface.Roughness * light.Color else Color.Black
                ret <- ret + thing.Surface.Diffuse(pos)*lcolor + thing.Surface.Specular(pos)*scolor
        ret

    and reflectionColor (thing: SceneObject) (pos: Vector) (norm: Vector) (rd: Vector) (scene: Scene) (depth: int) =
        thing.Surface.Reflect(pos) * traceRay (Ray(pos, rd)) scene (depth + 1)

    and shade (isect: Intersection) (scene: Scene) (depth: int) =
        let d = isect.Ray.Dir
        let pos = (isect.Dist * isect.Ray.Dir) + isect.Ray.Start
        let normal = isect.Thing.Normal(pos)
        let reflectDir = d - (2.0 * (Vec.dot normal d) * normal)
        let ret = Color.Black
        let ret = ret + naturalColor isect.Thing pos normal reflectDir scene
        if depth >= MaxDepth
            then ret + Color(0.5, 0.5, 0.5)
            else ret + reflectionColor isect.Thing (pos + 0.001*reflectDir) normal reflectDir scene depth

    let render (screenWidth: int) (screenHeight: int) (setPixel: System.Action<int,int,System.Drawing.Color>) (scene: Scene) =
        let screen = ScreenDimensions(screenWidth, screenHeight)
        
        for y in 0..screenHeight-1 do
            for x in 0..screenWidth-1 do
                let color = traceRay (Ray(scene.Camera.Pos, (screen.GetPoint (float x) (float y) scene.Camera))) scene 0
                setPixel.Invoke(x, y, color.AsDrawingColor)

module Scenes =
    let defaultScene =
        Scene([|
                Plane(new Vector(0.0, 1.0, 0.0), 0.0, Surfaces.checkerboard);
                Sphere(new Vector(0.0, 1.0, 0.0), 1.0, Surfaces.shiny);
                Sphere(new Vector(-1.0, 0.5, 1.5), 0.5, Surfaces.shiny);
              |],
             [|
                Light(Vector(-2.0,2.5,0.0), Color(0.49,0.07,0.07));
                Light(Vector(1.5,2.5,1.5),  Color(0.07,0.07,0.49));
                Light(Vector(1.5,2.5,-1.5), Color(0.07,0.49,0.071));
                Light(Vector(0.0,3.5,0.0),  Color(0.21,0.21,0.35))
             |],
             Cam.create (Vector(3.0, 2.0, 4.0)) (Vector(-1.0, 0.5, 0.0)))
