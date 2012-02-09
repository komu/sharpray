namespace SharpRay.Core.Basic
open System

type Vector =
    struct
        val X: float
        val Y: float
        val Z: float

        new(x, y, z) = { X=x; Y=y; Z=z }

        static member (~-) (v: Vector) = Vector(-v.X, -v.Y, -v.Z)
        static member (*) (s: float, v: Vector) = Vector(s*v.X, s*v.Y, s*v.Z)
        static member (*) (v: Vector, s: float) = Vector(s*v.X, s*v.Y, s*v.Z)
        static member (/) (v: Vector, s: float) = v * (1.0/s)
        static member (+) (v: Vector, u: Vector) = Vector(v.X+u.X, v.Y+u.Y, v.Z+u.Z);
        static member (-) (v: Vector, u: Vector) = Vector(v.X-u.X, v.Y-u.Y, v.Z-u.Z);
    end

module Vec =
    let dot   (v: Vector) (u: Vector) = v.X*u.X + v.Y*u.Y + v.Z*u.Z
    let cross (v: Vector) (u: Vector) = Vector(v.Y*u.Z - v.Z*u.Y, v.Z*u.X - v.X*u.Z, v.X*u.Y - v.Y*u.X)

    let magnitudeSquared v = dot v v
    let magnitude v        = magnitudeSquared v |> Math.Sqrt
    let normalize v        = v / magnitude v

type Color =
    struct
        val R: float
        val G: float
        val B: float

        new(r, g, b) = { R=r; G=g; B=b }

        static member (*) (s: float, c: Color) = Color(s*c.R, s*c.G, s*c.B)
        static member (*) (c: Color, s: float) = Color(s*c.R, s*c.G, s*c.B)
        static member (/) (c: Color, s: float) = c * (1.0/s)
        static member (+) (c1: Color, c2: Color) = Color(c1.R+c2.R, c1.G+c2.G, c1.B+c2.B);
        static member (-) (c1: Color, c2: Color) = Color(c1.R-c2.R, c1.G-c2.G, c1.B-c2.B);
        static member (*) (c1: Color, c2: Color) = Color(c1.R*c2.R, c1.G*c2.G, c1.B*c2.B);
    end

module Col =
    let black = Color(0.0, 0.0, 0.0)
    let white = Color(1.0, 1.0, 1.0)
    let background = black

    let toDrawingColor (c: Color) =
        let trim d = Math.Min(d, 1.0) * 255.0 |> int
        System.Drawing.Color.FromArgb(trim c.R, trim c.G, trim c.B)

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
        member x.Diffuse v = Col.white
        member x.Specular v = Color(0.5, 0.5, 0.5)
        member x.Reflect v = 0.5
        member x.Roughness = 50.0
    }

    let checkerboard = { new Surface with
        member x.Diffuse p = if (Math.Floor(p.Z) + Math.Floor(p.X)) % 2.0 <> 0.0 then Col.white else Col.black
        member x.Reflect p = if (Math.Floor(p.Z) + Math.Floor(p.X)) % 2.0 <> 0.0 then 0.1 else 0.7
        member x.Specular _ = Col.white
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

type Sphere(center: Vector, radius: Double, surface: Surface) =
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
                    let dist = if disc < 0.0 then 0.0 else v - Math.Sqrt(disc)
                    if dist = 0.0 then None else Some (Intersection(x, ray, dist))

type Scene(things: SceneObject array, lights: Light array, camera: Camera) =
    member x.Things = things
    member x.Lights = lights
    member x.Camera = camera

type ScreenDimensions(width: int, height: int) =
    let recenterX (x: float) = (x - (float width / 2.0)) / (2.0 * float width)
    let recenterY (y: float) = -(y - (float height / 2.0)) / (2.0 * float height)

    member s.Width = width
    member s.Height = height
    member s.GetPoint (x: float) (y: float) (camera: Camera) = Vec.normalize(camera.Forward + recenterX x*camera.Right + recenterY y*camera.Up)

module Tracer =
    let MaxDepth = 5

    let intersections (ray: Ray) (scene: Scene): Intersection seq = 
        scene.Things 
            |> Seq.map (fun obj -> obj.Intersect(ray))
            |> Seq.filter (fun obj -> obj.IsSome)
            |> Seq.map (fun obj -> obj.Value)
            |> Seq.sortBy (fun obj -> obj.Dist)

    let testRay (ray: Ray) (scene: Scene): float =
        let isects = intersections ray scene
        if Seq.isEmpty isects 
            then 0.0
            else (Seq.head isects).Dist

    let rec traceRay (ray: Ray) (scene: Scene) (depth: int): Color =
        let isects = intersections ray scene
        if Seq.isEmpty isects 
            then Col.background
            else shade (Seq.head isects) scene depth

    and naturalColor (thing: SceneObject) (pos: Vector) (norm: Vector) (rd: Vector) (scene: Scene) =
        let mutable ret = Col.black
        for light in scene.Lights do
            let ldis = light.Pos - pos
            let livec = Vec.normalize ldis
            let neatIntersection = testRay (Ray(pos, livec)) scene
            let visible = (neatIntersection**2.0 > Vec.magnitudeSquared ldis) || (neatIntersection = 0.0)
            if visible then
                let illum = Vec.dot livec norm
                let lcolor = if illum > 0.0 then illum * light.Color else Col.black
                let specular = Vec.dot livec (Vec.normalize rd)
                let scolor = if specular > 0.0 then specular**thing.Surface.Roughness * light.Color else Col.black
                ret <- ret + thing.Surface.Diffuse(pos)*lcolor + thing.Surface.Specular(pos)*scolor
        ret

    and reflectionColor (thing: SceneObject) (pos: Vector) (norm: Vector) (rd: Vector) (scene: Scene) (depth: int) =
        thing.Surface.Reflect(pos) * traceRay (Ray(pos, rd)) scene (depth + 1)

    and shade (isect: Intersection) (scene: Scene) (depth: int) =
        let d = isect.Ray.Dir
        let pos = (isect.Dist * isect.Ray.Dir) + isect.Ray.Start
        let normal = isect.Thing.Normal(pos)
        let reflectDir = d - (2.0 * (Vec.dot normal d) * normal)
        let ret = Col.black
        let ret = ret + naturalColor isect.Thing pos normal reflectDir scene
        if depth >= MaxDepth
            then ret + Color(0.5, 0.5, 0.5)
            else ret + reflectionColor isect.Thing (pos + 0.001*reflectDir) normal reflectDir scene depth

    let render (screenWidth: int) (screenHeight: int) (setPixel: System.Action<int,int,System.Drawing.Color>) (scene: Scene) =
        let screen = ScreenDimensions(screenWidth, screenHeight)
        
        for y in 0..screenHeight-1 do
            for x in 0..screenWidth-1 do
                let color = traceRay (Ray(scene.Camera.Pos, (screen.GetPoint (float x) (float y) scene.Camera))) scene 0
                setPixel.Invoke(x, y, Col.toDrawingColor color)

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
