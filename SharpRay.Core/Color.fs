namespace SharpRay.Core

type Color =
    struct
        val R: float
        val G: float
        val B: float

        new(r, g, b) = { R=r; G=g; B=b }

        member c.AsDrawingColor =
            let trim d = (min d 1.0) * 255.0 |> int
            System.Drawing.Color.FromArgb(trim c.R, trim c.G, trim c.B)

        static member (*) (s: float, c: Color) = Color(s*c.R, s*c.G, s*c.B)
        static member (*) (c: Color, s: float) = Color(s*c.R, s*c.G, s*c.B)
        static member (/) (c: Color, s: float) = c * (1.0/s)
        static member (+) (c1: Color, c2: Color) = Color(c1.R+c2.R, c1.G+c2.G, c1.B+c2.B);
        static member (-) (c1: Color, c2: Color) = Color(c1.R-c2.R, c1.G-c2.G, c1.B-c2.B);
        static member (*) (c1: Color, c2: Color) = Color(c1.R*c2.R, c1.G*c2.G, c1.B*c2.B);

        static member Black = Color(0.0, 0.0, 0.0)
        static member White = Color(1.0, 1.0, 1.0)
    end
