using D=System.Drawing;
using System.Linq;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Windows.Forms;
using SharpRay.Core.Basic;
using Microsoft.FSharp.Core;

namespace RayTracer
{
    public class RayTracer
    {
        private ScreenDimensions screen;

        public Action<int, int, System.Drawing.Color> setPixel;

        public RayTracer(int screenWidth, int screenHeight, Action<int, int, System.Drawing.Color> setPixel)
        {
            this.screen = new ScreenDimensions(screenWidth, screenHeight);
            this.setPixel = setPixel;
        }

        internal void Render(Scene scene)
        {
            for (int y = 0; y < screen.Height; y++)
            {
                for (int x = 0; x < screen.Width; x++)
                {
                    var color = Tracer.traceRay(new Ray(scene.Camera.Pos, screen.GetPoint(x, y, scene.Camera)), scene, 0);
                    setPixel(x, y, Col.toDrawingColor(color));
                }
            }
        }

        internal readonly Scene DefaultScene =
            new Scene(new SceneObject[] { 
                                new Plane(new Vector(0, 1, 0), 0, Surfaces.checkerboard),
                                new Sphere(new Vector(0, 1, 0), 1, Surfaces.shiny),
                                new Sphere(new Vector(-1, 0.5, 1.5), 0.5, Surfaces.shiny)
                        },
                         new Light[] { 
                                new Light(new Vector(-2,2.5,0), new Color(.49,.07,.07)),
                                new Light(new Vector(1.5,2.5,1.5), new Color(.07,.07,.49)),
                                new Light(new Vector(1.5,2.5,-1.5), new Color(.07,.49,.071)),
                                new Light(new Vector(0,3.5,0), new Color(.21,.21,.35))
                        },
                     Cam.create(new Vector(3, 2, 4), new Vector(-1, .5, 0)));
    }
}
