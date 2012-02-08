using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using RayTracer;

namespace SharpRay.UI
{
    public partial class RayTracerForm : Form
    {
        Bitmap bitmap;
        PictureBox pictureBox;
        const int width = 600;
        const int height = 600;

        public RayTracerForm()
        {
            bitmap = new Bitmap(width, height);

            pictureBox = new PictureBox();
            pictureBox.Dock = DockStyle.Fill;
            pictureBox.SizeMode = PictureBoxSizeMode.StretchImage;
            pictureBox.Image = bitmap;

            ClientSize = new System.Drawing.Size(width, height + 24);
            Controls.Add(pictureBox);
            Text = "Ray Tracer";
            Load += RayTracerForm_Load;

            Show();
        }

        private void RayTracerForm_Load(object sender, EventArgs e)
        {
            this.Show();
            var rayTracer = new RayTracer.RayTracer(width, height, (int x, int y, System.Drawing.Color color) =>
            {
                bitmap.SetPixel(x, y, color);
                if (x == 0) pictureBox.Refresh();
            });
            rayTracer.Render(rayTracer.DefaultScene);
            pictureBox.Invalidate();

        }
    }
}
