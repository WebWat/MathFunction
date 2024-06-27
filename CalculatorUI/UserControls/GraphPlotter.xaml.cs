using FunctionParser;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace CalculatorUI.UserControls
{
    /// <summary>
    /// Логика взаимодействия для GraphPlotter.xaml
    /// </summary>
    public partial class GraphPlotter : UserControl
    {
        public FunctionX FunctionX { get; set; }
        public double CenterX { get; set; } = 0;
        public double CenterY { get; set; } = 0;

        public int DotsCount { get; set; } = 5000;
        
        public double Scale { get; set; } = 50;

        private double _diminisher = 1;


        public GraphPlotter()
        {
            InitializeComponent();
        }

        public void Update()
        {
            MainGrid.Children.Clear();

            if (CenterX == 0 && CenterY == 0)
            {
                var centerForX = ActualHeight / 2;

                var lineX = new Line
                {
                    X1 = 0,
                    Y1 = centerForX,
                    X2 = ActualWidth,
                    Y2 = centerForX,
                    Stroke = Brushes.Black,
                };

                var centerForY = ActualWidth / 2;

                var lineY = new Line
                {
                    X1 = centerForY,
                    Y1 = 0,
                    X2 = centerForY,
                    Y2 = ActualHeight,
                    Stroke = Brushes.Black,
                };

                MainGrid.Children.Add(lineX);
                MainGrid.Children.Add(lineY);

                var labelX = new Label();
                var offsetX = ActualWidth - GetGridX(Scale / 2);
                labelX.Margin = new Thickness(offsetX, centerForX, 0, 0);
                labelX.Content = (Scale / 2).ToString();

                var labelY = new Label();
                var offsetY = ActualHeight - GetGridY(-Scale / 2);
                labelY.Margin = new Thickness(centerForY, offsetY, 0, 0);
                labelY.Content = (Scale / 2).ToString();

                MainGrid.Children.Add(labelX);
                MainGrid.Children.Add(labelY);
            }

            var path = new Path();
            var geometry = new PathGeometry();

            var list = new List<PathSegment>();
            var minX = CenterX - Scale;
            var maxX = CenterY + Scale;
            var step = (maxX - minX) / DotsCount;
            //var yLimit = (CenterY + Scale, CenterY - Scale);
            Predicate<double> inLimit = val => val <= CenterY + Scale && val >= CenterY - Scale;

            for (int i = 0; i <= DotsCount; i++)
            {
                var currentX = minX + i * step;
                var value = FunctionX.Calc(currentX);
                if (inLimit(value))
                {
                    list.Add(
                        new LineSegment
                        {
                            Point = new Point 
                            { 
                                X = GetGridX(i * step), 
                                Y = GetGridY(value) 
                            }
                        }
                    );
                }
                else
                {
                    if (list.Count > 1)
                    {
                        var pathFigure = new PathFigure(((LineSegment)list[0]).Point, list[1..], false);

                        geometry.AddGeometry(
                            new PathGeometry([pathFigure])
                        );
                    }
                    list.Clear();
                }
            }

            if (list.Count > 1)
            {
                var pathFigure = new PathFigure(((LineSegment)list[0]).Point, list[1..], false);

                geometry.AddGeometry(
                    new PathGeometry([pathFigure])
                );
            }

            path.Data = geometry;
            path.Stroke = Brushes.Red;
            MainGrid.Children.Add(path);
        }

        private double GetGridX(double position)
            => position * ActualWidth / (Scale * 2);

        private double GetGridY(double y)
        {
            var updatedScale = (Scale * 2 * ActualHeight) / ActualWidth;
            var position = y + updatedScale / 2;
            return ActualHeight - position * ActualHeight / updatedScale;
        }

        private void ScaleChanged(object sender, MouseWheelEventArgs e)
        {
            if (e.Delta > 0)
            {
                Scale -= _diminisher;
            }
            else
            {
                Scale += _diminisher;
            }

            if (Scale > 0)
                Update();
        }
    }
}
