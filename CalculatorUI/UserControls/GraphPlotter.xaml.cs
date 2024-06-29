using FunctionParser;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
        private double _centerGridX;
        public double CenterY { get; set; } = 0;
        private double _centerGridY;

        public int DotsCount { get; set; } = 1000;
        
        public double Scale { get; set; } = 10;

        private double _diminisher = 1;
        private Point _lastPoint;


        public GraphPlotter()
        {
            InitializeComponent();
        }

        public void UpdateGraph(bool needClear = true)
        {
            if (FunctionX == null) return;

            if (needClear) MainGrid.Children.Clear();

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
            path.StrokeThickness = 2;
            MainGrid.Children.Add(path);
        }

        private double GetGridX(double position)
            => position * ActualWidth / (Scale * 2);

        private double GetGridY(double y)
        {
            var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
            var position = y + updatedScale / 2 - CenterY;
            return ActualHeight - position * ActualHeight / updatedScale;
        }

        // need autoscale
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
                UpdateGraph();
        }

        private void PreviewMouseMove(object sender, MouseEventArgs e)
        {
            if (e.LeftButton == MouseButtonState.Pressed)
            {
                var point = Mouse.GetPosition(MainGrid);

                if (_lastPoint == default)
                {
                    _lastPoint = point;
                    return;
                }

                MainGrid.Children.Clear();

                var offsetX = point.X - _lastPoint.X;
                var offsetY = point.Y - _lastPoint.Y;

                _centerGridX += offsetY;
                _centerGridY += offsetX;

                var lineX = new Line
                {
                    X1 = 0,
                    Y1 = _centerGridX,
                    X2 = ActualWidth,
                    Y2 = _centerGridX,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1.5
                };

                var lineY = new Line
                {
                    X1 = _centerGridY,
                    Y1 = 0,
                    X2 = _centerGridY,
                    Y2 = ActualHeight,
                    Stroke = Brushes.Black,
                    StrokeThickness = 1.5
                };

                MainGrid.Children.Add(lineX);
                MainGrid.Children.Add(lineY);

                _lastPoint = point;
                CenterX -= offsetX * 2 * Scale / ActualWidth;

                var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
                CenterY += offsetY * updatedScale / ActualHeight;

                //Debug.WriteLine("point: " + point.X + " " + point.Y);
                //Debug.WriteLine("center: " + CenterX + " " + CenterY);
                //Debug.WriteLine("offset:" + offsetX + " " + offsetY);

                UpdateGraph(false);
            }
        }

        private void PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            _lastPoint = default;
        }

        private void LoadedEvent(object sender, RoutedEventArgs e)
        {
            _centerGridX = ActualHeight / 2;

            var lineX = new Line
            {
                X1 = 0,
                Y1 = _centerGridX,
                X2 = ActualWidth,
                Y2 = _centerGridX,
                Stroke = Brushes.Black,
                StrokeThickness = 1.5
            };

            _centerGridY = ActualWidth / 2;

            var lineY = new Line
            {
                X1 = _centerGridY,
                Y1 = 0,
                X2 = _centerGridY,
                Y2 = ActualHeight,
                Stroke = Brushes.Black,
                StrokeThickness = 1.5
            };

            MainGrid.Children.Add(lineX);
            MainGrid.Children.Add(lineY);
        }
    }
}
