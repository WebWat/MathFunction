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
        public FunctionX FunctionX;
        private double _centerX = 0;
        private double _centerGridX;
        private double _centerY = 0;
        private double _centerGridY;

        public int DotsCount { get; set; } = 1000;
        
        public double Scale { get; set; } = 10;

        private Point _lastPoint;


        public GraphPlotter()
        {
            InitializeComponent();
        }


        public void UpdateGraph()
        {
            if (FunctionX == null) return;

            MainGrid.Children.Clear();

            var path = new Path();
            var geometry = new PathGeometry();

            var list = new List<PathSegment>();
            var minX = _centerX - Scale;
            var maxX = _centerX + Scale;
            var step = (maxX - minX) / DotsCount;
            var updatedScale = Scale * ActualHeight / ActualWidth;
            Predicate<double> inLimit = val => val <= _centerY + updatedScale && val >= _centerY - updatedScale;

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

                    if (list.Count != 0) list.Clear();
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
            path.StrokeThickness = 3;
            MainGrid.Children.Add(path);
        }

        private double GetGridX(double position)
            => position * ActualWidth / (Scale * 2) - 1;

        private double GetGridY(double y)
        {
            var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
            var position = y + updatedScale / 2 - _centerY;
            return ActualHeight - position * ActualHeight / updatedScale;
        }

        // need autoscale
        private void ScaleChanged(object sender, MouseWheelEventArgs e)
        {
            if (e.Delta > 0)
            {
                Scale -= Scale * 0.1;
            }
            else
            {
                Scale += Scale * 0.1;
            }

            _centerGridX = GetGridY(0);
            AxisX.X1 = 0;
            AxisX.Y1 = _centerGridX;
            AxisX.X2 = ActualWidth;
            AxisX.Y2 = _centerGridX;

            _centerGridY = GetGridX(_centerX);
            AxisY.X1 = _centerGridY;
            AxisY.Y1 = 0;
            AxisY.X2 = _centerGridY;
            AxisY.Y2 = ActualHeight;
            AxisY.Opacity = _centerGridY > 0 ? 1 : 0;
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

                var offsetX = point.X - _lastPoint.X;
                var offsetY = point.Y - _lastPoint.Y;

                _centerGridX += offsetY;
                _centerGridY += offsetX;

                AxisX.X1 = 0;
                AxisX.Y1 = _centerGridX;
                AxisX.X2 = ActualWidth;
                AxisX.Y2 = _centerGridX;

                AxisY.X1 = _centerGridY;
                AxisY.Y1 = 0;
                AxisY.X2 = _centerGridY;
                AxisY.Y2 = ActualHeight;
                AxisY.Opacity = _centerGridY > 0 ? 1 : 0;

                _centerX -= offsetX * 2 * Scale / ActualWidth;

                var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
                _centerY += offsetY * updatedScale / ActualHeight;

                _lastPoint = point;

                Debug.WriteLine(_centerX + " " + _centerY);

                UpdateGraph();
            }
        }

        private void PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            _lastPoint = default;
        }

        private void UserControl_Loaded(object sender, RoutedEventArgs e)
        {
            _centerGridX = ActualHeight / 2;

            AxisX.X1 = 0;
            AxisX.Y1 = _centerGridX;
            AxisX.X2 = ActualWidth;
            AxisX.Y2 = _centerGridX;

            _centerGridY = ActualWidth / 2;

            AxisY.X1 = _centerGridY;
            AxisY.Y1 = 0;
            AxisY.X2 = _centerGridY;
            AxisY.Y2 = ActualHeight;
        }
    }
}
