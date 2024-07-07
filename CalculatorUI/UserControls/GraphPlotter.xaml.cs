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

        public int DotsCount { get; set; } = 500;
        
        public double Scale { get; set; } = 10;

        private Point _lastPoint;


        public GraphPlotter()
        {
            InitializeComponent();
        }


        public void UpdateGraph()
        {
            if (FunctionX == null) return;

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
                double value = FunctionX.Calc(currentX);

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
            
            Path.Data = geometry;
            Path.Stroke = Brushes.Red;
            Path.StrokeThickness = 4;
        }

        private double GetGridX(double position)
            => position * ActualWidth / (Scale * 2);

        private double GetGridY(double y)
        {
            var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
            var position = y + updatedScale / 2 - _centerY;
            return ActualHeight - position * ActualHeight / updatedScale;
        }

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
            var minX = _centerX - Scale;
            var maxX = _centerX + Scale;

            if (minX < 0 && maxX > 0)
            {
                _centerGridY = GetGridX(-minX);

            }
            else if (maxX < 0 || minX > 0)
            {
                _centerGridY = 0;
            }
            else
            {
                throw new Exception("lol");
            }

            UpdateAxis();
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

                _centerX -= offsetX * 2 * Scale / ActualWidth;

                var updatedScale = Scale * 2 * ActualHeight / ActualWidth;
                _centerY += offsetY * updatedScale / ActualHeight;

                _lastPoint = point;

                Coords.Content = $"x: {_centerX:f3}; y: {_centerY:f3}";

                UpdateAxis();
                UpdateGraph();
            }
        }

        private void PreviewMouseUp(object sender, MouseButtonEventArgs e)
        {
            _lastPoint = default;
        }

        private void UserControl_Loaded(object sender, RoutedEventArgs e)
        {
            _centerGridX = ActualHeight / 2 + 1;

            AxisX.X1 = 0;
            AxisX.Y1 = _centerGridX;
            AxisX.X2 = ActualWidth;
            AxisX.Y2 = _centerGridX;

            ViewX.X1 = 0;
            ViewX.Y1 = _centerGridX;
            ViewX.X2 = ActualWidth;
            ViewX.Y2 = _centerGridX;

            _centerGridY = ActualWidth / 2 + 1;

            AxisY.X1 = _centerGridY;
            AxisY.Y1 = 0;
            AxisY.X2 = _centerGridY;
            AxisY.Y2 = ActualHeight;

            ViewY.X1 = _centerGridY;
            ViewY.Y1 = 0;
            ViewY.X2 = _centerGridY;
            ViewY.Y2 = ActualHeight;
        }

        private void UpdateAxis()
        {
            AxisX.Y1 = _centerGridX;
            AxisX.X2 = ActualWidth;
            AxisX.Y2 = _centerGridX;

            AxisY.X1 = _centerGridY;
            AxisY.X2 = _centerGridY;
            AxisY.Y2 = ActualHeight;
            AxisY.Opacity = _centerGridY > 0 ? 1 : 0;
        }
    }
}
