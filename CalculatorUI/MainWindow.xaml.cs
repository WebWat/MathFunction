using FunctionParser;
using System;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

namespace CalculatorUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void TextChanged(object sender, TextChangedEventArgs e)
        {
            try
            {
                if (!string.IsNullOrEmpty(text.Text))
                {
                    Graph.FunctionX = new FunctionX(text.Text);
                    Graph.Update();
                }
            }
            catch (Exception)
            {

            }
        }

        private void UpdateDotsCount_Click(object sender, RoutedEventArgs e)
        {
            Graph.DotsCount = int.Parse(DotsCount.Text);
            Graph.Update();
        }
    }
}
