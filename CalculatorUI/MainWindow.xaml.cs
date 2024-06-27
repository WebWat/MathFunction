using FunctionParser;
using System;
using System.Windows;
using System.Windows.Controls;

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
    }
}
