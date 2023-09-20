using MathFunction;
using System.Windows;
using System.Windows.Controls;
using static Node;

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
                if (string.IsNullOrEmpty(text.Text))
                {
                    result.Text = "0,000";
                }
                else
                {
                    var func = new FunctionX(text.Text);
                    result.Text = func.Calc(0).ToString("f3");
                }
            }
            catch (UnknownOperation)
            {
                result.Text = nameof (UnknownOperation);
            }
        }
    }
}
