using CompMath;
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

namespace UI
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
                if (text.Text == "")
                {
                    result.Text = "0";
                }
                else
                {
                    var func = new FunctionX(text.Text);
                   // result.Text = func.Calc(0).ToString("f3");
                }
            }
            catch { }
        }

        private void Lol(object sender, KeyEventArgs e)
        {
            //if (e.Key == Key.Enter)
            //    try
            //    {
            //        if (text.Text == "")
            //        {
            //            result.Text = "0";
            //        }
            //        else
            //        {
            //            var func = new Function(text.Text);
            //            result.Text = func.Calc(0).ToString("f3");
            //        }
            //    }
            //    catch { }
        }
    }
}
