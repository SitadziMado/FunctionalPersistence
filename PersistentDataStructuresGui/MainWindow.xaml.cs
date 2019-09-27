using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
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
using Microsoft.FSharp.Core;
using Persistent;
using QuickGraph;
using static Persistent.BinaryTree;

namespace PersistentDataStructuresGui
{
    /// <summary>
    /// Логика взаимодействия для MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private readonly List<BinaryTree<int>> _versions = new List<BinaryTree<int>>();

        public IBidirectionalGraph<object, IEdge<object>> GraphToVisualize { get; private set; }

        public MainWindow()
        {
            _versions.Add(construct(new[] { 2, 1, 3, 5, 7, 3, 5 }));

            InitializeComponent();
            CreateGraphToVisualize();
        }
        
        private void CreateGraphToVisualize()
        {
            BinaryTreeAdapter adapter;

            if (!InsertButton.IsEnabled)
            {
                try
                {
                    var version = int.Parse(VersionTextBox.Text);
                    adapter = new BinaryTreeAdapter(new[] {_versions[version]});
                }
                catch (ArgumentOutOfRangeException)
                {
                    Inform($"Введите номер версии от {0} до {_versions.Count - 1}", MessageBoxImage.Exclamation);
                    return;
                }
            }
            else
            {
                adapter = new BinaryTreeAdapter(_versions);
            }
            
            var g = new BidirectionalGraph<object, IEdge<object>>();
            
            foreach (var a in adapter.Nodes)
            {
                g.AddVertex(a);
            }
            
            foreach (var a in adapter.Edges)
            {
                g.AddEdge(new Edge<object>(a.Item1, a.Item2));
            }

            GraphLayout.Graph = g;
            GraphToVisualize = g;
        }

        private void Inform(string message, MessageBoxImage icon = MessageBoxImage.Information)
        {
            MessageBox.Show(message, "Информация", MessageBoxButton.OK, icon);
        }

        private void Toggle(UIElement control)
        {
            control.IsEnabled = !control.IsEnabled;
        }

        private void Operate(Action action)
        {
            try
            {
                action();
            }
            catch (FormatException)
            {
                Inform("Неверный формат элемента, введите число", MessageBoxImage.Exclamation);
            }
            catch (ArgumentException)
            {
                Inform("Введенный элемент не был найден", MessageBoxImage.Exclamation);
            }
        }

        private void InsertButton_Click(object sender, RoutedEventArgs e)
        {
            Operate(() =>
            {
                var func = insert(int.Parse(ElementTextBox.Text));
                var ver = func.Invoke(_versions.Last());
                _versions.Add(ver);

                CreateGraphToVisualize();
            });
        }

        private void RemoveButton_Click(object sender, RoutedEventArgs e)
        {
            Operate(() =>
            {
                var func = remove(int.Parse(ElementTextBox.Text));
                var ver = func.Invoke(_versions.Last());
                _versions.Add(ver);

                CreateGraphToVisualize();
            });
        }

        private void FindButton_Click(object sender, RoutedEventArgs e)
        {
            Operate(() =>
            {
                var elem = int.Parse(ElementTextBox.Text);
                var version = int.Parse(VersionTextBox.Text);
                var found = contains(elem, _versions.Last());

                if (found)
                {
                    Inform($"Элемент {elem} найден в версии {version}");
                }
                else
                {
                    Inform($"Элемент {elem} не найден в версии {version}");
                }
            });
        }
        
        private void ShowOnlyToggle_Checked(object sender, RoutedEventArgs e)
        {
            var version = int.Parse(VersionTextBox.Text);

            if (InsertButton.IsEnabled)
            {
                if (version < 0 || version >= _versions.Count)
                {
                    Inform($"Введите номер версии от {0} до {_versions.Count - 1}", MessageBoxImage.Exclamation);
                    return;
                }

                ShowOnlyTextBlock.Text = "Показать все\nверсии";
            }
            else
            {
                ShowOnlyTextBlock.Text = "Показать одну\nверсию";
            }

            Toggle(InsertButton);
            Toggle(RemoveButton);
            Toggle(FindButton);
            Toggle(RemoveVersionButton);

            CreateGraphToVisualize();
        }

        private void RemoveVersionButton_Click(object sender, RoutedEventArgs e)
        {
            try
            {
                if (_versions.Count > 1)
                {
                    var version = int.Parse(VersionTextBox.Text);
                    _versions.RemoveAt(version);
                    CreateGraphToVisualize();
                }
                else
                {
                    Inform("Должна остаться хотя бы одна версия");
                }
            }
            catch (ArgumentOutOfRangeException)
            {
                Inform("Введенный элемент не был найден", MessageBoxImage.Exclamation);
            }
        }
    }
}
