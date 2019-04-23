using System;
using static Persistent;

namespace CSharpClient
{
    class Program
    {
        static void Main(string[] args)
        {
            var list = createList(new[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});

            print(toSeq(list));
        }
    }
}
