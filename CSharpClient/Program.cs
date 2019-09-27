using System;
using Persistent;
using static Persistent.List;

namespace CSharpClient
{
    class Program
    {
        static void Main(string[] args)
        {
            var list = createList(new[] {1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
            var nil = List<int>.Nil;
            

            print(toSeq(list));
        }
    }
}
