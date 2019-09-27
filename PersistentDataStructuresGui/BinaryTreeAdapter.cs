using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Persistent;
using static Persistent.BinaryTree;

namespace PersistentDataStructuresGui
{
    public class BinaryTreeAdapter
    {
        public readonly HashSet<AdapterNode> Nodes;
        public readonly List<(AdapterNode, AdapterNode)> Edges;

        public BinaryTreeAdapter(IEnumerable<BinaryTree<int>> trees)
        {
            var flat = new HashSet<BinaryTree<int>>();

            foreach (var tree in trees)
            {
                var seq = toRawSeq<int>(TraversalOrder.Prefix).Invoke(tree);

                foreach (var a in seq)
                {
                    flat.Add(a);
                }
            }

            (Nodes, Edges) = ParseTree(flat);
        }

        private (HashSet<AdapterNode>, List<(AdapterNode, AdapterNode)>) ParseTree(HashSet<BinaryTree<int>> trees)
        {
            var nodes = new HashSet<AdapterNode>();
            var map = new Dictionary<BinaryTree<int>, AdapterNode>();
            var edges = new List<(AdapterNode, AdapterNode)>();

            foreach (var a in trees)
            {
                if (!map.ContainsKey(a))
                {
                    var node = new AdapterNode(value(a));
                    nodes.Add(node);
                    map[a] = node;
                }
            }

            foreach (var a in trees)
            {
                var l = left(a);
                var r = right(a);

                if (map.ContainsKey(l))
                {
                    edges.Add((map[a], map[l]));
                }

                if (map.ContainsKey(r))
                {
                    edges.Add((map[a], map[r]));
                }
            }

            return (nodes, edges);
        }
    }
}
