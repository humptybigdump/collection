#include "Kruskal.h"
#include "UnionFind.h"
#include "WeightedGraph.h"

#include <algorithm>
#include <memory>
#include <iostream>

Kruskal::Kruskal(const WeightedGraph& graph)
    : weight(0)
{
    // Extract all edges from the given graph.
    auto edges = graph.allEdges();

    // Sort the edges by weight.
    std::sort(edges.begin(), edges.end(),
        [] (const WeightedEdgePtr& l, const WeightedEdgePtr& r) -> bool {
            return l->w < r->w;
        }
    );

    UnionFind uf(graph.vertexCount());
    for (auto& e : edges) {
        if (uf.findSet(e->p.first) != uf.findSet(e->p.second)) {
            weight += e->w;
            uf.unionSets(e->p.first, e->p.second);
            mst.push_back(e);
            // MSTs are complete with V-1 edges. Quit if this is the case.
            if (mst.size() == graph.vertexCount() - 1)
                break;
        }
    }
}
