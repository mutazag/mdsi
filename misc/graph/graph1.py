#%%
# https://www.analyticsvidhya.com/blog/2018/04/introduction-to-graph-theory-network-analysis-python-codes/
import networkx as nx


# %%
G = nx.Graph()

# %%
G.add_node(1)

# %%
G.add_nodes_from([2,3])

# %%
G.add_edge(1,2)

# %%
e = (2,3)
G.add_edge(*e)
# %%
G.add_edges_from([(1,2),(1,3)])

# %%

import matplotlib.pyplot as plt

# %%
nx.draw(G)

# %%
G.add_node(4)


# %%
G.add_edges_from([(4,1),(4,3)])

# %%
G.add_node(5)
G.add_edges_from([(5,3),(5,2)])


# %%
G.add_node(6)
G.add_edge(6,5)

# %%
e1 = [7,8,9]
G.add_nodes_from(e1)
G.add_edges_from([(6,7),(6,8),(6,9)])

# %%
nx.draw_networkx(G)

# %%
G.add_nodes_from([10,11])


# %%
G.add_edges_from([(10,1),(10,2),(11,4),(11,5)])

# %%
G.add_edges_from([(7,8)])


# %%
