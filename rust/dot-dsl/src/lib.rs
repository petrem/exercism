use attrs::Attrs;
use graph::{Edge, Graph, Node};
use std::collections::HashMap;

pub mod attrs {
    use crate::HashMap;
    pub trait Attrs {
        fn get_attrs(&self) -> &HashMap<String, String>;
        fn get_attrs_mut(&mut self) -> &mut HashMap<String, String>;

        fn with_attrs(mut self, attrs: &[(&str, &str)]) -> Self
        where
            Self: Sized,
        {
            self.get_attrs_mut()
                .extend(attrs.iter().map(|&(k, v)| (k.into(), v.into())));
            self
        }

        fn attr(&self, attr_name: &str) -> Option<&str> {
            self.get_attrs().get(attr_name).map(|v| v.as_str())
        }
    }
}

pub mod graph {
    use crate::HashMap;

    pub use graph_items::edge::Edge;
    pub use graph_items::node::Node;

    #[derive(Debug)]
    pub struct Graph {
        pub nodes: Vec<Node>,
        pub edges: Vec<Edge>,
        pub attrs: HashMap<String, String>,
    }

    pub mod graph_items {
        pub mod node {
            #[derive(Clone, Debug, Eq, PartialEq)]
            pub struct Node {
                pub attrs: crate::HashMap<String, String>,
                pub name: String,
            }
        }

        pub mod edge {
            #[derive(Clone, Debug, Eq, PartialEq)]
            pub struct Edge {
                pub node1: String,
                pub node2: String,
                pub attrs: crate::HashMap<String, String>,
            }
        }
    }
}

impl Graph {
    pub fn new() -> Self {
        Graph {
            nodes: Vec::new(),
            edges: Vec::new(),
            attrs: HashMap::new(),
        }
    }

    pub fn with_nodes(mut self, nodes: &[Node]) -> Self {
        self.nodes.extend(nodes.iter().cloned());
        self
    }

    pub fn with_edges(mut self, edges: &[Edge]) -> Self {
        self.edges.extend(edges.iter().cloned());
        self
    }

    pub fn node(&self, node_name: &str) -> Option<&Node> {
        self.nodes.iter().find(|&node| node.has_name(node_name))
    }

    // expose trait methods for the tests
    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Attrs::with_attrs(self, attrs)
    }

    pub fn attr(&self, name: &str) -> Option<&str> {
        Attrs::attr(self, name)
    }
}

impl Attrs for Graph {
    fn get_attrs(&self) -> &HashMap<String, String> {
        &self.attrs
    }

    fn get_attrs_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.attrs
    }
}

impl Node {
    pub fn new(name: &str) -> Self {
        Node {
            name: name.into(),
            attrs: HashMap::new(),
        }
    }

    #[inline]
    pub fn has_name(&self, name: &str) -> bool {
        self.name == name
    }

    // expose trait methods for the tests
    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Attrs::with_attrs(self, attrs)
    }

    pub fn attr(&self, name: &str) -> Option<&str> {
        Attrs::attr(self, name)
    }
}

impl Attrs for Node {
    fn get_attrs(&self) -> &HashMap<String, String> {
        &self.attrs
    }

    fn get_attrs_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.attrs
    }
}

impl Edge {
    pub fn new(node1: &str, node2: &str) -> Self {
        Edge {
            node1: node1.into(),
            node2: node2.into(),
            attrs: HashMap::new(),
        }
    }

    // expose trait methods for the tests
    pub fn with_attrs(self, attrs: &[(&str, &str)]) -> Self {
        Attrs::with_attrs(self, attrs)
    }

    pub fn attr(&self, name: &str) -> Option<&str> {
        Attrs::attr(self, name)
    }
}

impl Attrs for Edge {
    fn get_attrs(&self) -> &HashMap<String, String> {
        &self.attrs
    }

    fn get_attrs_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.attrs
    }
}
