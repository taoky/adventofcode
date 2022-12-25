// ArenaTree: https://dev.to/deciduously/no-more-tears-no-more-knots-arena-allocated-trees-in-rust-44k6

#[derive(Debug, Default)]
pub struct Tree<T> {
    pub arena: Vec<Node<T>>,
    pub visited: Vec<bool>,
}

#[derive(Debug, Default)]
pub struct Node<T> {
    pub val: T,
    pub parent: usize, // parent of root is root itself
    pub children: Vec<usize>,
}

impl<T> Node<T> {
    pub fn new(val: T, parent: usize) -> Self {
        Self {
            val,
            parent,
            children: Vec::new(),
        }
    }
}

impl<T> Tree<T> {
    pub fn add(&mut self, node: Node<T>) {
        let parent = node.parent;
        self.arena.push(node);
        self.visited.push(false);
        let node_idx = self.arena.len() - 1;
        self.arena[parent].children.push(node_idx);
    }
}

#[derive(Debug, Default, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    #[default]
    Folder,
    File,
}

#[derive(Debug, Default)]
pub struct Item {
    pub name: String,
    pub size: usize,
    pub typ: Type,
}

impl Item {
    pub fn new(name: &str, size: usize, typ: Type) -> Self {
        Self {
            name: name.to_string(),
            size,
            typ,
        }
    }
}

pub fn handle_input(tree: &mut Tree<Item>, mut cwd: usize) {
    let mut mark_visited = false;
    loop {
        let mut input = String::new();
        let input_size = std::io::stdin().read_line(&mut input).unwrap();
        if input_size == 0 {
            break;
        }
        if input.starts_with('$') {
            if mark_visited {
                tree.visited[cwd] = true;
                mark_visited = false;
            }
            // user command
            let cmd: Vec<_> = input.trim().split(' ').collect();
            match cmd[1] {
                "cd" => {
                    let target = cmd[2];
                    if target == ".." {
                        cwd = tree.arena[cwd].parent;
                    } else {
                        let mut success = false;
                        for child in &tree.arena[cwd].children {
                            if tree.arena[*child].val.name == target {
                                cwd = *child;
                                success = true;
                                break;
                            }
                        }
                        assert!(success);
                    }
                    assert!(tree.arena[cwd].val.typ == Type::Folder);
                }
                "ls" => {
                    // do nothing here
                }
                _ => panic!("Invalid command"),
            }
        } else {
            // ls output
            if tree.visited[cwd] {
                continue;
            } else {
                mark_visited = true;
            }
            let os = input.trim().split(' ').collect::<Vec<_>>();
            let node = if os[0] == "dir" {
                Node::new(Item::new(os[1], 0, Type::Folder), cwd)
            } else {
                let size: usize = os[0].parse().unwrap();
                Node::new(Item::new(os[1], size, Type::File), cwd)
            };
            let size = node.val.size;
            let typ = node.val.typ;
            tree.add(node);

            // recursively update size
            if typ == Type::File {
                let mut recursive_idx = cwd;
                while recursive_idx != 0 {
                    tree.arena[recursive_idx].val.size += size;
                    recursive_idx = tree.arena[recursive_idx].parent;
                }
                tree.arena[0].val.size += size;
            }
        }
    }
}
