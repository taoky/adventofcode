use adventofcode_2022::utils::day7::{handle_input, Item, Node, Tree, Type};

fn main() {
    let mut tree = Tree::<Item>::default();
    // We know that the first node idx must be 0
    let root = Node::new(Item::new("/", 0, Type::Folder), 0);
    tree.add(root);

    handle_input(&mut tree, 0);
    let mut sum = 0;
    for i in tree.arena {
        if i.val.typ == Type::Folder && i.val.size <= 100000 {
            sum += i.val.size;
        }
    }
    println!("{}", sum);
}
