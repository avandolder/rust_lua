use std::cell::RefCell;
use std::rc::Rc;

pub struct Table(Vec<(Value, Value)>);

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Function(Rc<RefCell<()>>),
    Nil,
    Number(f64),
    String(String),
    Table(Rc<RefCell<Table>>),

    // Unimplemented:
    Thread,
    Userdata,
}
