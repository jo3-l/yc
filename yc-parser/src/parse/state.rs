use drop_bomb::DropBomb;

#[derive(Clone, Debug)]
pub(crate) struct ParseState {
    var_stack: Vec<String>,
    loop_depth: usize,
}

impl ParseState {
    pub(crate) fn enter_scope(&mut self) -> Scope {
        Scope::new(self.var_stack.len())
    }

    pub(crate) fn enter_loop(&mut self) -> Loop {
        Loop::new(self.loop_depth, self.var_stack.len())
    }

    pub(crate) fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    pub(crate) fn has_var(&self, var: &str) -> bool {
        self.var_stack.contains(&var.to_string())
    }

    pub(crate) fn push_var(&mut self, var: &str) {
        self.var_stack.push(var.to_string())
    }
}

#[derive(Debug)]
pub(crate) struct Scope {
    orig_stack_depth: usize,
    bomb: DropBomb,
}

impl Scope {
    fn new(orig_stack_depth: usize) -> Self {
        Self {
            orig_stack_depth,
            bomb: DropBomb::new(
                "`Scope::exit()` must be called explicitly \
                to prevent leaking variables into outer scopes",
            ),
        }
    }

    pub(crate) fn exit(&mut self, state: &mut ParseState) {
        self.bomb.defuse();
        state.var_stack.truncate(self.orig_stack_depth);
    }
}

#[derive(Debug)]
pub(crate) struct Loop {
    orig_loop_depth: usize,
    orig_stack_depth: usize,
    bomb: DropBomb,
}

impl Loop {
    fn new(orig_loop_depth: usize, orig_stack_depth: usize) -> Self {
        Self {
            orig_loop_depth,
            orig_stack_depth,
            bomb: DropBomb::new(
                "`Loop::exit()` must be called explicitly \
                to prevent leaking variables into outer scopes",
            ),
        }
    }

    pub(crate) fn exit(mut self, state: &mut ParseState) {
        self.bomb.defuse();
        state.var_stack.truncate(self.orig_stack_depth);
        state.loop_depth = self.orig_loop_depth;
    }
}
