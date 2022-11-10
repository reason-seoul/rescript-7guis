module Make = (
  Config: {
    type state
    let initialState: state

    type action

    let reduce: (state, action) => state
  },
) => {
  type state = Config.state
  type action = Config.action

  let initialState = Config.initialState
  let reduce = Config.reduce

  type t = {
    state: state,
    onUpdate: (. state, state) => option<unit => unit>,
  }

  let make = onUpdate => {
    state: initialState,
    onUpdate,
  }

  let init = program => {
    program.onUpdate(. initialState, initialState)
  }

  let dispatch = (program, action) => {
    let prev = program.state
    let next = program.state->reduce(action)
    switch program.onUpdate(. next, prev) {
    | Some(cleanup) => cleanup()
    | None => ()
    }
    next
  }
}
