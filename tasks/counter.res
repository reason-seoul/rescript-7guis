type state = {count: int}

type action = Increase

module Counter = {
  include Program.Make({
    type state = state
    type action = action

    let initialState = {
      count: 0,
    }

    let reduce = (state, action) => {
      switch action {
      | Increase => {count: state.count + 1}
      }
    }
  })

  let increase = t => {
    let state = t->dispatch(Increase)
    {...t, state}
  }
}
