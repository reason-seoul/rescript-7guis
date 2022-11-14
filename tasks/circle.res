module Point = {
  type t = {
    x: int,
    y: int,
  }

  let distance = (p1, p2) => {
    open Belt

    let dx = p1.x - p2.x
    let dy = p1.y - p2.y

    Js.Math.sqrt((dx * dx + dy * dy)->Int.toFloat)
  }
}

module Rect = {
  type t = {
    x: int,
    y: int,
    width: int,
    height: int,
  }
}

module Circle = {
  type t = {
    center: Point.t,
    radius: int,
  }

  let includes = (t, point) => {
    let dist = t.center->Point.distance(point)
    dist <= t.radius->Belt.Int.toFloat
  }

  let toRect = ({center, radius}) => {
    {
      Rect.x: center.x - radius,
      y: center.y - radius,
      width: radius * 2,
      height: radius * 2,
    }
  }
}

type entry = {
  id: int,
  disabled: bool,
  selected: bool,
  circle: Circle.t,
}

type context = {
  id_seq: int,
  mouse: Point.t,
  backward: list<entry>,
  forward: list<entry>,
}

type state =
  | Idle(context)
  | Hover({entry: entry, context: context})
  | Focus({entry: entry, changed: entry, context: context})

type action =
  | ChangeRadius(int)
  | MouseMove(Point.t)
  | Click
  | Confirm
  | Undo
  | Redo

module CircleDrawer = {
  let defaultRadius = 10

  module Util = {
    let transferHead = (this, other) => {
      switch this {
      | list{head, ...this} => (this, list{head, ...other})
      | _ => (this, other)
      }
    }

    let disableLastEntry = entries => {
      switch entries {
      | list{entry, ...entries} => list{{...entry, disabled: true}, ...entries}
      | entries => entries
      }
    }

    let enableLastEntry = entries => {
      switch entries {
      | list{entry, ...entries} => list{{...entry, disabled: false}, ...entries}
      | entries => entries
      }
    }
  }

  module Context = {
    let canUndo = ctx => {
      ctx.backward->Belt.List.length != 0
    }

    let canRedo = ctx => {
      ctx.forward->Belt.List.length != 0
    }

    let getHoveringEntry = context => {
      let {mouse, backward} = context
      backward->Belt.List.getBy(entry => entry.circle->Circle.includes(mouse))
    }
  }

  module State = {
    let canUndo = state => {
      let Idle(context) | Hover({context}) | Focus({context}) = state
      context->Context.canUndo
    }

    let canRedo = state => {
      let Idle(context) | Hover({context}) | Focus({context}) = state
      context->Context.canRedo
    }

    let getChangingEntry = state => {
      switch state {
      | Focus({changed}) => Some(changed)
      | _ => None
      }
    }

    let getEntries = state => {
      let Idle(context) | Hover({context}) | Focus({context}) = state
      context.backward->Belt.List.toArray
    }
  }

  include Program.Make({
    type state = state
    type action = action

    let initialState = Idle({
      id_seq: 0,
      mouse: {x: -9999, y: -9999},
      forward: list{},
      backward: list{},
    })

    let reduce = (state, action) => {
      open Belt

      switch (state, action) {
      | (Idle(context) | Hover({context}), MouseMove(mouse)) => {
          let context = {...context, mouse}
          let {backward} = context
          let hoveringEntry = context->Context.getHoveringEntry
          let backward = backward->Belt.List.map(entry =>
            switch (hoveringEntry, entry) {
            | (Some(hoveringEntry), entry) if entry.id == hoveringEntry.id => {
                ...entry,
                selected: true,
              }
            | (_, entry) => {...entry, selected: false}
            }
          )
          let context = {
            ...context,
            backward,
          }
          switch hoveringEntry {
          | Some(entry) => Hover({entry, context})
          | None => Idle(context)
          }
        }

      | (Idle(context), Click) => {
          let {id_seq, backward, mouse} = context
          let {x, y} = mouse
          let id = id_seq + 1
          let entry = {
            id,
            selected: true,
            disabled: false,
            circle: {
              center: {x, y},
              radius: defaultRadius,
            },
          }
          Hover({
            entry,
            context: {
              ...context,
              id_seq: id,
              forward: list{},
              backward: backward->List.add(entry),
            },
          })
        }

      | (Idle(context), Undo) if context->Context.canUndo => {
          let {backward, forward} = context
          let (backward, forward) = backward->Util.transferHead(forward)
          let backward = backward->Util.enableLastEntry
          Idle({...context, backward, forward})
        }

      | (Idle(context), Redo) if context->Context.canRedo => {
          let {backward, forward} = context
          let backward = switch (backward, forward) {
          | (list{backwardEntry, ..._}, list{forwardEntry, ..._})
            if backwardEntry.id == forwardEntry.id =>
            backward->Util.disableLastEntry

          | _ => backward
          }
          let (forward, backward) = forward->Util.transferHead(backward)
          Idle({...context, backward, forward})
        }

      | (Hover({entry, context}), Click) => Focus({entry, changed: entry, context})

      | (Focus({entry: focusing, context}), ChangeRadius(radius)) => {
          let {backward} = context
          let changed = {...focusing, circle: {...focusing.circle, radius}}
          let backward = backward->List.map(entry =>
            switch entry {
            | entry if entry.id == focusing.id => changed
            | _ => entry
            }
          )
          Focus({entry: focusing, changed, context: {...context, backward}})
        }

      | (Focus({entry, changed, context}), Confirm) => {
          let backward = switch context.backward {
          | list{changed, ...backward} =>
            backward->List.add(entry)->Util.disableLastEntry->List.add(changed)
          | backward => backward
          }
          Hover({entry: changed, context: {...context, backward}})
        }

      | _ => state
      }
    }
  })

  let mousemove = (program, point) => {
    let state = program->dispatch(MouseMove(point))
    {...program, state}
  }

  let click = program => {
    let state = program->dispatch(Click)
    {...program, state}
  }

  let confirm = program => {
    let state = program->dispatch(Confirm)
    {...program, state}
  }

  let undo = program => {
    let state = program->dispatch(Undo)
    {...program, state}
  }

  let redo = program => {
    let state = program->dispatch(Redo)
    {...program, state}
  }

  let changeRadius = (program, radius) => {
    let state = program->dispatch(ChangeRadius(radius))
    {...program, state}
  }
}
