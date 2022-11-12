module Point = {
  type t = {
    x: int,
    y: int,
  }

  let distance = (p1, p2) => {
    open Belt

    let x1 = p1.x->Int.toFloat
    let y1 = p1.y->Int.toFloat

    let x2 = p2.x->Int.toFloat
    let y2 = p2.y->Int.toFloat

    open Js.Math
    (pow_float(~base=x2 -. x1, ~exp=2.) +. pow_float(~base=y2 -. y1, ~exp=2.))->Js.Math.sqrt
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
    open Rect
    {x: center.x - radius, y: center.y - radius, width: radius * 2, height: radius * 2}
  }
}

type entry = {
  id: int,
  disabled: bool,
  selected: bool,
  circle: Circle.t,
}

type data = {
  id_seq: int,
  mouse: Point.t,
  backward: list<entry>,
  forward: list<entry>,
}

type state =
  | Idle(data)
  | Hover({entry: entry, data: data})
  | Focus({entry: entry, changed: entry, data: data})

type action =
  | ChangeRadius(int)
  | MouseMove(Point.t)
  | Click
  | Confirm
  | Undo
  | Redo

module CircleDrawer = {
  module Util = {
    let defaultRadus = 10

    let arrayToList = arr => arr->Belt.List.fromArray
    let listToArray = list => list->Belt.List.toArray

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

  module Data = {
    let canUndo = data => {
      data.backward->Belt.List.length != 0
    }

    let canRedo = data => {
      data.forward->Belt.List.length != 0
    }

    let getNearestEntry = ({mouse, backward: entries}) => {
      open Belt
      entries
      ->List.sort((a, b) => {
        let a_dist = a.circle.center->Point.distance(mouse)
        let b_dist = b.circle.center->Point.distance(mouse)
        (a_dist -. b_dist)->Int.fromFloat
      })
      ->List.head
    }

    let getHoveringEntry = data => {
      switch (data, data->getNearestEntry) {
      | ({mouse}, Some(entry)) if entry.circle->Circle.includes(mouse) => Some(entry)
      | _ => None
      }
    }
  }

  module State = {
    let canUndo = state => {
      let Idle(data) | Hover({data}) | Focus({data}) = state
      data->Data.canUndo
    }

    let canRedo = state => {
      let Idle(data) | Hover({data}) | Focus({data}) = state
      data->Data.canRedo
    }

    let getChangingEntry = state => {
      switch state {
      | Focus({changed}) => Some(changed)
      | _ => None
      }
    }

    let getEntries = state => {
      let Idle(data) | Hover({data}) | Focus({data}) = state
      data.backward->Belt.List.toArray
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
      | (Idle(data) | Hover({data}), MouseMove(mouse)) => {
          let data = {...data, mouse}
          let {backward} = data
          let hoveringEntry = data->Data.getHoveringEntry
          let backward = backward->Belt.List.map(entry =>
            switch (hoveringEntry, entry) {
            | (Some(hoveringEntry), entry) if entry.id == hoveringEntry.id => {
                ...entry,
                selected: true,
              }
            | (_, entry) => {...entry, selected: false}
            }
          )
          let data = {
            ...data,
            backward,
          }
          switch hoveringEntry {
          | Some(entry) => Hover({entry, data})
          | None => Idle(data)
          }
        }

      | (Idle(data), Click) => {
          let {id_seq, backward, mouse} = data
          let {x, y} = mouse
          let id = id_seq + 1
          let entry = {
            id,
            selected: true,
            disabled: false,
            circle: {
              center: {x, y},
              radius: Util.defaultRadus,
            },
          }
          Hover({
            entry,
            data: {
              ...data,
              id_seq: id,
              forward: list{},
              backward: backward->List.add(entry),
            },
          })
        }

      | (Idle(data), Undo) if data->Data.canUndo => {
          let {backward, forward} = data
          let (backward, forward) = backward->Util.transferHead(forward)
          let backward = backward->Util.enableLastEntry
          Idle({...data, backward, forward})
        }

      | (Idle(data), Redo) if data->Data.canRedo => {
          let {backward, forward} = data
          let backward = switch (backward, forward) {
          | (list{backwardEntry, ..._}, list{forwardEntry, ..._})
            if backwardEntry.id == forwardEntry.id =>
            backward->Util.disableLastEntry

          | _ => backward
          }
          let (forward, backward) = forward->Util.transferHead(backward)
          Idle({...data, backward, forward})
        }

      | (Hover({entry, data}), Click) => Focus({entry, changed: entry, data})

      | (Focus({entry: focusing, data}), ChangeRadius(radius)) => {
          let {backward} = data
          let changed = {...focusing, circle: {...focusing.circle, radius}}
          let backward = backward->List.map(entry =>
            switch entry {
            | entry if entry.id == focusing.id => changed
            | _ => entry
            }
          )
          Focus({entry: focusing, changed, data: {...data, backward}})
        }

      | (Focus({entry, changed, data}), Confirm) => {
          let backward = switch data.backward {
          | list{changed, ...backward} =>
            backward->List.add(entry)->Util.disableLastEntry->List.add(changed)
          | backward => backward
          }
          Hover({entry: changed, data: {...data, backward}})
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
