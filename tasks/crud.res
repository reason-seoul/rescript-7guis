type entry = {
  id: int,
  name: string,
  surname: string,
}

type state = {
  id_seq: int,
  filter: string,
  name: string,
  surname: string,
  entries: array<entry>,
  selected: option<entry>,
}

type action =
  | CreateEntry
  | UpdateEntry(int)
  | SelectEntry(int)
  | DeleteEntry(int)
  | ChangeName(string)
  | ChangeSurname(string)
  | ChangeFilter(string)

module CRUD = {
  module Util = {
    let getFullname = ({name, surname}: entry) => {
      name ++ ", " ++ surname
    }
  }

  module State = {
    let selectEntry = ({entries}, id) => {
      entries->Belt.Array.getBy(entry => entry.id == id)
    }

    let getFilteredEntries = ({filter, entries}) => {
      entries->Belt.Array.keep(({surname}) => surname->Js.String2.startsWith(filter))
    }
  }

  include Program.Make({
    type state = state
    type action = action

    let initialState = {
      id_seq: 0,
      filter: "",
      name: "",
      surname: "",
      entries: [],
      selected: None,
    }

    let reduce = (state, action) => {
      open Belt
      switch (state, action) {
      | ({id_seq, name, surname, entries}, CreateEntry) => {
          let id = id_seq + 1
          let entry = {id, name, surname}
          {
            ...state,
            id_seq: id,
            entries: entries->Array.concat([entry]),
          }
        }

      | ({entries, name, surname}, UpdateEntry(id)) => {
          ...state,
          entries: entries->Array.map(entry =>
            switch entry {
            | entry if entry.id == id => {...entry, name, surname}
            | entry => entry
            }
          ),
        }

      | (_, SelectEntry(id)) => {
          let selected = state->State.selectEntry(id)
          switch selected {
          | Some({name, surname}) => {...state, selected, name, surname}
          | None => {...state, selected}
          }
        }

      | ({entries}, DeleteEntry(id)) => {
          let state = {
            ...state,
            entries: entries->Array.keep(entry =>
              switch entry {
              | entry if entry.id == id => false
              | _ => true
              }
            ),
          }
          {
            ...state,
            selected: state->State.selectEntry(id),
          }
        }

      | (_, ChangeName(name)) => {
          ...state,
          name,
        }

      | (_, ChangeSurname(surname)) => {
          ...state,
          surname,
        }

      | (_, ChangeFilter(filter)) => {
          ...state,
          filter,
        }
      }
    }
  })

  let createEntry = program => {
    let state = program->dispatch(CreateEntry)
    {...program, state}
  }

  let updateEntry = (program, id) => {
    let state = program->dispatch(UpdateEntry(id))
    {...program, state}
  }

  let selectEntry = (program, id) => {
    let state = program->dispatch(SelectEntry(id))
    {...program, state}
  }

  let deleteEntry = (program, id) => {
    let state = program->dispatch(DeleteEntry(id))
    {...program, state}
  }

  let changeName = (program, name) => {
    let state = program->dispatch(ChangeName(name))
    {...program, state}
  }

  let changeSurname = (program, surname) => {
    let state = program->dispatch(ChangeSurname(surname))
    {...program, state}
  }

  let changeFilter = (program, filter) => {
    let state = program->dispatch(ChangeFilter(filter))
    {...program, state}
  }
}
