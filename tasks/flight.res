module Field = {
  type t =
    | Empty
    | Valid(string)
    | Invalid(string)

  let parse = text => {
    let re = %re("/^(?<DD>\d{2})\.(?<MM>\d{2})\.(?<YYYY>\d{4})$/")
    switch text {
    | "" => Empty
    | text =>
      switch re->Js.Re.test_(text) {
      | true => Valid(text)
      | false => Invalid(text)
      }
    }
  }

  let validate = text => {
    switch text->parse {
    | Valid(_) => true
    | _ => false
    }
  }

  let toString = t => {
    switch t {
    | Empty => ""
    | Valid(text) | Invalid(text) => text
    }
  }
}

type state =
  | OneWay(Field.t)
  | Returning(Field.t, Field.t)

type action =
  | SelectType([#oneway | #returning])
  | ChangeStartDate(string)
  | ChangeEndDate(string)

module FlightBooker = {
  module Util = {
    let formatType = type_ => {
      switch type_ {
      | #oneway => "one-way flight"
      | #returning => "return flight"
      }
    }
  }

  module State = {
    let getType = state => {
      switch state {
      | OneWay(_) => #oneway
      | Returning(_, _) => #returning
      }
    }

    let getStartDate = state => {
      switch state {
      | OneWay(field) | Returning(field, _) => field->Field.toString
      }
    }

    let getEndDate = state => {
      switch state {
      | OneWay(field) | Returning(_, field) => field->Field.toString
      }
    }
  }

  include Program.Make({
    type state = state
    type action = action

    let initialState = OneWay(Empty)

    let reduce = (state, action) => {
      switch (state, action) {
      | (OneWay(field), SelectType(#returning)) => Returning(field, field)
      | (Returning(field, _), SelectType(#oneway)) => OneWay(field)
      | (OneWay(_), ChangeStartDate(text)) => OneWay(text->Field.parse)
      | (Returning(_, end), ChangeStartDate(text)) => Returning(text->Field.parse, end)
      | (Returning(start, _), ChangeEndDate(text)) => Returning(start, text->Field.parse)
      | _ => state
      }
    }
  })

  let selectType = (program, type_) => {
    let state = program->dispatch(SelectType(type_))
    {...program, state}
  }

  let changeStartDate = (program, startDate) => {
    let state = program->dispatch(ChangeStartDate(startDate))
    {...program, state}
  }

  let changeEndDate = (program, endDate) => {
    let state = program->dispatch(ChangeEndDate(endDate))
    {...program, state}
  }
}
