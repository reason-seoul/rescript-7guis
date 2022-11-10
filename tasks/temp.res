type state = {
  celsius: float,
  fahrenheit: float,
}

type action =
  | UpdateCelsius(float)
  | UpdateFahrenheit(float)

module TemperatureConverter = {
  module Util = {
    // F = C * (9/5) + 32
    let ce2fa = ce => ce *. (9. /. 5.) +. 32.

    // C = (F - 32) * (5/9)
    let fa2ce = fa => (fa -. 32.) *. (5. /. 9.)
  }

  include Program.Make({
    type state = state
    type action = action

    let initialState = {
      celsius: 5.,
      fahrenheit: Util.ce2fa(5.),
    }

    let reduce = (state, action) => {
      switch (state, action) {
      | (_, UpdateCelsius(celsius)) => {
          celsius,
          fahrenheit: celsius->Util.ce2fa,
        }
      | (_, UpdateFahrenheit(fahrenheit)) => {
          fahrenheit,
          celsius: fahrenheit->Util.fa2ce,
        }
      }
    }
  })

  let updateCelsius = (program, ce) => {
    let state = program->dispatch(UpdateCelsius(ce))
    {...program, state}
  }

  let updateFahrenheit = (program, fa) => {
    let state = program->dispatch(UpdateFahrenheit(fa))
    {...program, state}
  }
}
