type data = {
  capturedTime: int,
  totalElapsedTime: int,
  duration: int,
}

type state = Idle(data) | Running(data) | Done(data)

type action =
  | Tick({currentTime: int})
  | Reset({currentTime: int})
  | ChangeDuration({currentTime: int, duration: int})

module Timer = {
  module Util = {
    let maxDuration = 60 * 1000

    let durationToPercent = duration => {
      open Belt
      (Int.toFloat(duration) /. Int.toFloat(maxDuration) *. 100.)->Int.fromFloat
    }

    let percentToDuration = pct => {
      open Belt
      (pct /. 100. *. Int.toFloat(maxDuration))->Int.fromFloat
    }

    let formatTime = time => {
      let time = time->Belt.Int.toFloat
      (time /. 1000.)->Js.Float.toFixedWithPrecision(~digits=1) ++ "s"
    }
  }

  module State = {
    let canTick = state => {
      switch state {
      | Idle(_) | Running(_) => true
      | Done(_) => false
      }
    }

    let getTotalElapsedTime = state => {
      let Idle(data) | Running(data) | Done(data) = state
      let {totalElapsedTime} = data
      totalElapsedTime
    }

    let getDuration = state => {
      let Idle(data) | Running(data) | Done(data) = state
      let {duration} = data
      duration
    }
  }

  include Program.Make({
    type state = state
    type action = action

    let initialState = Idle({
      capturedTime: 0,
      totalElapsedTime: 0,
      duration: Util.maxDuration / 2,
    })

    let deriveState = (currentTime, {capturedTime, totalElapsedTime, duration}) => {
      let elapsedTime = currentTime - capturedTime
      let nextTotalElapsedTime = totalElapsedTime + elapsedTime
      let done = nextTotalElapsedTime >= duration
      let data = {
        capturedTime: currentTime,
        totalElapsedTime: done ? totalElapsedTime : nextTotalElapsedTime,
        duration,
      }
      done ? Done(data) : Running(data)
    }

    let reduce = (state, action) => {
      switch (state, action) {
      | (Idle(data), Tick({currentTime})) =>
        deriveState(currentTime, {...data, capturedTime: currentTime})
      | (Running(data), Tick({currentTime})) => deriveState(currentTime, data)
      | (Running(data) | Done(data), Reset({currentTime})) =>
        deriveState(
          currentTime,
          {
            ...data,
            totalElapsedTime: 0,
          },
        )
      | (Running(data) | Done(data), ChangeDuration({currentTime, duration})) =>
        deriveState(
          currentTime,
          {
            ...data,
            duration,
          },
        )
      | _ => state
      }
    }
  })

  let tick = (program, currentTime) => {
    let state = program->dispatch(Tick({currentTime: currentTime->Belt.Int.fromFloat}))
    {...program, state}
  }

  let reset = (program, currentTime) => {
    let state = program->dispatch(Reset({currentTime: currentTime->Belt.Int.fromFloat}))
    {...program, state}
  }

  let updateDuration = (program, currentTime, duration) => {
    let state =
      program->dispatch(ChangeDuration({currentTime: currentTime->Belt.Int.fromFloat, duration}))
    {...program, state}
  }
}
