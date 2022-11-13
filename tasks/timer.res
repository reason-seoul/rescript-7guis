type context = {
  capturedTime: int,
  totalElapsedTime: int,
  duration: int,
}

type state = Idle(context) | Running(context) | Done(context)

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
      let Idle(context) | Running(context) | Done(context) = state
      let {totalElapsedTime} = context
      totalElapsedTime
    }

    let getDuration = state => {
      let Idle(context) | Running(context) | Done(context) = state
      let {duration} = context
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
      let context = {
        capturedTime: currentTime,
        totalElapsedTime: done ? totalElapsedTime : nextTotalElapsedTime,
        duration,
      }
      done ? Done(context) : Running(context)
    }

    let reduce = (state, action) => {
      switch (state, action) {
      | (Idle(context), Tick({currentTime})) =>
        deriveState(currentTime, {...context, capturedTime: currentTime})
      | (Running(context), Tick({currentTime})) => deriveState(currentTime, context)
      | (Running(context) | Done(context), Reset({currentTime})) =>
        deriveState(
          currentTime,
          {
            ...context,
            totalElapsedTime: 0,
          },
        )
      | (Running(context) | Done(context), ChangeDuration({currentTime, duration})) =>
        deriveState(
          currentTime,
          {
            ...context,
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
