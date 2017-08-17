# miso-middleware

A collection of _middlewares_ for
the [miso](https://github.com/haskell-miso/miso), a Haskell front-end
webframework. A _middleware_ is a function that modifies the behavior
of an existing `App` or adds new functionality to it.

Currently this package contains three middlewares which allow you to do the following:
1. Automatically models to local storage and restore them on the initial pageload.
2. Log all actions and model changes to the console.
3. Add a time-travelling debugger that records all state transitions
   and allows you to navigate between them.
