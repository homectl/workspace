### 2.0.0

- Add support for timed wait using `WaitTimeout` as `EventPolicy` to limit fps.
- Add `setWindowSizeCallback` so that it is no longer necessary to poll for
  window size changes.
- **Potentially breaking change:** Add `configEnableDebug :: Bool` field to the
  `ContextHandlerParameters Handler`/`HandleConfig` data constructor.
- Suppress debug logging by default. Turn it back on with
  `defaultHandleConfig{configEnableDebug = True}` config.
- Move to OpenGL Core Profile 4.5.

### 1.4.1.4

- [sorki](https://github.com/sorki) bumped versions.
- Reworked the nix integration.

### 1.4.1.3

- [MaciekFlis](https://github.com/MaciekFlis) bumped versions and resolver.

### 1.4.1.2

- [Kludgy](https://github.com/Kludgy) bumped versions and resolver.
- [LinuxUser404](https://github.com/LinuxUser404) bumped versions and added `cabal.project` & `.travis.yml`.
- Set up travis builds with stack.

### 1.4.1.1

- Split changelog to own file (before this, it's located in README.md).
- [lambdael](https://github.com/lambdael) added `glfwGetWindowSize`.
- [Bump upper bound on `base` dependency](https://github.com/fpco/stackage/issues/2670). Bump stack LTS.

### 1.4.1

- Split `Wrapped` module to `Window` and `Misc` modules.
- Don't expose `ErrorCallback`, do expose the `Error` type for custom error callbacks.
- Switch from ad-hoc parenting for shared contexts, to the "ancestor" pattern described in [#24](https://github.com/plredmond/GPipe-GLFW/issues/24#issuecomment-299681824).
- Adjustments to debug logging format.
- Add smoketest for window close functions & sequential GPipe windows.
- Bump deps to `GPipe-2.2.1`

### 1.4.0

- Rewrite for new window handling interface.
- Separate smoke tests to own package.

### 1.3.0

- Overhaul `Graphics.GPipe.Context.GLFW.Input` to expose most of the functionality in [GLFW Input guide](http://www.glfw.org/docs/latest/input_guide.html).

### 1.2.3

- [SwiftsNamesake](https://github.com/SwiftsNamesake) bumped version constraints.
- Add a smoke test and stubs for shared-context tests.

### 1.2.2

- [grtlr](https://github.com/grtlr) added scroll callback registration.
- Add a readme to be a good citizen and update documentation.

### 1.2.1

- [bch29](https://github.com/bch29) refactored and added new GLFW input callback registration functions as well as the `unsafe` module to access the GLFW window directly.

### 1.2

- [bch29](https://github.com/bch29) exposed more of the underlying GLFW hints.
