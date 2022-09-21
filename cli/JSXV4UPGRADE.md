**JSX V4 upgrade path**

1. Nothing to change for existing projects with old bsconfig.json

   JSX V3 by default.

2. New opt-in configuration added in bsconfig.json

   a. JSX V3

   ```json
   "jsx": {
     "version": 3,
   }
   ```

   b. JSX V4 with classic mode (generate calls to `React.createElement` just as with V3)

   ```json
   "jsx": {
     "version": 4,
     "mode": "classic"
   }
   ```

   c. JSX V4 with experimental JSX mode (generate calls `jsx` functions)

   ```json
   "jsx": {
     "version": 4,
     "mode": "automatic"
   }
   ```

   JSX V4 with `"automatic"` runtme needs React v17.\* or higher as a peer dependency. It will require a new version of`rescript-react`.

   > The existing configuration `reason.react-jsx` will be ignored if the new configuration is present.

   d. Dependencies
   Dependencies inherit the jsx configuration of the root project.
   To build certain dependencies in JSX V3 compatibility mode, use `"v3-dependencies"` as in the example:

   ```json
   "jsx": { "version": 4, "v3-dependencies": ["rescript-react-navigation"] }
   ```
