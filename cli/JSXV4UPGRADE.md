**JSX V4 upgrade path**

1. No need to upgrade rescript-react or change bsconfig.json

   JSX V3 by default.

2. New opt-in configuration added in bsconfig.json

   a. JSX V3

   ```json
   "jsx": {
     "version": 3,
     "runtime": "classic"
   }
   ```

   **Note:** When using `jsx` v3, the `runtime` option will be ignored.

   b. JSX V4 with classic mode (generate calls to `React.createElement` just as with V3)

   ```json
   "jsx": {
     "version": 4,
     "runtime": "classic"
   }
   ```

   c. JSX V4 with experimental JSX mode (generate calls `jsx` functions)

   ```json
   "jsx": {
     "version": 4,
     "runtime": "automatic"
   }
   ```

   JSX V4 with `"automatic"` runtme needs React v17.\* or higher as a peer dependency. It will require a new version of`rescript-react`.

   > The existing configuration `reason.react-jsx` will be ignored if the new configuration is present.
