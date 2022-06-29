**JSX V4 upgrade path**

1. No upgrading rescript-react && no changes in bsconfig.json

   JSX V3 by default.

2. No upgrading rescript-react && new configuration added in bsconfig.json

   a. JSX V3

   ```json
   "react": {
     "jsx": 3,
     "runtime": "classic"
   }
   ```

   The `react.runtime` affects nothing, no matter of `"classic"` or `"automatic"`.

   b. JSX V4 with classic mode

   ```json
   "react": {
     "jsx": 4,
     "runtime": "classic"
   }
   ```

   c. JSX V4 with new JSX mode (Experimental)

   ```json
   "react": {
     "jsx": 4,
     "runtime": "automatic"
   }
   ```

   JSX V4 with `Js.React` which needs the peer-dependecy of React v17.\* or higher. It may break the project with dependencies which are using the explicit types of `rescript-react`.

   > The existing configuration `reason.react-jsx` will be ignored by the new one.

3. Upgrading rescript-react

JSX V3 and V4 will work with the upgraded rescript-react. The new JSX mode is no longer the experimental feature. The new JSX mode will be triggered by `react.runtime` configuration in bsconfig.json.
