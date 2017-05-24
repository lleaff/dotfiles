module.exports = {
  "parser": "babel-eslint",

  "globals": {
    "React": true,
    "ReactDOM": true,
    "$": true,
    "_": true,
    "jQuery": true,
  },

  "env": {
    "es6": true,
    "node": true,
    "browser": true,
    "mocha": true,
  },

  "plugins": ["react"],

  "rules": {
    // "indent": [2, 2],
    "linebreak-style": [2, "unix"],
    "semi": [1, "always"], // Enfore semi-colons after statements
    "no-console": [0], // Disallow usage of console API (default 2)
    "no-unused-vars": [1,
      { "vars": "local", // local | all
        "argsIgnorePattern": "^_",
        "args": "after-used", // "after-used": Allow unused params if they come before a used param in the arg list
      }], // Warn against unused variables
    "comma-dangle": [1, "always-multiline"], // Disallow or enforce trailing comma in array literals, e.g.: "always-multiline"
    "no-cond-assign": [1], // Disallow `if (myVar = smth())`

    // eslint-plugin-react
    //------------------------------------------------------------
    "react/display-name": 0, // Prevent missing displayName in a React component definition
    "react/jsx-no-undef": 2, // Disallow undeclared variables in JSX
    "react/jsx-sort-props": 0, // Enforce props alphabetical sorting
    "react/jsx-uses-react": 2, // Prevent React to be incorrectly marked as unused
    "react/jsx-uses-vars": 2, // Prevent variables used in JSX to be incorrectly marked as unused
    "react/no-did-mount-set-state": 2, // Prevent usage of setState in componentDidMount
    "react/no-did-update-set-state": 2, // Prevent usage of setState in componentDidUpdate
    "react/no-multi-comp": 0, // Prevent multiple component definition per file
    "react/no-unknown-property": 2, // Prevent usage of unknown DOM property
    "react/prop-types": 0, // Prevent missing props validation in a React component definition (propTypes)
    "react/react-in-jsx-scope": 2, // Prevent missing React when using JSX
    "react/self-closing-comp": 2, // Prevent extra closing tags for components without children
    //"react/wrap-multilines": 2, // Prevent missing parentheses around multilines JSX // Disabled cause eslint doesn't find the rule (even tho it's documented)

  },

  "extends": ["eslint:recommended"],
};
