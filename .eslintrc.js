// Utilities
//------------------------------------------------------------

const typeMismatch = (dst, src) => {
  console.error('[deepMerge] Type mismatch: ', dst, '...and...', src);
};
const dedup = (arr) => [...new Set(arr)];
const _deepMerge = (dest, source) => {
  if (dest === undefined) {
    return source;
  } else if (Array.isArray(source)) {
    if (!(Array.isArray(dest))) {
      typeMismatch(dest, source);
      return dedup([ dest, ...source ]);
    }
    return [ ...dest, ...source ];
  } else if (typeof source === 'object') {
    if (!(typeof dest === 'object')) {
      typeMismatch(dest, source);
      return source;
    }
    let obj = Object.assign({}, dest);
    for (const [key, val] of Object.entries(source)) {
      obj[key] = deepMerge(obj[key], val);
    }
    return obj;
  } else {
    return source;
  }
};
function deepMerge(dest, ...sources) {
  return sources.reduce((p, source) => _deepMerge(p, source), dest);
}

// Config
//------------------------------------------------------------

const config = {
  "parser": "babel-eslint",

  "globals": {
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

  "extends": [
    "eslint:recommended",
  ],

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
  },
};

// React
// eslint-plugin-react
const react = {
  "globals": {
    "React": true,
    "ReactDOM": true,
  },
  "plugins": [
    "react",
  ],
  "rules": {
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
    "react/jsx-no-bind": 2, // Prevent binding or declaring arrow functions in render
    "react/jsx-no-target-blank": 2, // Prevent unsecure <a target="_blank"> links
  },
};

// Flux-standard-actions
// eslint-plugin-flux-standard-action
const fluxStandardActions = {
  "plugins": [ "flux-standard-actions" ],
  "extends": [
    "plugin:flux-standard-actions/recommended",
  ],
  "rules": {
    "flux-standard-actions/plain-object": 2,
    "flux-standard-actions/create-action-function": 0,
  },
};

module.exports = deepMerge(
  config,
  react,
  fluxStandardActions
);
