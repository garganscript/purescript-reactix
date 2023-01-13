'use strict';

import * as React from 'react';
import * as ReactDom from 'react-dom';
import { createRoot } from 'react-dom/client';

export let react = React;
export let reactDOM = ReactDom;
export let _createRoot = createRoot;

export function _render(element, root) {
  return root.render(element);
}
