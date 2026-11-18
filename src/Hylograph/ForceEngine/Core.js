// Force Engine Core FFI
//
// What is left here is the browser: the animation frame and two kinds of
// drag. The forces themselves are PureScript (hylograph-forces-pure); there
// is no D3 in this file or anywhere else in the library.

// =============================================================================
// Animation Frame
// =============================================================================

// Use setTimeout instead of rAF to avoid browser throttling
// Set to true to use setTimeout instead of requestAnimationFrame
const USE_SETTIMEOUT = true;
const FRAME_TIME = 8; // Target ~60fps (accounting for ~8ms work time)

// Request animation frame - returns a cancel function
export function requestAnimationFrame_(callback) {
  return function() {
    let id;
    const handler = function(timestamp) {
      callback(timestamp || performance.now())();
    };

    if (USE_SETTIMEOUT) {
      id = setTimeout(handler, FRAME_TIME);
      return function() { clearTimeout(id); };
    } else {
      id = requestAnimationFrame(handler);
      return function() { cancelAnimationFrame(id); };
    }
  };
}

// =============================================================================
// Mutable Cancel Ref (for animation loop)
// =============================================================================

export function newCancelRef() {
  return { cancel: function() {} };
}

export function setCancelRef(ref) {
  return function(cancel) {
    return function() {
      ref.cancel = cancel;
    };
  };
}

export function getCancelRef(ref) {
  return function() {
    return ref.cancel;
  };
}

// =============================================================================
// Drag Behavior (native Pointer Events)
// =============================================================================

// Where a pointer event is, in the coordinates of an SVG element (through
// its screen transform, so viewBox, zoom and pan are all accounted for), or
// relative to the box of an HTML one.
function pointerIn(container, event) {
  const svg = container.ownerSVGElement || (container.tagName === 'svg' ? container : null);
  if (svg && container.getScreenCTM) {
    const ctm = container.getScreenCTM();
    if (ctm) {
      const point = svg.createSVGPoint();
      point.x = event.clientX;
      point.y = event.clientY;
      const local = point.matrixTransform(ctm.inverse());
      return [local.x, local.y];
    }
  }
  const box = container.getBoundingClientRect();
  return [event.clientX - box.left, event.clientY - box.top];
}

// One drag on one element. `subjectOf(event)` finds the simulation node,
// `frame` is the element whose coordinates the node lives in, and `release`
// decides what happens to the pin when the drag ends. The node keeps the
// offset from the pointer that it had when it was picked up.
function attachDrag(element, subjectOf, frame, reheat, release) {
  let subject = null;
  let offset = [0, 0];

  element.addEventListener('pointerdown', function(event) {
    if (event.button !== 0) return;
    subject = subjectOf(event);
    if (!subject) return;
    element.setPointerCapture(event.pointerId);
    event.preventDefault();
    reheat();
    const [x, y] = pointerIn(frame(), event);
    offset = [subject.x - x, subject.y - y];
    subject.fx = subject.x;
    subject.fy = subject.y;
    element.style.cursor = 'grabbing';
  });

  element.addEventListener('pointermove', function(event) {
    if (!subject) return;
    const [x, y] = pointerIn(frame(), event);
    subject.fx = x + offset[0];
    subject.fy = y + offset[1];
  });

  const end = function(event) {
    if (!subject) return;
    if (element.hasPointerCapture(event.pointerId)) element.releasePointerCapture(event.pointerId);
    release(subject);
    subject = null;
    element.style.cursor = 'grab';
  };
  element.addEventListener('pointerup', end);
  element.addEventListener('pointercancel', end);
  element.addEventListener('dragstart', function(event) { event.preventDefault(); });

  element.style.cursor = 'grab';
  element.style.touchAction = 'none';
}

// Drag transformed group elements (like bubble packs). The node is the datum
// of the nearest ancestor of what was pressed with the class `module-pack`;
// the pointer is read in the coordinates of the container the selector names.
export function attachGroupDragWithReheat_(elements) {
  return function(containerSelector) {
    return function(reheatCallback) {
      return function() {
        const container = document.querySelector(containerSelector);
        if (!container) {
          return;
        }
        const subjectOf = function(event) {
          let el = event.target;
          while (el && !el.classList?.contains('module-pack')) {
            el = el.parentElement;
          }
          return el?.__data__;
        };
        const release = function(subject) {
          subject.fx = null;
          subject.fy = null;
        };
        elements.forEach(function(el) {
          attachDrag(el, subjectOf, function() { return container; }, reheatCallback, release);
        });
      };
    };
  };
}

// Drag with toggle-pinning behavior: a node stays pinned where it is
// dropped; pressing a pinned node and letting go within 3px unpins it.
export function attachPinningDrag_(elements) {
  return function(reheatCallback) {
    return function() {
      const release = function(subject) {
        const dx = subject.fx - subject.x;
        const dy = subject.fy - subject.y;
        const distMoved = Math.sqrt(dx * dx + dy * dy);
        if (distMoved < 3 && subject._wasPinned) {
          subject.fx = null;
          subject.fy = null;
          delete subject._wasPinned;
        }
        if (subject.fx != null) {
          subject._wasPinned = true;
        }
      };
      elements.forEach(function(el) {
        attachDrag(el, function() { return el.__data__; }, function() { return el.parentElement || el; }, reheatCallback, release);
      });
    };
  };
}

// =============================================================================
// DOM Utilities
// =============================================================================

// Query elements by CSS selector
// Returns an array of Element (Web.DOM.Element compatible)
export function querySelectorElements_(selector) {
  return function() {
    const nodeList = document.querySelectorAll(selector);
    return Array.from(nodeList);
  };
}

// =============================================================================
// Debug Helpers
// =============================================================================

// Debug helper - only logs when explicitly called
export function logNodes_(label) {
  return function(nodes) {
    return function() {
      // No-op in production - call explicitly for debugging
    };
  };
}
